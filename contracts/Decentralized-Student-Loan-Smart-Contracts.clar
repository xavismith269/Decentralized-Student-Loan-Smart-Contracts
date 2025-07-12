(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-EXISTS (err u101))
(define-constant ERR-NO-LOAN-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-LOAN-NOT-APPROVED (err u104))
(define-constant ERR-LOAN-ALREADY-APPROVED (err u105))
(define-constant ERR-LOAN-DEFAULTED (err u106))
(define-constant ERR-GRACE-PERIOD-EXISTS (err u107))
(define-constant ERR-NO-GRACE-PERIOD (err u108))
(define-constant ERR-INVALID-GRACE-PERIOD (err u109))

(define-data-var contract-owner principal tx-sender)
(define-data-var min-credit-score uint u650)
(define-data-var interest-rate uint u5)
(define-data-var max-grace-period-months uint u6)

(define-map StudentLoans
    principal
    {
        amount: uint,
        term-length: uint,
        monthly-payment: uint,
        credit-score: uint,
        approved: bool,
        defaulted: bool,
        total-paid: uint,
        last-payment: uint,
        grace-period-end: (optional uint),
        grace-periods-used: uint,
    }
)

(define-public (request-loan
        (amount uint)
        (term-months uint)
        (credit-score uint)
    )
    (let ((monthly-payment (calculate-monthly-payment amount term-months)))
        (asserts! (is-none (map-get? StudentLoans tx-sender)) ERR-LOAN-EXISTS)
        (asserts! (>= credit-score (var-get min-credit-score)) ERR-NOT-AUTHORIZED)
        (ok (map-set StudentLoans tx-sender {
            amount: amount,
            term-length: term-months,
            monthly-payment: monthly-payment,
            credit-score: credit-score,
            approved: false,
            defaulted: false,
            total-paid: u0,
            last-payment: stacks-block-height,
            grace-period-end: none,
            grace-periods-used: u0,
        }))
    )
)

(define-public (approve-loan (student principal))
    (let ((loan (unwrap! (map-get? StudentLoans student) ERR-NO-LOAN-EXISTS)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved loan)) ERR-LOAN-ALREADY-APPROVED)
        (try! (stx-transfer? (get amount loan) tx-sender student))
        (ok (map-set StudentLoans student (merge loan { approved: true })))
    )
)

(define-public (make-payment)
    (let (
            (loan (unwrap! (map-get? StudentLoans tx-sender) ERR-NO-LOAN-EXISTS))
            (payment (get monthly-payment loan))
        )
        (asserts! (get approved loan) ERR-LOAN-NOT-APPROVED)
        (asserts! (not (get defaulted loan)) ERR-LOAN-DEFAULTED)
        (try! (stx-transfer? payment tx-sender (var-get contract-owner)))
        (ok (map-set StudentLoans tx-sender
            (merge loan {
                total-paid: (+ (get total-paid loan) payment),
                last-payment: stacks-block-height,
                grace-period-end: none,
            })
        ))
    )
)

(define-read-only (get-loan-details (student principal))
    (ok (map-get? StudentLoans student))
)

(define-read-only (calculate-monthly-payment
        (amount uint)
        (term-months uint)
    )
    (let (
            (interest-amount (* amount (var-get interest-rate)))
            (total-amount (+ amount interest-amount))
        )
        (/ total-amount term-months)
    )
)

(define-public (mark-loan-defaulted (student principal))
    (let ((loan (unwrap! (map-get? StudentLoans student) ERR-NO-LOAN-EXISTS)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set StudentLoans student (merge loan { defaulted: true })))
    )
)

(define-public (update-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set interest-rate new-rate))
    )
)

(define-map GracePeriodRequests
    principal
    {
        months-requested: uint,
        reason: (string-ascii 100),
        request-block: uint,
        approved: bool,
    }
)

(define-public (request-grace-period
        (months uint)
        (reason (string-ascii 100))
    )
    (let (
            (loan (unwrap! (map-get? StudentLoans tx-sender) ERR-NO-LOAN-EXISTS))
            (current-grace-periods (get grace-periods-used loan))
        )
        (asserts! (get approved loan) ERR-LOAN-NOT-APPROVED)
        (asserts! (not (get defaulted loan)) ERR-LOAN-DEFAULTED)
        (asserts! (is-none (get grace-period-end loan)) ERR-GRACE-PERIOD-EXISTS)
        (asserts! (is-none (map-get? GracePeriodRequests tx-sender))
            ERR-GRACE-PERIOD-EXISTS
        )
        (asserts!
            (and (> months u0) (<= months (var-get max-grace-period-months)))
            ERR-INVALID-GRACE-PERIOD
        )
        (asserts! (< current-grace-periods u3) ERR-INVALID-GRACE-PERIOD)
        (ok (map-set GracePeriodRequests tx-sender {
            months-requested: months,
            reason: reason,
            request-block: stacks-block-height,
            approved: false,
        }))
    )
)

(define-public (approve-grace-period (student principal))
    (let (
            (loan (unwrap! (map-get? StudentLoans student) ERR-NO-LOAN-EXISTS))
            (request (unwrap! (map-get? GracePeriodRequests student) ERR-NO-GRACE-PERIOD))
            (grace-end-block (+ stacks-block-height (* (get months-requested request) u4320)))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved request)) ERR-GRACE-PERIOD-EXISTS)
        (map-delete GracePeriodRequests student)
        (ok (map-set StudentLoans student
            (merge loan {
                grace-period-end: (some grace-end-block),
                grace-periods-used: (+ (get grace-periods-used loan) u1),
            })
        ))
    )
)

(define-public (deny-grace-period (student principal))
    (let ((request (unwrap! (map-get? GracePeriodRequests student) ERR-NO-GRACE-PERIOD)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved request)) ERR-GRACE-PERIOD-EXISTS)
        (ok (map-delete GracePeriodRequests student))
    )
)

(define-read-only (is-in-grace-period (student principal))
    (let ((loan (map-get? StudentLoans student)))
        (match loan
            loan-data (match (get grace-period-end loan-data)
                grace-end (ok (>= grace-end stacks-block-height))
                (ok false)
            )
            (ok false)
        )
    )
)

(define-read-only (get-grace-period-request (student principal))
    (ok (map-get? GracePeriodRequests student))
)
