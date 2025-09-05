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
(define-constant ERR-REFINANCING-REQUEST-EXISTS (err u110))
(define-constant ERR-NO-REFINANCING-REQUEST (err u111))
(define-constant ERR-INVALID-REFINANCING-TERMS (err u112))

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

(define-map RefinancingRequests
    principal
    {
        new-interest-rate: uint,
        new-term-months: uint,
        new-credit-score: uint,
        justification: (string-ascii 200),
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

(define-public (request-refinancing
        (new-interest-rate uint)
        (new-term-months uint)
        (new-credit-score uint)
        (justification (string-ascii 200))
    )
    (let (
            (loan (unwrap! (map-get? StudentLoans tx-sender) ERR-NO-LOAN-EXISTS))
            (current-monthly-payment (get monthly-payment loan))
            (remaining-balance (- (get amount loan) (get total-paid loan)))
            (new-monthly-payment (calculate-monthly-payment-with-rate remaining-balance
                new-term-months new-interest-rate
            ))
        )
        (asserts! (get approved loan) ERR-LOAN-NOT-APPROVED)
        (asserts! (not (get defaulted loan)) ERR-LOAN-DEFAULTED)
        (asserts! (is-none (map-get? RefinancingRequests tx-sender))
            ERR-REFINANCING-REQUEST-EXISTS
        )
        (asserts! (>= new-credit-score (get credit-score loan))
            ERR-INVALID-REFINANCING-TERMS
        )
        (asserts! (< new-monthly-payment current-monthly-payment)
            ERR-INVALID-REFINANCING-TERMS
        )
        (asserts! (> new-term-months u0) ERR-INVALID-REFINANCING-TERMS)
        (ok (map-set RefinancingRequests tx-sender {
            new-interest-rate: new-interest-rate,
            new-term-months: new-term-months,
            new-credit-score: new-credit-score,
            justification: justification,
            request-block: stacks-block-height,
            approved: false,
        }))
    )
)

(define-public (approve-refinancing (student principal))
    (let (
            (loan (unwrap! (map-get? StudentLoans student) ERR-NO-LOAN-EXISTS))
            (request (unwrap! (map-get? RefinancingRequests student)
                ERR-NO-REFINANCING-REQUEST
            ))
            (remaining-balance (- (get amount loan) (get total-paid loan)))
            (new-monthly-payment (calculate-monthly-payment-with-rate remaining-balance
                (get new-term-months request)
                (get new-interest-rate request)
            ))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved request)) ERR-REFINANCING-REQUEST-EXISTS)
        (map-delete RefinancingRequests student)
        (ok (map-set StudentLoans student
            (merge loan {
                term-length: (get new-term-months request),
                monthly-payment: new-monthly-payment,
                credit-score: (get new-credit-score request),
                amount: remaining-balance,
                total-paid: u0,
            })
        ))
    )
)

(define-public (deny-refinancing (student principal))
    (let ((request (unwrap! (map-get? RefinancingRequests student)
            ERR-NO-REFINANCING-REQUEST
        )))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved request)) ERR-REFINANCING-REQUEST-EXISTS)
        (ok (map-delete RefinancingRequests student))
    )
)

(define-read-only (calculate-monthly-payment-with-rate
        (amount uint)
        (term-months uint)
        (rate uint)
    )
    (let (
            (interest-amount (* amount rate))
            (total-amount (+ amount interest-amount))
        )
        (/ total-amount term-months)
    )
)

(define-read-only (get-refinancing-request (student principal))
    (ok (map-get? RefinancingRequests student))
)

(define-read-only (calculate-refinancing-savings (student principal))
    (let (
            (loan (map-get? StudentLoans student))
            (request (map-get? RefinancingRequests student))
        )
        (match loan
            loan-data (match request
                request-data (let (
                        (current-payment (get monthly-payment loan-data))
                        (remaining-balance (- (get amount loan-data) (get total-paid loan-data)))
                        (new-payment (calculate-monthly-payment-with-rate remaining-balance
                            (get new-term-months request-data)
                            (get new-interest-rate request-data)
                        ))
                        (monthly-savings (- current-payment new-payment))
                        (total-savings (* monthly-savings (get new-term-months request-data)))
                    )
                    (ok {
                        monthly-savings: monthly-savings,
                        total-savings: total-savings,
                        new-monthly-payment: new-payment,
                    })
                )
                (err ERR-NO-REFINANCING-REQUEST)
            )
            (err ERR-NO-LOAN-EXISTS)
        )
    )
)
