(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-EXISTS (err u101))
(define-constant ERR-NO-LOAN-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-LOAN-NOT-APPROVED (err u104))
(define-constant ERR-LOAN-ALREADY-APPROVED (err u105))
(define-constant ERR-LOAN-DEFAULTED (err u106))

(define-data-var contract-owner principal tx-sender)
(define-data-var min-credit-score uint u650)
(define-data-var interest-rate uint u5)

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
