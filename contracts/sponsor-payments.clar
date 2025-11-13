(define-map sponsor-balances
  {
    student: principal,
    sponsor: principal,
  }
  { available: uint }
)

(define-map student-totals
  { student: principal }
  { total: uint }
)

(define-read-only (get-sponsor-available
    (student principal)
    (sponsor principal)
  )
  (default-to u0
    (get available
      (map-get? sponsor-balances {
        student: student,
        sponsor: sponsor,
      })
    ))
)

(define-read-only (get-student-total (student principal))
  (default-to u0 (get total (map-get? student-totals { student: student })))
)

(define-public (sponsor-deposit
    (student principal)
    (amount uint)
  )
  (begin
    (asserts! (> amount u0) (err u100))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let (
        (current (default-to u0
          (get available
            (map-get? sponsor-balances {
              student: student,
              sponsor: tx-sender,
            })
          )))
        (new (+ current amount))
        (total-current (default-to u0 (get total (map-get? student-totals { student: student }))))
        (new-total (+ total-current amount))
      )
      (map-set sponsor-balances {
        student: student,
        sponsor: tx-sender,
      } { available: new }
      )
      (map-set student-totals { student: student } { total: new-total })
      (ok new)
    )
  )
)

(define-public (sponsor-refund
    (student principal)
    (amount uint)
  )
  (begin
    (asserts! (> amount u0) (err u101))
    (let ((current (default-to u0
        (get available
          (map-get? sponsor-balances {
            student: student,
            sponsor: tx-sender,
          })
        ))))
      (asserts! (>= current amount) (err u102))
      (let (
          (new (- current amount))
          (total-current (default-to u0
            (get total (map-get? student-totals { student: student }))
          ))
          (new-total (- total-current amount))
        )
        (map-set sponsor-balances {
          student: student,
          sponsor: tx-sender,
        } { available: new }
        )
        (map-set student-totals { student: student } { total: new-total })
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (ok amount)
      )
    )
  )
)

(define-public (student-withdraw-from-sponsor
    (sponsor principal)
    (amount uint)
  )
  (begin
    (asserts! (> amount u0) (err u103))
    (let ((current (default-to u0
        (get available
          (map-get? sponsor-balances {
            student: tx-sender,
            sponsor: sponsor,
          })
        ))))
      (asserts! (>= current amount) (err u104))
      (let (
          (new (- current amount))
          (total-current (default-to u0
            (get total (map-get? student-totals { student: tx-sender }))
          ))
          (new-total (- total-current amount))
        )
        (map-set sponsor-balances {
          student: tx-sender,
          sponsor: sponsor,
        } { available: new }
        )
        (map-set student-totals { student: tx-sender } { total: new-total })
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (ok amount)
      )
    )
  )
)
