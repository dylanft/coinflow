;;;;;;;;;;;;;;;;;;;;; SIP 010 ;;;;;;;;;;;;;;;;;;;;;;
(impl-trait .sip-010-trait-ft-standard.sip-010-trait)



;; Defines a demo XBTC token according to the SIP-010 Standard
(define-fungible-token xbtc)

(define-data-var token-uri (string-utf8 256) u"")

(define-constant CONTRACT-OWNER 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-constant ERR-UNAUTHORIZED-MINT (err u100))

;; errors
(define-constant ERR-NOT-AUTHORIZED u14401)

;; ---------------------------------------------------------
;; SIP-10 Functions
;; ---------------------------------------------------------

(define-read-only (get-total-supply)
  (ok (ft-get-supply xbtc))
)

(define-read-only (get-name)
  (ok "XBTC")
)

(define-read-only (get-symbol)
  (ok "XBTC")
)

(define-read-only (get-decimals)
  (ok u0)
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance xbtc account))
)

(define-read-only (get-token-uri)
  (ok (some (var-get token-uri)))
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err ERR-NOT-AUTHORIZED))

    (match (ft-transfer? xbtc amount sender recipient)
      response (begin
        (print memo)
        (ok response)
      )
      error (err error)
    )
  )
)




(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED-MINT)
    (ft-mint? xbtc amount recipient)
  )
)

(define-public (burn (amount uint) (burner principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED-MINT)
    (ft-burn? xbtc amount burner)
  )
)

;; (define-public (mint-many-demo (amount uint))
;;   (begin
;;     (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED-MINT)
;;     (try! (ft-mint? usda amount tx-sender))
;;     (try! (ft-mint? usda amount 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5))
;;     (try! (ft-mint? usda amount 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG))
;;     (try! (ft-mint? usda amount 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC))
;;     (try! (ft-mint? usda amount 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND))
;;     (try! (ft-mint? usda amount 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB))
;;     (try! (ft-mint? usda amount 'ST3AM1A56AK2C1XAFJ4115ZSV26EB49BVQ10MGCS0))
;;     (try! (ft-mint? usda amount 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP))
;;     (try! (ft-mint? usda amount 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ))
;;     (try! (ft-mint? usda amount 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6))
;;   )
;; )



(begin
  ;; (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED-MINT)
  (try! (ft-mint? xbtc u100 tx-sender))
  (try! (ft-mint? xbtc u100 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5))
  (try! (ft-mint? xbtc u100 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG))
  (try! (ft-mint? xbtc u100 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC))
  (try! (ft-mint? xbtc u100 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND))
  (try! (ft-mint? xbtc u100 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB))
  (try! (ft-mint? xbtc u100 'ST3AM1A56AK2C1XAFJ4115ZSV26EB49BVQ10MGCS0))
  (try! (ft-mint? xbtc u100 'ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP))
  (try! (ft-mint? xbtc u100 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ))
  (try! (ft-mint? xbtc u100 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6))
)
