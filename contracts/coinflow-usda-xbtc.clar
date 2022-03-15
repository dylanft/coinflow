;; coinflow
;; Proof of Concept for putting collateral to work for a short term loan.

;; error constants
(define-constant ERR-NEGATIVE-LOAN-AMOUNT (err u100))
(define-constant ERR-NOT-ENOUGH-DAYS (err u101))
(define-constant ERR-NO-INTEREST (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-PAST-LOAN-INIT-DEADLINE (err u104))
(define-constant ERR-SENDING-TO-WRONG-LP (err u105))
(define-constant ERR-NO-EXISTING-LOAN-OFFER-FROM-LP (err u106))
(define-constant ERR-NOT-ENOUGH-JUICE (err u107))
(define-constant ERR-WITH-CLAIM-LOAN-DNE (err u108))
(define-constant ERR-BORROWER-HAS-MORE-TIME (err u109))
(define-constant ERR-UNEQUAL-COLLATERAL-REPAYMENT (err u110))
(define-constant ERR-LENDER-MUST-RETURN-COLLATERAL (err u111)) 
(define-constant ERR-FALSE-CLAIM-ON-BORROWER-COLLATERAL (err u112))
(define-constant ERR-LP-HAS-MORE-TIME-TO-RETURN-COLLATERAL (err u113))
(define-constant ERR-LP-ALREADY-RETURNED-COLLATERAL (err u114))
(define-constant ERR-LOAN-AMT-INCORRECT (err u115))
(define-constant UNAUTHORIZED-MVMT-BORROWER-COLLATERAL (err u116))


(define-constant ERR-NO-INTEREST2 (err u202))
(define-constant ERR-NO-INTEREST3 (err u203))
(define-constant ERR-NO-INTEREST4 (err u204))
(define-constant ERR-NO-INTEREST5 (err u205))
(define-constant ERR-NO-INTEREST6 (err u206))
(define-constant ERR-NO-INTEREST7 (err u207))
(define-constant ERR-NO-INTEREST8 (err u208))
(define-constant ERR-NO-INTEREST9 (err u209))


;;

;; other constants
(define-constant usda-to-xbtc-ratio u50000)
(define-constant LP-COLLATERAL-REPAYMENT-BUFFER u100)

;; maps
(define-map loanInterest principal uint)
(define-map loanLength principal uint)
(define-map loanSource principal principal) ;; maps a borrower to a lender
(define-map debtSource principal principal) ;; maps lender to a borrower
(define-map initDeadline principal uint) ;; borrower must send collateral to LP by initDeadline. principal = LP.
(define-map repayDeadline principal uint)
(define-map loans
    {borrower: principal, LP: principal}
    {
        originalLoanAmount: uint,
        repaymentBlockDeadline: uint,
        borrowerCollateralAmt: uint,
        lpCollateralAmt: uint
    }
)
(define-map loanBalances
    {borrower: principal, LP: principal}
    uint
)

(define-map lpCollateralAmount 
        {borrower: principal, lender: principal}
        uint
)
(define-map hasLenderReturnedCollateral
    {borrower: principal, lender: principal}
    bool
)


;; variables
;; Define coinflow wallet
(define-data-var coinflowWallet principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc)


;; functions
(define-read-only (get-loan-interest-from (wallet principal))
	(map-get? loanInterest wallet)
)

(define-read-only (get-loan-source-from (wallet principal))
	(map-get? loanSource wallet)
)

(define-read-only (get-loan-length-from (wallet principal))
	(map-get? loanLength wallet)
)

(define-read-only (get-loan-init-deadline (wallet principal))
	(map-get? initDeadline wallet)
)

(define-read-only (get-loan-repayment-deadline (wallet principal))
	(default-to u0 (map-get? repayDeadline wallet))
)

(define-read-only (get-loan-health (borrower principal) (lender principal))
    (map-get? loans {borrower: borrower, LP: lender})
)

(define-read-only (get-loan-balance (borrower principal) (lender principal))
    (default-to u0 (map-get? loanBalances {borrower: borrower, LP: lender}))
)

(define-read-only (get-lp-collateral-amount (borrower principal) (lender principal))
	(map-get? lpCollateralAmount 
        {borrower: borrower, lender: lender}
    )
)

(define-read-only (get-status-of-LP-collateral-return (borrower principal) (lender principal))
    (default-to false (map-get? hasLenderReturnedCollateral {borrower: borrower, lender: lender}))
)

(define-private (set-status-of-LP-collateral-return (borrower principal) (lender principal) (status bool))
    (map-set hasLenderReturnedCollateral 
        {borrower: borrower, lender: lender}
        status
    )
)

(define-private (set-loan-source (borrower principal) (lender principal))
	(map-set loanSource borrower lender)
)

(define-private (set-lp-collateral-amount (amount uint) (borrower principal) (lender principal))
	(map-set lpCollateralAmount 
        {borrower: borrower, lender: lender}
        amount
    )
)

(define-private (set-debt-source (borrower principal) (lender principal))
	(map-set debtSource lender borrower)
)


;; TODO: borrower must also select a max acceptable interest rate over the repayment terms they want.
(define-public (signal-interest (loanAmount uint) (blocksToRepay uint))
	(begin
		(asserts! (> loanAmount u0) ERR-NEGATIVE-LOAN-AMOUNT)
		(asserts! (>= loanAmount u100) ERR-NOT-ENOUGH-JUICE)
		(asserts! (>= blocksToRepay u144) ERR-NOT-ENOUGH-DAYS)
		(print loanAmount)
		(print blocksToRepay)
		(print tx-sender)
		(map-set loanInterest tx-sender loanAmount)
		(map-set loanLength tx-sender blocksToRepay)
		(ok true)
	)
)

(define-public (lock-up-LP-collateral (borrower principal) (blocksToInitialize uint) (loanAmount uint))
	;; TODO: assert that there is interest from the borrower
	;; TODO: lookup loanAmount and ensure LP has enough collateral to lock up and to offer the loan
	(begin 
		(asserts! (>= (unwrap! (get-loan-interest-from borrower) ERR-NO-INTEREST) u0) ERR-INSUFFICIENT-FUNDS)
        ;; can probably remove this function and have the transfer amount gathered based on the get-loan-interest-from fcn
        (asserts! (is-eq loanAmount (unwrap! (get-loan-interest-from borrower) ERR-NO-INTEREST)) ERR-LOAN-AMT-INCORRECT)
		(map-set initDeadline borrower (+ block-height blocksToInitialize))
		(map-set loanSource borrower tx-sender)
		(map-set debtSource tx-sender borrower)
		;; (try! (stx-transfer? (* u5 loanAmount) tx-sender (var-get coinflowWallet)))
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer (* u5 loanAmount) tx-sender (var-get coinflowWallet) none))
        (set-lp-collateral-amount (* u4 loanAmount) borrower tx-sender)
		(ok true)
	)
)

(define-public (initialize-loan (usdaAmount uint) (xbtcAmount uint) (borrower principal) (lender principal) (memo (optional (buff 34))))
    (begin
        ;;checks to see if the borrower has declared interest in getting a loan. otherwise, txn fails.
        (asserts! (>= usdaAmount u0) ERR-INSUFFICIENT-FUNDS)

		;; asserts that LP input is same as wallet that locked up the loan amount and LP collateral.
		(asserts! (is-eq lender (unwrap! (get-loan-source-from tx-sender) ERR-NO-EXISTING-LOAN-OFFER-FROM-LP)) ERR-SENDING-TO-WRONG-LP)

		;; ensure OVER collateralizing with 2x usda loan amount worth of XBTC. oracle implementation needed for this and to handle liquidation scenarios. 
		(asserts! (>= (/ (* usda-to-xbtc-ratio xbtcAmount) u2) usdaAmount) (err u210))

		;; ensures its not too late to initialize the loan
		(asserts! (< block-height (unwrap! (get-loan-init-deadline tx-sender) ERR-NO-INTEREST4)) ERR-PAST-LOAN-INIT-DEADLINE)

		;; Borrower sends collateral for their loan amount to the LP
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wrapped-bitcoin transfer xbtcAmount tx-sender lender memo))

		;; Borrower claims / is sent the usda loan amount
        (try! (as-contract (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer usdaAmount tx-sender borrower none)))

        (map-set repayDeadline tx-sender (+ block-height (unwrap! (get-loan-length-from tx-sender) ERR-NO-INTEREST6)))
        
        ;; Initialize the loan and store info in map
        (map-set loans
            {borrower: tx-sender, LP: lender}
            {
                originalLoanAmount: (unwrap! (get-loan-interest-from tx-sender) ERR-NO-INTEREST7), ;; in USDA
                repaymentBlockDeadline: (+ block-height (unwrap! (get-loan-length-from tx-sender) ERR-NO-INTEREST8)),
                borrowerCollateralAmt: xbtcAmount, ;;in  XBTC
                ;; lpCollateralAmt: (* u4 (unwrap! (get-loan-interest-from tx-sender) ERR-NO-INTEREST))
                lpCollateralAmt: (unwrap! (get-lp-collateral-amount tx-sender lender) ERR-NO-EXISTING-LOAN-OFFER-FROM-LP) ;; in USDA
            }
        )

        (map-set loanBalances
            {borrower: tx-sender, LP: lender}
            usdaAmount
            
        )

        (set-status-of-LP-collateral-return borrower lender false)

        
        (ok true)
    )
)

;;TODO: assert that loan balance exists and is greater than zero !!!!!!!!
;; assumptions:
;;     anyone can pay back anyone's loan if they want to.
;;     transfer amount doesn't exceed remaining balance
;; need to delete the loan from the map if its paid off entirely 
(define-public (repay-loan (amount uint) (borrower principal) (lender principal) (memo (optional (buff 34))))
    (begin
        (asserts! (> (get-loan-balance borrower lender) u0) ERR-WITH-CLAIM-LOAN-DNE)
        ;; (try! (stx-transfer? amount tx-sender lender))
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer amount tx-sender lender memo))
        ;; (try! (stx-transfer? amount tx-sender lender))
        (map-set loanBalances
            {borrower: borrower, LP: lender}
            (- (get-loan-balance borrower lender) amount)
        ) 
        (ok true)
    )
)

;; Scenario 2: Borrower fails to repay loan IN FULL by repayment deadline.
;; assumptions: 
;;      if don't payback in full, cannot claim any of borrower LPs. in practice, this should be prorated to some extent if borrower pays back 99% for example.
;;      who is able to call this txn and when? need to be sure!
(define-public (claim-lp-collateral (borrower principal) (lender principal))
    (begin
        (asserts! (> (get-loan-balance borrower tx-sender) u0) ERR-WITH-CLAIM-LOAN-DNE)
        (asserts! (> block-height (unwrap-panic (map-get? repayDeadline borrower))) ERR-BORROWER-HAS-MORE-TIME)
		;; (try! (as-contract (stx-transfer? (unwrap-panic (get-lp-collateral-amount borrower lender)) tx-sender lender)))
		(try! (as-contract (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer (unwrap-panic (get-lp-collateral-amount borrower lender)) tx-sender lender none)))

        (ok true)
    )
)

;;TODO: This function will be updated to check to see if the native BTC txn has occurred based on additional input
;; collateral returned could be a variable that gets updated based on txn checks
(define-public (return-collateral-to-borrower (amount uint) (borrower principal) (lender principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq amount (get borrowerCollateralAmt (unwrap-panic (get-loan-health borrower lender)))) ERR-UNEQUAL-COLLATERAL-REPAYMENT)
        (asserts! (is-eq tx-sender lender) ERR-LENDER-MUST-RETURN-COLLATERAL)
        ;; (try! (as-contract (stx-transfer? amount tx-sender borrower)))
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wrapped-bitcoin transfer amount lender borrower memo))
        (set-status-of-LP-collateral-return borrower lender true)
        (ok true)
    )
)

(define-public (claim-borrower-collateral (amount uint) (borrower principal) (lender principal))
    (begin
        (asserts! (is-eq tx-sender borrower) ERR-FALSE-CLAIM-ON-BORROWER-COLLATERAL)
        (asserts! (is-eq amount (get borrowerCollateralAmt (unwrap-panic (get-loan-health borrower lender)))) ERR-UNEQUAL-COLLATERAL-REPAYMENT)
        ;; (asserts! (>= (+ block-height LP-COLLATERAL-REPAYMENT-BUFFER) (get repaymentBlockDeadline (unwrap-panic (get-loan-health borrower lender)))) ERR-LP-HAS-MORE-TIME-TO-RETURN-COLLATERAL)

        ;; Ensures borrower can ONLY claim the LPs collateral if the LP has NOT returned the borrowers collateral by the repayment deadline + some buffer of blocks
        (asserts! (is-eq false (get-status-of-LP-collateral-return borrower lender)) ERR-LP-ALREADY-RETURNED-COLLATERAL)
        (asserts! (>= block-height (+ LP-COLLATERAL-REPAYMENT-BUFFER (get repaymentBlockDeadline (unwrap-panic (get-loan-health borrower lender))))) ERR-LP-HAS-MORE-TIME-TO-RETURN-COLLATERAL)
        
        ;; (try! (as-contract (stx-transfer? amount tx-sender borrower)))
        (try! (as-contract (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer (get borrowerCollateralAmt (unwrap-panic (get-loan-health borrower lender))) tx-sender borrower none)))

        (ok true)
    )
)


;;TODO: create a function for the borrower to claim the LP collateral IFF borrower repays in full AND LP doesn't return collateral

;; TODO: create a dictionary like data-var to store all loans numbered 1 through n and implement in appropriate functions
;; aybe appropriate functions is just send-STX-collateral-to-LP?
;; then reset the variables associated with the wallets after loan initialized?
;; or better to reset some after LP collateral locked up and rest after borrowers collateral sent to LP


;; TODO: Write a function for LP to claim the LP collateral
;; must check if loan initialized or not
;; must check appropriate deadlineese depending on initialized status

;; TODO: introduce interest rate in a way where borrower can choose from best LP option
;; where each LP sets their own time limit on the offer in num of blocks
;; and wher esupplier sets their own terms






;; Clarinet testing:
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow signal-interest u1000 u145 'STFCVYY1RJDNJHST7RRTPACYHVJQDJ7R1DWTQHQA)
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow get-loan-interest-from tx-sender)
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow send-collateral-to-LP u500 'ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK)

;; More testing
;; ::set_tx_sender ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow signal-interest u1000 u145)
;; ::advance_chain_tip 1
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow get-loan-interest-from tx-sender)


;; ::set_tx_sender STFCVYY1RJDNJHST7RRTPACYHVJQDJ7R1DWTQHQA
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow lock-up-LP-collateral 'ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK u288 u1000)
;; ::get_assets_maps


;; ;; TODO: fix as contract in the send STX function
;; ::advance_chain_tip 1
;; ::set_tx_sender ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow send-collateral-to-LP u2000 'STFCVYY1RJDNJHST7RRTPACYHVJQDJ7R1DWTQHQA)







