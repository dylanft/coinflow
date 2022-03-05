;; coinflow
;; Proof of Concept for putting collateral to work for a short term loan.

;;(impl-trait 'SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR.usda-token)
;;(try! (contract-call? 'SP466FNC0P7JWTNM2R9T199QRZN1MYEDTAR0KP27.miamicoin-token transfer total-artist tx-sender (var-get artist-address) (some 0x00)))
;; (use-trait .usda-token)

;; error constants
(define-constant err-negative-loan-amount (err u100))
(define-constant err-not-enough-days (err u101))
(define-constant err-no-interest (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-past-loan-init-deadline (err u104))
(define-constant err-sending-to-wrong-LP (err u105))
(define-constant err-no-existing-loan-offer-from-LP (err u106))
(define-constant err-not-enough-juice (err u107))
(define-constant err-with-claim-loan-DNE (err u108))
(define-constant err-borrower-has-more-time (err u109))
(define-constant ERR-UNEQUAL-COLLATERAL-REPAYMENT (err u110))
(define-constant ERR-LENDER-MUST-RETURN-COLLATERAL (err u111)) 
(define-constant ERR-FALSE-CLAIM-ON-BORROWER-COLLATERAL (err u112))
(define-constant ERR-LP-HAS-MORE-TIME-TO-RETURN-COLLATERAL (err u113))
(define-constant ERR-LP-ALREADY-RETURNED-COLLATERAL (err u114))
(define-constant ERR-LOAN-AMT-INCORRECT (err u115))

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
(define-data-var coinflowWallet principal 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow)


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
		(asserts! (> loanAmount u0) err-negative-loan-amount)
		(asserts! (>= loanAmount u100) err-not-enough-juice)
		(asserts! (>= blocksToRepay u144) err-not-enough-days)
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
		(asserts! (>= (unwrap! (get-loan-interest-from borrower) err-no-interest) u0) err-insufficient-funds)
        ;; can probably remove this function and have the transfer amount gathered based on the get-loan-interest-from fcn
        (asserts! (is-eq loanAmount (unwrap! (get-loan-interest-from borrower) err-no-interest)) ERR-LOAN-AMT-INCORRECT)
		(map-set initDeadline borrower (+ block-height blocksToInitialize))
		(map-set loanSource borrower tx-sender)
		(map-set debtSource tx-sender borrower)
		;; (try! (stx-transfer? (* u5 loanAmount) tx-sender (var-get coinflowWallet)))
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer (* u5 loanAmount) tx-sender (var-get coinflowWallet) none))
        (set-lp-collateral-amount (* u4 loanAmount) borrower tx-sender)
		(ok true)
	)
)

(define-public (send-collateral-to-LP (amount uint) (lp principal) (memo (optional (buff 34))))
	(begin 
        (print (unwrap! (get-loan-interest-from tx-sender) err-no-interest))

		;;checks to see if the borrower has declared interest in getting a loan. otherwise, txn fails.
		(asserts! (>= (unwrap! (get-loan-interest-from tx-sender) err-no-interest) u0) err-insufficient-funds)
		
		;; asserts that LP input is same as wallet that locked up the loan amount and LP collateral.
		(asserts! (is-eq lp (unwrap! (get-loan-source-from tx-sender) err-no-existing-loan-offer-from-LP)) err-sending-to-wrong-LP)

        ;; amount represents XBTC being sent by borrower
		;; ensure OVER collateralizing with 2x usda loan amount worth of XBTC. oracle implementation needed for this and to handle liquidation scenarios. 
		(asserts! (>= (/ (* usda-to-xbtc-ratio amount) u2) (unwrap! (get-loan-interest-from tx-sender) err-no-interest)) err-insufficient-funds)
		 ;;(print lp)
		
		;; ensures its not too late to initialize the loan
		(asserts! (< block-height (unwrap! (get-loan-init-deadline tx-sender) err-no-interest)) err-past-loan-init-deadline)
		
		;; Borrower sends collateral for their loan amount to the LP
        (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wrapped-bitcoin transfer amount tx-sender lp memo))

		;; Contract sends loan amount to the borrower (constraints on LP collateral amount enforced already)
		;; TODO: where does the discounted coinflow / fee / interest rate come into play?
		;; for now lets say smart contract sends LP 2 pct of loanAmount
		;; TODO: don't take a percent on the amount of collateral sent by borrower. take a % based on loan amount
		;; (try! (stx-transfer? (/ amount u100) (as-contract (var-get coinflowWallet)) lp))
		;; (try! (stx-transfer? (- amount (/ amount u100)) (as-contract (var-get coinflowWallet)) tx-sender))
		;; (as-contract (try! (stx-transfer? (/ amount u100) (var-get coinflowWallet) lp)))
		;; (as-contract (try! (stx-transfer? (- amount (/ amount u100)) (var-get coinflowWallet) tx-sender)))

        (try! (initialize-loan (unwrap! (get-loan-interest-from tx-sender) err-no-interest) tx-sender lp memo))
        ;; (try! ((initialize-loan (unwrap! (get-loan-interest-from tx-sender) err-no-interest) tx-sender lp memo)))
		;; (try! (initialize-loan (/ amount u2) tx-sender lp))
		
        
        ;; Establish the repayment deadline 
		;; TODO: update from hardcoded value of ~30 days = 4320 blocks
		;; (map-set repayDeadline tx-sender (+ block-height ((unwrap! (get-loan-length-from tx-sender) err-no-interest))))
		;; (map-set repayDeadline tx-sender (+ block-height u4320))
		(map-set repayDeadline tx-sender (+ block-height (unwrap! (get-loan-length-from tx-sender) err-no-interest)))
        
        ;; Initialize the loan and store info in map
        (map-set loans
            {borrower: tx-sender, LP: lp}
            {
                originalLoanAmount: (unwrap! (get-loan-interest-from tx-sender) err-no-interest), ;; in USDA
                repaymentBlockDeadline: (+ block-height (unwrap! (get-loan-length-from tx-sender) err-no-interest)),
                borrowerCollateralAmt: amount, ;;in  XBTC
                ;; lpCollateralAmt: (* u4 (unwrap! (get-loan-interest-from tx-sender) err-no-interest))
                lpCollateralAmt: (unwrap! (get-lp-collateral-amount tx-sender lp) err-no-existing-loan-offer-from-LP) ;; in USDA
            }
        )

        (map-set loanBalances
            {borrower: tx-sender, LP: lp}
            (/ amount u2)
            
        )
		(ok true)
		;; TODO: add a new loan event to some dictionary like data store in the smart contract.

	)
)

(define-private (initialize-loan (loanAmount uint) (borrower principal) (lender principal) (memo (optional (buff 34))))
	(begin 
   		(try! (as-contract (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.usda-token transfer loanAmount tx-sender lender memo)))

		;; (try! (as-contract (stx-transfer? (/ loanAmount u100) tx-sender lender)))
		;; (try! (as-contract (stx-transfer? (- loanAmount (/ loanAmount u100)) tx-sender borrower)))
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
        (asserts! (> (get-loan-balance borrower lender) u0) err-with-claim-loan-DNE)
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
        (asserts! (> (get-loan-balance borrower tx-sender) u0) err-with-claim-loan-DNE)
        (asserts! (> block-height (unwrap-panic (map-get? repayDeadline borrower))) err-borrower-has-more-time)
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
