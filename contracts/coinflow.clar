
;; coinflow
;; Proof of Concept for putting collateral to work for a short term loan.

;; error constants
(define-constant err-negative-loan-amount (err u100))
(define-constant err-not-enough-days (err u101))
(define-constant err-no-interest (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-past-loan-init-deadline (err u104))
(define-constant err-sending-to-wrong-LP (err u105))
(define-constant err-no-existing-loan-offer-from-LP (err u106))
(define-constant err-not-enough-juice (err u107))

;;

;; other constants
(define-constant stx-to-usd-ratio u3)

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
	(map-get? repayDeadline wallet)
)

(define-read-only (get-loan-health (borrower principal) (lender principal))
    (map-get? loans {borrower: borrower, LP: lender})
)

(define-read-only (get-loan-balance (borrower principal) (lender principal))
    (default-to u0 (map-get? loanBalances {borrower: borrower, LP: lender}))
)

(define-private (set-loan-source (borrower principal) (lender principal))
	(map-set loanSource borrower lender)
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
		(map-set initDeadline borrower (+ block-height blocksToInitialize))
		(map-set loanSource borrower tx-sender)
		(map-set debtSource tx-sender borrower)
		(try! (stx-transfer? (* u5 loanAmount) tx-sender (var-get coinflowWallet)))
		(ok true)
	)
)

(define-public (send-STX-collateral-to-LP (amount uint) (lp principal))
	(begin
		(print (unwrap! (get-loan-interest-from tx-sender) err-no-interest))

		;;checks to see if the borrower has declared interest in getting a loan. otherwise, txn fails.
		(asserts! (>= (unwrap! (get-loan-interest-from tx-sender) err-no-interest) u0) err-insufficient-funds)
		
		;; asserts that LP input is same as wallet that locked up the loan amount and LP collateral.
		(asserts! (is-eq lp (unwrap! (get-loan-source-from tx-sender) err-no-existing-loan-offer-from-LP)) err-sending-to-wrong-LP)

		;; ensure OVER collateralizing with 2x loan amount. 
		;; NOT assuming loan amt in USDA yet b/c would need to implement an oracle
		(asserts! (>= (/ amount u2) (unwrap! (get-loan-interest-from tx-sender) err-no-interest)) err-insufficient-funds)
		 ;;(print lp)
		
		;; ensures its not too late to initialize the loan
		(asserts! (< block-height (unwrap! (get-loan-init-deadline tx-sender) err-no-interest)) err-past-loan-init-deadline)
		
		;; Borrower sends collateral for their loan amount to the LP
		(try! (stx-transfer? amount tx-sender lp))

		;; Contract sends loan amount to the borrower (constraints on LP collateral amount enforced already)
		;; TODO: where does the discounted coinflow / fee / interest rate come into play?
		;; for now lets say smart contract sends LP 2 pct of loanAmount
		;; TODO: don't take a percent on the amount of collateral sent by borrower. take a % based on loan amount
		;; (try! (stx-transfer? (/ amount u100) (as-contract (var-get coinflowWallet)) lp))
		;; (try! (stx-transfer? (- amount (/ amount u100)) (as-contract (var-get coinflowWallet)) tx-sender))
		;; (as-contract (try! (stx-transfer? (/ amount u100) (var-get coinflowWallet) lp)))
		;; (as-contract (try! (stx-transfer? (- amount (/ amount u100)) (var-get coinflowWallet) tx-sender)))
		(try! (initialize-loan (/ amount u2) tx-sender lp))
		;; Establish the repayment deadline 
		;; TODO: update from hardcoded value of ~30 days = 4320 blocks
		;; (map-set repayDeadline tx-sender (+ block-height ((unwrap! (get-loan-length-from tx-sender) err-no-interest))))
		;; (map-set repayDeadline tx-sender (+ block-height u4320))
		(map-set repayDeadline tx-sender (+ block-height (unwrap! (get-loan-length-from tx-sender) err-no-interest)))
        
        ;; Initialize the loan and store info in map
        (map-set loans
            {borrower: tx-sender, LP: lp}
            {
                originalLoanAmount: (/ amount u2),
                repaymentBlockDeadline: (+ block-height (unwrap! (get-loan-length-from tx-sender) err-no-interest)),
                borrowerCollateralAmt: amount,
                lpCollateralAmt: (* u2 amount)
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

(define-private (initialize-loan (loanAmount uint) (borrower principal) (lender principal))
	(begin 
		(try! (as-contract (stx-transfer? (/ loanAmount u100) tx-sender lender)))
		(try! (as-contract (stx-transfer? (- loanAmount (/ loanAmount u100)) tx-sender borrower)))
		(ok true)
	)
)

;;TODO: assert that loan balance exists and is greater than zero.
;; assumptions:
;;     anyone can pay back anyone's loan if they want to.
;;     transfer amount doesn't exceed remaining balance
;; need to delete the loan from the map if its paid off entirely 
(define-public (repay-loan (amount uint) (borrower principal) (lender principal))
    (begin
        (try! (stx-transfer? amount tx-sender lender))
        (map-set loanBalances
            {borrower: borrower, LP: lender}
            (- (get-loan-balance borrower lender) amount)
        ) 
        (ok true)
    )
)



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
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow send-STX-collateral-to-LP u500 'ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK)

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
;; (contract-call? 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE.coinflow send-STX-collateral-to-LP u2000 'STFCVYY1RJDNJHST7RRTPACYHVJQDJ7R1DWTQHQA)
