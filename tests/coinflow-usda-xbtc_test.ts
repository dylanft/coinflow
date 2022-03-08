
import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v0.14.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Ensure loan initialization works as intended. Basic stepthrough.",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        let deployer = accounts.get("deployer")!.address;
        // let coinflow_contract = accounts.get("wallet_1")!.address;
        let borrower = accounts.get("wallet_1")!.address;
        let lender = accounts.get("wallet_2")!.address;

        let block = chain.mineBlock([
            /* 
             * Add transactions with: 
             * Tx.contractCall(...)
            */
            Tx.contractCall("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc","signal-interest", ["u1000000", "u145"], borrower),
            Tx.contractCall("coinflow-usda-xbtc","lock-up-LP-collateral", [types.principal(borrower), "u288", "u1000000"], lender),
            // Tx.contractCall("coinflow-usda-xbtc","send-collateral-to-LP", ["u40", types.principal(lender), "none"], borrower)

        ]);
        // assertEquals(block.receipts.length, 0);
        assertEquals(block.height, 2);

        block = chain.mineBlock([
            /* 
             * Add transactions with: 
             * Tx.contractCall(...)
            */
            // Tx.contractCall("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc","get-loan-interest-from", [borrower], borrower), 
            // Tx.contractCall("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc","lock-up-LP-collateral", [borrower, "u288", "u1000000"], lender.address)

        ]);


        let loanHealth = chain.callReadOnlyFn('coinflow-usda-xbtc', 'get-loan-health', [types.principal(borrower), types.principal(lender)], borrower);
        console.log(loanHealth);
        

        let loanBalance = chain.callReadOnlyFn('coinflow-usda-xbtc', 'get-loan-balance', [types.principal(borrower), types.principal(lender)], borrower);
        console.log(loanBalance);
        // loanBalance.result.expectUint(1000000)
        // assertEquals(block.receipts.length, 0);
        assertEquals(block.height, 3);
    },
});
