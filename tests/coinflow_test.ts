
import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v0.14.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Ensure that coinflow works",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        let sender = accounts.get("wallet_1")!.address;

        let block = chain.mineBlock([
            /* 
             * Add transactions with: 
             * Tx.contractCall(...)
            */
           Tx.contractCall("coinflow","signal-interest", ["u1000", "u145"], sender)
        ]);
        console.log(block);
        // assertEquals(block.receipts.length, 0);
        // assertEquals(block.height, 2);

        block = chain.mineBlock([
            /* 
             * Add transactions with: 
             * Tx.contractCall(...)
            */
        ]);
        assertEquals(block.receipts.length, 0);
        assertEquals(block.height, 3);
    },
});
