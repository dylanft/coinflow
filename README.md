# coinflow
Proof of Concept for putting collateral to work for a short term loan.

[embed]https://www.bitflow.finance/_files/ugd/a03832_cf67cb1e86bf44a3a5186e36357c4559.pdf[/embed]
<!-- 
Example:
>> ::set_tx_sender ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
>> (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc signal-interest u1000000 u145)


>> ::set_tx_sender ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
>> ::get_assets_maps

>> (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc lock-up-LP-collateral 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 u288 u1000000)

>> ::set_tx_sender ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
>> (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.coinflow-usda-xbtc send-collateral-to-LP u40 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG none) -->
