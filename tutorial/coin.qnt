module coin {

    type Addr = str
    type UInt = int

    pure val MAX_UINT = 20

    pure def isUInt(i: int): bool = (0 <= i and i <= MAX_UINT)
    
    pure val ADDR = Set("null", "alice", "bob", "charlie", "eve")

    var minter: Addr
    var balances: Addr -> UInt

    val state = { minter: minter, balances: balances }
    def require(cond: bool) : bool = cond
    val totalSupply = ADDR.fold(0, (sum, a) => sum + balances.get(a))
    
    action init: bool = {
        nondet sender = oneOf(ADDR)
        all {
            minter' = sender,
            balances' = ADDR.mapBy(_ => 0),
        }
    }

    action mint(sender: Addr, receiver: Addr, amount: UInt): bool = all {
        sender == minter,
        val newBal = balances.get(receiver) + amount
        all {
            require(isUInt(newBal)),
            balances' = balances.set(receiver, newBal),
            minter' = minter,
        }
    }

    action send(sender: Addr, receiver: Addr, amount: UInt): bool = all {
        not(amount > balances.get(sender)),
        if (sender == receiver) {
            balances' = balances
        } else {
            val newSenderBal = balances.get(sender) - amount
            val newReceiverBal = balances.get(receiver) + amount
            all {
                isUInt(newSenderBal),
                isUInt(newReceiverBal),
                balances' = balances
                    .set(sender, newSenderBal)
                    .set(receiver, newReceiverBal)
            }
        },
        minter' = minter,
    }

    action step : bool = {
        nondet sender = oneOf(ADDR)
        nondet receiver = oneOf(ADDR)
        nondet amount = 1.to(100).oneOf()
        any {
            mint(sender, receiver, amount),
            send(sender, receiver, amount),
        }
    }

    val balancesRangeInv: bool =  {
        ADDR.forall(a => isUInt(balances.get(a)))
    }

    val totalSupplyDoesNotOverflowInv: bool = {
        isUInt(totalSupply)
    }

    temporal NoSupplyOverflow: bool = always(totalSupplyDoesNotOverflowInv)

    run sendWithoutMintTest = {
        init.then(send(minter, "bob", 5)).fail()
    }

    run mintSendTest = {
        init.then(mint(minter, "bob", 10))
            .then(send("bob", "eve", 4))
            .then(all {
                assert(balances.get("bob") == 6),
                assert(balances.get("eve") == 4),
                minter' = minter,
                balances' = balances,
            })
    }

    run overflowTest = {
        init
            .then(mint(minter, "bob", MAX_UINT))
            .then(send("bob", "eve", MAX_UINT))
            .then(mint(minter, "bob", MAX_UINT))
            .then(send("bob", "eve", MAX_UINT))
            .then(all {
                assert(totalSupplyDoesNotOverflowInv),
                balances' = balances,
                minter' = minter,
            })
            .fail()
    }


}