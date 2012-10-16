fun change (coinvals, 0) = []
    | change (c::coinvals,amount) =
        if amount < c then change(coinvals,amount)
                else c::change(c::coinvals,amount-c);

val gb_cons = [50,20,10,5,2,1]
and us_cons = [25,10,5,1];

change(gb_cons,43);

fun allChange (coins, coinvals, 0) = [coins]
    | allChange (coins, [], amount) = []
    | allChange (coins, c::coinvals, amount) =
        if amount < 0 then []
        else allChange(c::coins, c::coinvals,amount-c) @
            allChange(coins,coinvals,amount);

fun allChangef (coins, coinvals, 0) = [coins]
    | allChangef (coins, [], amount) = []
    | allChangef (coins,(c,0)::coinvals, amount) =
        allChangef(coins, coinvals,amount)
    | allChangef (coins,(c,n)::coinvals, amount) =
        if amount < 0 then []
        else allChangef(c::coins,(c,n-1):coinvals,amount-c) @
             allChangef(coins, coinvals, amount);


















