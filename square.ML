

fun findroot (a, x, acc) =
    let val nextx = (a/x + x) /2.0
    in if abs (x-nextx) < acc*x
        then nextx else findroot(a, nextx, acc)
    end;

fun sqroot a = findroot(a, 1.0, 1.0E~10);

fun sqroot2 a =
    let val acc = 1.0E~10
        fun findroot x =
            let val nextx = (a/x + x) / 2.0
            in if abs (x-nextx) < acc * x
                then nextx else findroot nextx
            end
    in findroot 1.0 end;

fun pos d = neg(d-2.0) + 1.0/d
and neg d = if d>0.0 then pos(d-2.0) - 1.0/d
                        else 0.0;

fun sum (d,one) =
        if d>0.0 then sum(d-2.0,~one) + one/d else 0.0;











