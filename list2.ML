
fun nlength [] = 0
    | nlength (x::xs) = 1 + nlength xs;

local
    fun addlen (n,[]) = n
        | addlen (n,x::l) = addlen (n+x,l)
in
    fun length l = addlen (0,l)
end;

fun take ([],i) =  []
    | take (x::xs,i) = if i > 0 then x::take(xs,i-1)
                                else [];

fun rtake ([],_,taken) = taken
    | rtake (x::xs,i,taken) =
        if i>0 then rtake(xs,i-1,x::taken)
                else taken;
fun drop ([],_) = []
    | drop (x::xs,i) = if i > 0 then drop(xs,i-1)
                        else x::xs;

infixr 5 @;
fun ([] @ ys) = ys
    | ((x::xs) @ ys) = x::(xs@ys);

fun nrev [] = []
    | nrev (x::xs) = (nrev(xs)) @ [x];

fun revAppend ([], ys) = ys
    | revAppend (x::xs,ys) = revAppend(xs,x::ys);














