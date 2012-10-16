
datatype 'a seq = Nil
                | Cons of 'a * (unit -> 'a seq);

exception Empty;
fun hd (Cons(x,xf)) = x
    | hd Nil = raise Empty;

fun tl (Cons(x,xf)) = xf()
    | tl Nil = raise Empty;

fun cons(x,xq) = Cons(x,fn() => xq);

fun from k = Cons(k,fn() => from(k+1));

fun take (xq,0) = []
    | take (Nil,n) = raise Subscript
    | take (Cons(x,xf), n) = x :: take (xf(), n-1);





















