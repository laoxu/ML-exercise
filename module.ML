(*
    signature
    structure
    abstract type
    functor
*)

(* 7.1 Representing queues as lists *)
structure Queue1 =
    struct
    type 'a t = 'a list;
    exception E;

    val empty = [];
    fun enq(q,x) = q @ [x];
    fun null(x::q) = false;
        | null _ = true;
    fun hd (x::q) = x
        | hd [] = raise E;
    fun deq (x::q) = q
        | deq [] = raise E;
    end;






































