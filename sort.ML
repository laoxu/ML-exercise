(* sorting is one of the most studied topics in the theory of computing.
serval sorting algorithms are widely known. to sort n items, insertion sort takes
order n2 time; merge sort takes order n log n time ; quick sort takes order n log n on average,
n2 in the worst case. *)

local val a = 16807.0 and m = 2147483647.0
in fun nextrand seed =
        let val t = a*seed
        in t - m * real(floor(t/m)) end
end;
fun randlist (n,seed,tail) =
    if n = 0 then (seed,tail)
    else randlist(n-1,nextrand seed, seed::tail);


fun ins (x,[]): real list = [x]
    | ins (x,y:ys) =
        if x<=y then x::y::ys
            else y::ins(x,ys);

fun insort [] = []
    | insort (x::xs) = ins(x, insort xs);

fun quick [] = []
    | quick [x] = [x]
    | quick (a::bs) =
        let fun partition (left,right,[]): real list =
            (quick left) @ (a :: quick right)
                | partition (left,right, x::xs) =
                    if x<=a then partition (x::left,right,xs)
                            else partition (left,x::right,xs)
        in partition([],[],bs) end;

fun quicker ([],sorted) = []
    | quicker ([x],sorted) = x::sorted
    | quicker (a::bs,sorted) =
        let fun partition (left,right,[]): real list =
                quicker(left,a::quicker(right,sorted))
            | partition (left,right,x::xs) =
                if x<=a then partition (x::left,right,xs)
                        else partition (left,x::right,xs)
        in partition([],[],bs) end;


fun find ([],i) = []
    | find (a::bs,i) =
        let fun partition (left,right,[]) :real =
                let val l = length left
                    in if i <l then find(left,i)
                        else if i =1 then a
                        else find (right,i-l-1)
                end
               | partition (left,right,x::xs) =
                    if x<=a then partition (x::left,right,xs)
                            else partition (left,x::right,xs)
        in partition([],[],bs) end;

fun findrange ([], i, j) = []
    | findrange (a::bs,i,j) = (*the head "a " is the pivot*)
        let fun partition (left,right,[]) : real list =
                    let val l = length left
                    int findrange (left,i ,Int.min(j,l-1)) @
                            (if i<=l andalso l<=j then [a] else []) @
                        findrange (right,Int.max(0,i-l-1), j-l-1)
                    end
                | partition (left, right, x::xs) =
                            if x<=a then partition (x::left,right,xs)
                                else partition (left,x::right,xs)
        in if i>j then [] else partition([],[],bs) end;




























