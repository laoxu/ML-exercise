fun insort lessequal =
    let fun ins (x,[]) = [x]
            | ins (x, y::ys) =
                if lessequal(x,y) then x::y::ys
                                    else y :: ins (x,ys)
        fun sort [] = []
            | sort (x::xs) = ins (x,sort xs)
    in sort end;

fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

fun map f [] = []
    | map f (x::xs) = (f,x) :: map f xs;

fun filter pred [] = []
    | filter pred (x::xs) =
        if pred x then x:: filter pred xs
                    else filter pred xs;

fun takewhile pred [] = []
    | takewhile pred (x::xs) =
        if pred x then x :: takewhile pred xs
                else [];

fun dropwhile pred [] = []
    | dropwhile pred (x::xs) =
        if pred x then dropwhile pred xs
                    else x::xs;


fun exists pred [] = false
    | exists pred (x::xs) = (pred x) orelse exists pred xs;

fun all pred [] = true
    | all pred (x::xs) = (pred x) andalso all pred xs;


fun foldl f e [] = e
    | foldl f e (x::xs) = foldl f (f(x,e)) xs;

fun foldr f e [] = e
    | foldr f e (x::xs) = f(x, foldr f e xs);


















