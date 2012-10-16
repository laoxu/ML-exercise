
fun testcase (p,q) =
    case p-q of
        0 =>  "zero"
        | 1 => "one"
        | 2 => "two"
        | n => if n<10 then "lots" else "lots and lots";



fun upto (m,n) =
    if m > n then []  else m :: upto(m+1,n);