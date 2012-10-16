


fun upto (m,n) =
    if m > n then []  else m :: upto(m+1,n);

fun maxl [m] : int = m
    | maxl (m::n::ns) = if m > n then maxl (m::ns)
                        else maxl(n::ns);

fun null [] = true
    | null ([_::_]) = false;

fun hd (x::_) = x;

fun tl (_::xs) = xs;

fun prod ns = if null ns then 1
                else (hd ns) * (prod (tl ns));

fun factl (n) = prod (upto (1,n));

fun lastelement [m] = m
    | lastelement [_::n] = lastelement n;








