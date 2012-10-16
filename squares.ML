
fun squares r =
    let fun between (x,y) =
        let val diff = r - x*x
            fun above y =
                if y > x then []
                else if y*y <diff then above (y+1)
                else if y*y = diff then (x,y) :: between(x-1,y+1)
                else between(x-1,y)
            in above y end;
        val firstx = floor(Math.sqrt(real r))
    in between (firstx, 0) end;

    fun next(xlist, y::ys) : int list =
        if hd xlist <= y then next(y::xlist, ys)
        else
            let fun swap [x] = y::x::ys
                | swap (x::xk::xs) =
                    if xk>y then x:: swap(xk::xs)
            in swap(xlist) end;

    fun nextperm (y::ys) = next([y],ys);
























