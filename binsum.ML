fun bincarry (0,ps) = ps
    | bincarry (1,[]) = [1]
    | bincarry (1,p::ps) = (1-p) :: bincarry(p,ps);


fun binsum (c, [], qs) = bincarry (c, qs)
    | binsum (c, ps, []) = bincarry (c,ps)
    | binsum (c, p::ps, q::qs) =
        ((c+p+q) mod 2) :: binsum((c+p+q) div 2, ps, qs);

binsum(0, [1,1,0,1], [0,1,1,1,1]);

fun bcarry (false,ps) = ps
    | bcarry (true,[]) = [true]
    | bcarry (true, p::ps) = not p :: bcarry(p,ps);

