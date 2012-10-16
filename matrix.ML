fun headcol [] = []
    | headcol ((x::_) :: rows) = x :: headcol rows;

fun tailcol [] = []
    | tailcol ((_::xs) :: rows) = xs :: tailcol rows;

fun transp ([]::rows) = []
    | transp rows = headcol rows :: transp(tailcol rows);

fun dotprod ([], []) = 0.0
    | dotprod (x::xs, y::ys) = x*y + dotprod(xs,ys);

fun rowprod (row,[]) = []
    | rowprod (row,col::cols) =
        dotprod(row,col) :: rowprod(row,cols);
fun rowlistprod ([], cols) = []
    | rowlistprod (row::rows, cols) =
        rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(rowsA,rowsB) = rowlistprod(rowsA, transp rowsB);

fun rowneg [] = [] : real list
    | rowneg (x::xs) = ~x :: rowneg xs;

fun matneg [] = []
    | matneg (row::rows) =
        rowneg row :: matneg rows;

fun rowsum ([],[]) = [] : real list
    | rowsum (x::xs,y::ys) = (x+y) :: rowsum(xs,ys);

fun matsum ([],[]) = []
    |matsum (Arow::rowA,Brow::rowB) =
        rowsum(Arow,Brow) :: matsum(rowA,rowB);



