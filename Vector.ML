val zerovec = (0.0,0.0);
val a = (1.5,6.8);
val b = (3.6,0.9);

fun lengthvec (x,y) : real = Math.sqrt(x * x + y * y);

lengthvec a;

type loc = real * real;

fun dist ((x0,y0),(x1,y1)) =
    let
        val dx = x1 - x0;
        val dy = y1 - y0;
    in
      Math.sqrt(dx * dx + dy * dy)
    end





