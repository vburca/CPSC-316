val x = 5;
fun f(y) = (x + y) - 2;
fun g(h) = let val x = 7 in h(x) end;
let val x = 10 in g(f) end;

