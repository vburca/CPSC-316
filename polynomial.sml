fun padd([],N) = N
|	padd(M,[]) = M
|	padd(a::P1,b::P2) = (a + b)::padd(P1,P2):(real list);

fun peval([],_) = 0.0
|	peval(a0::P,x) = a0 + x*peval(P,x);

fun smult(P,s:real) = map(fn a => a * s) P;

fun pmult([],P2) = []
|	pmult(a0::P1,P2) = padd(smult(P2,a0), pmult(P1,0.0::P2));

fun pmult2(P1,P2) =
	case P1 of 
	[] => [] |
	(a0::P1,P2) = padd(smult(P2,a0),pmult2(P1,0.0::P2));


