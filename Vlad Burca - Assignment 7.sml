fun sum_special([]) = (0,0)
|	sum_special([x]) = (x,0)
|	sum_special(even::odd::tail) =
		let 
			val (M,N) = sum_special(tail)
		in
			(even+M,odd+N)
		end;

fun padd([],N) = N
|   padd(M,[]) = M
|   padd(a::P1,b::P2) = (a + b)::padd(P1,P2):(real list);

fun dpadd([],N) = N
|	dpadd(M,[]) = M
|	dpadd(a::P1,b::P2) = padd(a,b)::dpadd(P1,P2);

fun smult(P,s:real) = map(fn a => a * s) P;

fun pmult([],P2) = []
|   pmult(a0::P1,P2) = padd(smult(P2,a0), pmult(P1,0.0::P2));

fun polmult(P,pol) = map(fn a => pmult(a,pol)) P;
 
fun dpmult([],P2) = []
|	dpmult(a0::P1,P2) = dpadd(polmult(P2,a0), dpmult(P1,[]::P2));

fun or_else(e,f) =
	case e of
		true => true |
		false => case f of 
					true => true |
					false => false;

fun and_also(e,f) =
	case e of
		true => (case f of
					true => true |
					false => false)
				|
		false => false;

fun uncurry F (a,b,c,d) = F a b c d;

fun filter(P) =
	fn L => case L of
				[] => [] |
				x::xs => (if P(x) then x::((filter(P))xs)
								else ((filter(P))xs));

datatype logical_expr =
		PROP_VAR of string |
		AND of logical_expr * logical_expr |
		OR of logical_expr * logical_expr |
		NOT of logical_expr;

fun member(_,nil) = false
|	member(x:string,head::tail) = 
		if x = head
			then true
			else member(x,tail);

fun eval(E:logical_expr, L) =
		case E of
			PROP_VAR p => member(p,L) | 
			AND(E1,E2) => eval(E1,L) andalso eval(E2,L) |
			OR(E1,E2) => eval(E1,L) orelse eval(E2,L) |
			NOT(E1) => not(eval(E1,L));
			
			
