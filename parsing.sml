(* parser for the following grammar:
     e ::= n | n+e | n-e
     n ::= d | dn
     d ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
*)
(* Datatypes for defining parse tree *)
datatype expression = NumberExpression of number 
		    | AddExpression of expression * expression 
		    | SubtractExpression of expression * expression
     and number = DigitNumber of digit 
		| NumberDigitNumber of digit * number
     and digit = Digit of char;

(* open TextIO to get openIn, lookahead, and input1 *)
open TextIO;
     
(* exception to raise when there's a syntax error *)
exception Syntax;
	  
(* digits appear sequentially from low to high in ASCII *)
fun isDigit(c) = (#"0" <= c andalso c <= #"9");
    
(* parsing functions *)
fun parse_expression IN =
    let 
	val e1 = NumberExpression(parse_number IN)
    in
	case lookahead(IN) of
	    SOME #"+" => (
	        input1(IN); (* consume + *)
		AddExpression(e1, parse_expression IN)
	    ) |
            SOME #"-" => (
	        input1(IN); (* consume - *)
		SubtractExpression(e1, parse_expression IN)
	    ) |
	    SOME _ => raise Syntax |
	    NONE => e1
    end
and parse_number IN = 
    let
	val digit = parse_digit IN
    in
	case lookahead(IN) of
	    SOME c =>
	    if isDigit(c) then NumberDigitNumber(digit, (parse_number IN))
	    else DigitNumber digit |
	    NONE => DigitNumber digit
    end
and parse_digit IN =
    case input1 IN of
	SOME c => 
	if isDigit(c) then (Digit c)
	else raise Syntax |
	NONE => raise Syntax

(* tree printing functions *)
fun indent 0 = ()
|   indent level = ( print "   "; indent(level-1))

fun print_expression(e, level) =
    ( indent(level);
      print("e\n");
      case e of
	  NumberExpression number => print_number(number,level+1) |
	  AddExpression(e1,e2) => ( print_expression(e1,level+1);
				    indent(level+1);
				    print "+\n";
				    print_expression(e2,level+1)) |
	  SubtractExpression(e1,e2) => ( print_expression(e1,level+1);
					 indent(level+1);
					 print "-\n";
					 print_expression(e2,level+1)))
and print_number(n, level) =
    ( indent(level);
      print("n\n");
      case n of
	  DigitNumber d => print_digit(d,level+1) |
	  NumberDigitNumber(d, n) => ( print_digit(d,level+1);
				       print_number(n,level+1)))
and print_digit(Digit c, level) = 
    ( indent(level);
      print("d\n");
      indent(level+1);
      print(str(c));
      print("\n"))

(* open test, parse it, and print the result *)
val infile = openIn("test");
    
print_expression(parse_expression(infile),0);
