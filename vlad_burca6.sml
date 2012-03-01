fun flip_alt [] = []
|	flip_alt ([e1]) = [e1]
|	flip_alt (e1::e2::xs) = [e2,e1] @ flip_alt(xs);

fun del_i_elem([], i) = []
|	del_i_elem (x::xs, 1) = xs
|	del_i_elem (x::xs, i) = x::del_i_elem(xs,i-1);
