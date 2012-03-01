mymember(H,[H|_]).
mymember(X,[_|T]) :- mymember(X,T).
% or mymember(X,[H|T]) :- X=H; mymember(X,T).

myappend([],Y,Y).
myappend([H|T],Y,[H|T1]) :- myappend(T,Y,T1).

samelength([],[]).
samelength([_|T1],[_|T2]) :- samelength(T1,T2).

myreverse([],[]).
myreverse([H|T],Z) :- samelength([H|T],Z), myreverse(T,TR), myappend(TR,[H],Z).

myreverse3(X,Z) :- myreverse2(X,[],Z).
myreverse2([],Y,Y).
myreverse2([H|T],Y,Z) :- myreverse2(T,[H|Y],Z).

mylength([],0).
mylength([_|T],N) :- mylength(T,N1), N is N1+1.

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

perm([],[]).
perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).

ordered([]).
ordered([_]).
ordered([X,Y|Z]) :- X < Y, ordered([Y|Z]).

mysort(X,Y) :- perm(X,Y), ordered(Y).

mysubset([X|R],S) :- mymember(X,S), mysubset(R,S).
mysubset([],_).

myunion([X|Y],Z,W) :- member(X,Z), union(Y,Z,W).
myunion([X|Y],Z,[X|W]) :- \+ member(X,Z), union(Y,Z,W).

myintersection([X|Y],Z,W) :- myintersection(Y,Z,U), (mymember(X,Z) -> W=[X|U]; W=U).
myintersection([],_,[]).

% mydelete(X,L,R) - deletes all occurrences of X from L to produce R
% myprune(A,B) - remove multiple instances of elements from A giving B
% prefix(A,B) - satisfied if list A is a prefix of list B
% segment(A,B) - satisfied if list A is contained in list B
