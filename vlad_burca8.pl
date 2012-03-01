sample([(alaska,oregon), (vermont,alaska), (hawaii,alaska),
		(texas,hawaii), (hawaii,texas), (texas,vermont),
		(hawaii,newyork), (hawaii,california), (vermont,newyork),
		(vermont,california)]).
sample2([(1,2),(2,3),(1,3),(3,4),(4,5),(3,5),(5,6),(6,1),(5,1)]).

edge(Edge,Graph) :- member(Edge,Graph).

check_tuple((_,Node),Node).
check_tuple((Node,_),Node).

check_duplicates([],_).
check_duplicates([Node|NodesRem],Singles) :- \+member(Node,Singles), append(Singles,[Node],NewSingles), check_duplicates(NodesRem,NewSingles).


tuple_to_list([(X,Y)],[X,Y]).
tuple_to_list([(X,Y)|RTuple], [X,Y|RList]) :- tuple_to_list(RTuple,RList).

delete_all_occ(X,[],[]).
delete_all_occ(X,[X|T],R) :- delete_all_occ(X,T,R).
delete_all_occ(X,[H|T],[H|R]) :- X\==H, delete_all_occ(X,T,R).

rem_duplicates([],[]).
rem_duplicates([H|T],[H|R]) :- delete_all_occ(H,T,S), rem_duplicates(S,R).
 
nodes(Graph,Nodes) :- tuple_to_list(Graph,List), rem_duplicates(List,Nodes).


path(Graph,From,To) :- path(Graph,From,To,P).

path(Graph,From,To,Path) :- path_(Graph,From,To,[From],RevPath), reverse(RevPath,Path).
path_(Graph,From,To,Path,[To|Path]) :- edge((From,To),Graph).
path_(Graph,From,To,Visited,Path) :- edge((From,Aux),Graph), Aux \== To, \+member(Aux,Visited), path_(Graph,Aux,To,[Aux|Visited],Path).


breadthFirstPath(Graph,From,To,List) :- breadthFirstSearch(Graph,To,List,[(From,[From])]).

breadthFirstSearch(Graph,To,List,[(From,NodeList)|R_Queue]) :- edge((From,To),Graph), append(NodeList,[To],List).
breadthFirstSearch(Graph,To,List,[(From,NodeList)|R_Queue]) :- addToQueue(Graph,From,NodeList,R_Queue,N_Queue), breadthFirstSearch(Graph,To,List,N_Queue).


addToQueue([],From,NodeList,Queue,Queue).
addToQueue([(From,To)|Edges],From,NodeList,Queue,N_Queue) :- \+member(To,NodeList), append(NodeList,[To],N_NodeList), append(Queue,[(To,N_NodeList)],Aux_Queue), addToQueue(Edges,From,NodeList,Aux_Queue,N_Queue).
addToQueue([(X,To)|Edges],From,NodeList,Queue,N_Queue) :- X\==From, !, addToQueue(Edges,From,NodeList,Queue,N_Queue); member(To,NodeList),addToQueue(Edges,From,NodeList,Queue,N_Queue).

			
