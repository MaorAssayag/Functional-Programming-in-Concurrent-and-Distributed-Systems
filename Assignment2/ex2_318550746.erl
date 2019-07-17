%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21.3.2019
%%%-------------------------------------------------------------------
-module(ex2_318550746).
-author("MaorAssayag").

-export([findKelem/2, reverse/1, deleteKelem/2, addKelem/3, union/2]).

%% findKelem(List,K) - Returns the K’th element of List. In case of an error, it should return an atom, notFound
% Exported method with arity = 2
findKelem(List,K) -> findKelem(List, K, 0).
% When K > 1 continue searching
findKelem([_|T],K,_) when K > 1 -> findKelem(T, K-1, 0);
% When k = 1 return the Head, which is the value corrispond to the original K
findKelem([H|_],1,_) -> H;
% If the List is empty return notFound - K is out of the List bounds.
findKelem(_,_,_) -> notFound.

%% reverse(List) - Reverse List’s items
% Exported method with arity = 1
reverse(List) -> reverse(List, []).
% If the Tali is empty, return what constructed so far
reverse([],Right) -> Right;
% Connect the head as the head of what constructed so far, starting with an empty list
reverse([H|T],Right) -> reverse(T,[H|Right]).

%% deleteKelem(List,Elem) - Deletes all instances of Elem from List
% Exported method with arity = 2
% X - the list withoth elements that equal to Elem, using List Comprehensions
deleteKelem(List,Elem) -> [X || X <- List, X =/= Elem].

%% addKelem(List,K,Elem) - Adds Elem to List in K’th place. Assume valid input
% Exported method with arity = 3
addKelem(List,K,Elem)  -> addKelem(List,K,Elem,[]).
% When K > 1, increase the iterator - which means connect the current H to the left of Left.
addKelem(List,1,Elem,Left) -> addKelem(List,0,Elem,[Elem | Left]);
% K>list length ->  Currently just add the Elem to be the last item.
addKelem([],K,Elem,Left) when K > 1 -> addKelem([],0,Elem,[Elem | Left]);
% If K  1 we got to the required index, insert Elem to the Left and connect with the Tali from its right
addKelem([],_,_,Left) -> reverse(Left);
% If K > 1 and we left with empty tail, its means that we got invalid input.
addKelem([H|T],K,Elem,Left) -> addKelem(T,K-1,Elem,[H|Left]).

%% union(List1,List2) - Returns the union of these two lists and removes multiple instances from both lists
% Exported method with arity = 2
union(List1,List2) -> union(List1,List2,[]).
% exit from recursion
union([],[],_) -> [];
% if we done with the first list, add the second list without duplicates there
union([],[H2|T2],_) -> [H2|union(T2,[],[])];
% main recursion, elminate the head vlaue from the rest of the list
union([H1|T1],List2,_) -> [H1|union(deleteKelem(T1,H1),deleteKelem(List2,H1),[])].