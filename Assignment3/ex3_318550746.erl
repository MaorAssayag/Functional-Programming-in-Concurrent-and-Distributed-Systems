%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04.04.2019
%%%-------------------------------------------------------------------
-module(ex3_318550746).
-author("MaorAssayag").

-export([matMult/1, addKElem/3, even/1, fiboR/1, fiboT/1, mSort/1, qSort/1, sortLC/1, sortLM/1, sortPM/1]).

%% matMult(Matrix) - Squares matrix Matrix, Implementing using list comprehension
matMult(Matrix) -> matMult(Matrix, transpose(Matrix)).
matMult([], _)->[];
% multiplication of each row from Matrix with the rows of the transpose Matrix
matMult([H|T], TMatrix)->
   [[lists:sum([ X*Y || {X,Y}<-lists:zip(H, H2)]) || H2<-TMatrix] | matMult(T,TMatrix)].

%% transpose - transpose the Matrix (2x2)
transpose([[]|_]) -> [];
% hd is the H of List=[H|T], tl is the L of List=[H|T]
transpose(Matrix) ->
  [lists:map(fun hd/1, Matrix) | transpose(lists:map(fun tl/1, Matrix))].

%% addKElem(List,K,Elem)
% Adds Elem in the K’th place of List (NOT replacing, but adding), Implementing using case-of
addKElem(List,K,Elem) -> addKElem(List,K,Elem, []).
% if we left with empty list just returned the Left side with Elem in the tail
addKElem([],_,Elem,Left) -> Left ++ [Elem];
% case-of
addKElem([H|T],K,Elem,Left) ->
  case K of
    % out of recursion
    0 -> Left ++ [H|T];
    % we got to the right index, insert Elem
    1 -> addKElem([H|T], 0, Elem, Left++[Elem]);
    % recursion
    K when K > 1 -> addKElem(T, K-1, Elem, Left++[H]);
    % if the input is non-valid return List
    _ ->[H|T]
  end.

%% even(List)
% Returns a list of all even members of List, in the same order as appeared in it
even(List) -> even(List, []).
% exit the recursion
even([],Result) -> Result;
% if H is even add it to the result and continue with the recursion
even([H|T], Result) when H rem 2 =:= 0 -> even(T, Result++[H]);
% then H rem is diff then 0 -> H isnt even
even([_|T], Result) -> even(T, Result).

%% fiboR(N)
% Returns the N’th Fibonacci number (1,1,2,3,5,…) - Implement it using recursion
fiboR(N) when N >= 0 -> case N of
	0 -> 0;
	1 -> 1;
 % recursion
	_ELSE -> fiboR(N-1) + fiboR(N-2)
end;
% For any other value return 0 (even non-valid inputs)
fiboR(_) -> 0.

%% fiboT(N)
% Returns the N’th Fibonacci number (1,1,2,3,5,…) - Implement it using tail recursion
fiboT(N) -> fiboT(N, 0).
% base cases
fiboT(1,Ans) -> Ans + 1;
fiboT(2,Ans) -> Ans + 1;
% tail recursion
fiboT(N,Ans) when N > 0 -> fiboT(N-1, Ans + fiboT(N - 2));
% For any other value return 0 (even non-valid inputs)
fiboT(_,_) -> 0.

%% mSort(List) - Returns a sorted List, implementing it to work as merge sort
mSort([]) -> [];
mSort(List) when length(List) =:= 1 -> List;
% Simply split the List and merged with recursion
mSort(List) -> {L1 ,L2} = lists:split(length(List) div 2, List),
               merge(mSort(L1), mSort(L2)).

%% merge(L1, L2) - Returns a mergerd List, implementing it to work as merge sort
merge(L1, L2) -> merge(L1, L2, []).
% if only 1 list remain, add it to the results
merge(L1, [], Result) -> Result ++ L1;
merge([], L2, Result) -> Result ++ L2;
% Decide where the current element will be
merge([H1|T1], [H2|T2], Result) when H1=<H2 ->
   merge(T1, [H2|T2], Result++[H1]);
merge([H1|T1], [H2|T2], Result) ->
   merge([H1|T1], T2, Result++[H2]).

%% qSort(List) - Returns a sorted List, implementing it to work as quick sort
qSort([]) -> [];
% The pivot will be the element in index 1 (the first element)
qSort([H|T]) ->
  qSort([Left || Left <- T, Left < H]) ++ [H] ++ qSort([Right || Right <- T, Right >= H]).

%% sortLC - Sorts List by List comprehension according to the residue of division by 3
% Each modulo-grouped list needs to be sorted by size
sortLC([]) -> [];
sortLC(List) ->
    % first sort all the elements in list that have remainder%3 = 0
    qSort([ZeroRem|| ZeroRem <- List,ZeroRem rem 3 =:= 0]) ++
    % then sort all the elements in list that have remainder%3 = 1
    qSort([OneRem|| OneRem <- List,OneRem rem 3 =:= 1]) ++
    % finally sort all the elements in list that have remainder%3 = 2
    qSort([TwoRem|| TwoRem <- List,TwoRem rem 3 =:= 2]).

%% sortLM - Sorts List By using lists module according to the residue of division by 3
% Each modulo-grouped list needs to be sorted by size
sortLM(List) ->
      % using list modules sort & filter by function
      lists:sort(lists:filter(fun(X)-> X rem 3 =:= 0 end, List)) ++
			lists:sort(lists:filter(fun(X)-> X rem 3 =:= 1 end, List)) ++
			lists:sort(lists:filter(fun(X)-> X rem 3 =:= 2 end, List)).

%% sortPM - Sorts List By using Pattern matching according to the residue of division by 3
% Each modulo-grouped list needs to be sorted by size
 % using a tail recursion 3 list one for each remainder
sortPM(List) -> sortPM(List,[],[],[]).
sortPM([H|T],L1,L2,L3) when H rem 3 =:= 0 -> sortPM(T, [H|L1], L2, L3);
sortPM([H|T],L1,L2,L3) when H rem 3 =:= 1 -> sortPM(T, L1, [H|L2], L3);
sortPM([H|T],L1,L2,L3) when H rem 3 =:= 2 -> sortPM(T, L1, L2, [H|L3]);
% concatenating all 3 lists after sorting them using qSort
sortPM([],L1,L2,L3) -> qSort(L1) ++ qSort(L2) ++ qSort(L3).
