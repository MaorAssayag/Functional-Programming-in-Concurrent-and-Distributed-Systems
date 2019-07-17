%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 01.05.2019
%%%-------------------------------------------------------------------
-module(ex4_318550746).
-author("MaorAssayag").
-export([flatten/1, smaller/2, replace/3, mapSub/2]).

%% flatten(List) : Returns a flat List
% if H is a List, start recursion
flatten([H|T]) when is_list(H)-> flatten(H) ++ flatten(T);
% if H isnt a List, add it to the results of flatten the tail
flatten([H|T]) -> [H|flatten(T)];
% an empty list
flatten([]) -> [].

%% smaller(List, Thr) : Return all elements in List that are smaller than or equal to Thr
% first lets flatten the List
smaller(List, Thr) -> smaller(flatten(List), Thr, []).
% Simply take the elements that less or equal then the Threshold
smaller(List, Thr, []) -> [Res || Res <- List, Res =< Thr].

%% replace(List, Old, New) : Replaces all instances of Old with New in List
%% This function keep the same order of elemntes using ++ list operation
replace(List, Old, New) -> replace(flatten(List), Old, New, []).
% end of list
replace([], _, _, Res) -> Res;
replace([H|T], Old, New, Res) ->
  case H of
    % keep iterating the List with replacing the Old value with the New value
    Old -> replace(T, Old, New, Res ++ [New]);
    % keep the value H the same
    _   -> replace(T, Old, New, Res ++ [H])
  end.

%% mapSub(List1, List2) : Subtracts List2 from List1, Follows the order of appearance in List2
% foldl - The function returns the final value of the accumulator, which sohuld be the results
% Calls subElement(Elem, AccIn) on successive elements A of List1, starting with AccIn == List2.
% subElement/2 must return a new accumulator, which is passed to the next call.
mapSub(List1,List2) -> {Result,_} = lists:foldl(fun subElement/2, {[],List2} ,List1), Result.
% if List2 is empty add the current List1 element to the results
subElement(List1Elem, {Result, []}) -> {Result ++ [List1Elem], []};
% if List2 current element is equal to the List1 current element then dont add it to the results, and remove it from List2
subElement(List1Elem, {Result, [List1Elem|T]}) -> {Result, T};
% otherwise, add the current List1 element to the results
subElement(List1Elem, {Result, [H|T]}) -> {Result ++ [List1Elem], [H|T]}.
