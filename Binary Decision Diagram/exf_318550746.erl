%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% In this assignment, we are asked to implement an automatic construction machine of a Binary Decision Diagram (BDD)
%%% to represent a Boolean function, so a user is able to get a BDD representation of a function within a single call
%%% to my machine.
%%% @end
%%% Created : 16.04.2019
%%%-------------------------------------------------------------------
-module(exf_318550746).
-author("MaorAssayag").
-export([exp_to_bdd/2, solve_bdd/2]).

%% Remove dulpicate occurence from a list
%% used to construct a simple variables list
removeDuplicates([]) -> [];
removeDuplicates([H|T]) -> [H | [X || X<-removeDuplicates(T), X/=H]].


%% generates all permutations of the elements in a list
getPermutations([]) -> [[]];
getPermutations(L)  -> [[H|T] || H <- L, T <- getPermutations(L--[H])].


%% Get the parameters (atoms) from the boolean function
getParameterList(BoolFunction) -> removeDuplicates( getParameterList(BoolFunction, []) ).
getParameterList(BoolFunction, List) ->
  case BoolFunction of
    {'not', X} when is_tuple(X) -> getParameterList(X, List);
    {'not', X} -> [X|List];
    % first argument is the operation 'and' or 'or'
    {_, {A,B}} when is_tuple(A) -> getParameterList(A, List) ++ getParameterList(B, List);
    % if X isnt tuple then its a parameter
    {_, {A,B}} when is_tuple(B) -> getParameterList(B, [A|List]);
    % if both arguments isnt tuples then they are parameters
    {_, {A,B}} -> [B | [A|List]];
    % 1 or 0
    {X} when is_number(X)-> [];
    X -> [X|List]
  end.


%% Evaluate expression {Op, X}
evaluate({'not', X}) when X =:= 1 -> 0;
evaluate({'not', X}) when X =:= 0 -> 1;

evaluate({'not',X}) ->
  EvaluateX = evaluate(X),
  case EvaluateX of
    % expression statys the same
    X -> {'not',X};
    % try to minimize further
    _ -> evaluate({'not',EvaluateX})
  end;

evaluate({Op, A}) ->
  case Op of
    % OR logic
    'or' ->
      case A of
        {_,1} -> 1;
        {1,_} -> 1;
        {0,X} -> evaluate(X);
        {X,0} -> evaluate(X);
        {0,0} -> 0;
        {X,X} -> evaluate(X);
        % then evalute both
        {X,Y} -> EvaluateX = evaluate(X),
                 EvaluateY = evaluate(Y),
                 case EvaluateX of
                   X ->
                     case EvaluateY of
                       % there is nothing to minimize in this expression
                       Y -> {Op,{X,Y}};
                       % Y has been miminize - evalute again with the minimize value
                       _ -> evaluate({Op,{X,EvaluateY}})
                     end;
                  % X has been miminize - evalute again with the minimize value
                   _ -> evaluate({Op,{EvaluateX,Y}})
                 end
      end;
    % AND logic
    'and' ->
      case A of
        {_,0} -> 0;
        {0,_} -> 0;
        {1,X} -> evaluate(X);
        {X,1} -> evaluate(X);
        {1,1} -> 1;
        {X,X} -> evaluate(X);
        % then evalute both
        {X,Y} -> EvaluateX = evaluate(X),
                 EvaluateY = evaluate(Y),
                 case EvaluateX of
                   X ->
                     case EvaluateY of
                       % there is nothing to minimize in this expression
                       Y -> {Op,{X,Y}};
                     % Y has been miminize - evalute again with the minimize value
                       _ -> evaluate({Op,{X,EvaluateY}})
                     end;
                   % X has been miminize - evalute again with the minimize value
                   _ -> evaluate({Op,{EvaluateX,Y}})
                 end
      end
  end;
% return the argument
evaluate(X) -> X.


%% replace occurrences of X to Y in {A,B}
replace(X,Y,{A,B}) when is_tuple(B) ->
  case A of
    X -> {Y, replace(X,Y,B)};
    _ -> {replace(X,Y,A), replace(X,Y,B)}
  end;

replace(X,Y,{A,B}) ->
  case B of
    X ->
      case A of
        X -> {Y,Y};
        _ -> {replace(X,Y,A),Y}
      end;
    _ ->
      case A of
        X -> {Y,B};
        _ -> {replace(X,Y,A),B}
      end
  end;
replace(X, Y, X) -> Y;
replace(_, _, A) -> A.


%% Create a Node in the BDD from { {Paramerter, Left, Right}, Hight, NumOfNodes, NumOfLeafs}
% if both childs is an equal value then the current node is a redundant node
getNode(_, {L,_,_,_}, {L,_,_,_}) when is_number(L) -> {L,0,0,0};
% if both childs is a value then the current node is a leaf
getNode(Param, {L,_,_,_}, {R,_,_,_}) when is_number(L),is_number(R)-> {{Param,L,R},0,1,1};
% if both childs is equal current node is redundant
getNode(_, L, L) -> {
  element(1, L),
  element(2, L),
  element(3, L),
  element(4, L)};
% construct current nodes with the childs info
getNode(Param, L, R) ->{
  % {Paramerter, Left, Right} for e.g. {x1, {x2,1,1,1},{0,0,0,0}}
  {Param, element(1, L), element(1, R)},
  % Tree height  = the max hight of left and right + 1 for the current node
  max(element(2, L), element(2, R)) + 1,
  % Num of nodes = Num of nodes in the right, in the left + 1 for the current node
  element(3, L) + element(3, R) + 1,
  % Num of leafs = Num of lefas in the right + in the left
  element(4, L) + element(4, R)
  }.


%% Construct a tree from Boolean function, each node represent a Parameter and forked values
getTree({BoolFunc}, _) when is_number(BoolFunc) -> {BoolFunc,0,0,0};
getTree(BoolFunc, []) -> {BoolFunc,0,0,0};
getTree(BoolFunc, _) when is_number(BoolFunc)-> {BoolFunc,0,0,0};
getTree(BoolFunc, [H|T]) ->
  % in the BDD Left side arrow is H=Param=0
  L  = getTree( evaluate(replace(H,0,BoolFunc)), T),
  % in the BDD Right side arrow is H=Param=1
  R = getTree( evaluate(replace(H,1,BoolFunc)), T),
  % Create current Node from the childs info
  getNode(H,L,R).


%% Return BDDS list, each one corresponds  to each Parameters permutation
getBDDS(BoolFunc, Permutations) -> getBDDS(BoolFunc, Permutations, []).
getBDDS({BoolFunc}, _, []) when is_number(BoolFunc) -> {BoolFunc};
getBDDS(BoolFunc, [H|T], BddsList) -> getBDDS(BoolFunc, T, [getTree(BoolFunc,H) | BddsList]);
getBDDS(_, [], BddsList) -> BddsList.

%% The function receives a Boolean function and returns the corresponding BDD tree
%% representation for that Boolean function.
%% Variable Ordering can be one of the following atoms: tree_height,
%% num_of_nodes or num_of_leafs.
exp_to_bdd(BoolFunc, Ordering) ->
  % capture the inital time stamp
  InitTime = getTime(),
  Ans = getOptimumBDD(getBDDS(BoolFunc, getPermutations(getParameterList(BoolFunc))), Ordering),
  io:fwrite(io_lib:format("Finished in ~p ns\n", [(getTime()-InitTime)])),
  Ans.

% %% Test to show all BDDs permutation - not in use
% exp_to_bdd2(BoolFunc) ->
%   % capture the inital time stamp
%   InitTime = getTime(),
%   Ans = getBDDS(BoolFunc, getPermutations(getParameterList(BoolFunc))),
%   io:fwrite(io_lib:format("Finished in ~p ns\n", [getTime()-InitTime])),
%   Ans.


%% Return the current Time stamp
getTime() -> os:system_time().


%% Return the optimum Bdd tree from Bdds list according to the Ordering specified
getOptimumBDD(Bdds, Ordering) ->
  case Ordering of
    tree_height  -> getOptimumBDD(Bdds,2,[]);
    num_of_nodes -> getOptimumBDD(Bdds,3,[]);
    num_of_leafs -> getOptimumBDD(Bdds,4,[]);
    _ -> io:fwrite("No such ordering supported\n")
  end.
getOptimumBDD([CurrBdd|Bdds], Ordering, []) -> getOptimumBDD(Bdds, Ordering, CurrBdd);
% if we done, return the bdd itself (without his specs : height, num of nodes/leafs)
getOptimumBDD([],_,CurrBdd) -> element(1,CurrBdd);
% if the next Bdd is more optimum replace the CurrBdd
getOptimumBDD([NextBdd|Bdds], Ordering, CurrBdd) when element(Ordering,NextBdd) < element(Ordering, CurrBdd) -> getOptimumBDD(Bdds, Ordering, NextBdd);
% Continue with the Current Bdd
getOptimumBDD([_|Bdds], Ordering, CurrBdd) -> getOptimumBDD(Bdds, Ordering, CurrBdd).


%% The function receives a BDD tree and a list of values for every Boolean variable that’s used in the
%% Boolean function and returns the result of that function, according to the given BDD tree.
%% Given values may be either in the form of true/false or 0/1.
solve_bdd(Bdd,List) ->
  InitTime = getTime(),
  Ans = solve_bdd_tree(Bdd,List),
  io:fwrite(io_lib:format("Finished in ~p ns\n", [(getTime()-InitTime)])),
  Ans.


%% Solve the Bdd tree according to the Tuple lists of {Param,Value}
solve_bdd_tree({Param, L, R}, TupleList)->
  case getValue(Param, TupleList) of
    % Current Param=1 Then go right in the tree
    1 -> solve_bdd_tree(R, TupleList);
    true -> solve_bdd_tree(R, TupleList);
    % Current Param=o Then go right in the tree
    0-> solve_bdd_tree(L, TupleList);
    false -> solve_bdd_tree(L, TupleList)
  end;
solve_bdd_tree(RemainBdd, _) -> RemainBdd.


%% Get the parameter value from the parameter list [{Param1,Value},{Param2,Value}..]
getValue(Param, TupleList) -> element(2, lists:keyfind(Param, 1, TupleList)).
