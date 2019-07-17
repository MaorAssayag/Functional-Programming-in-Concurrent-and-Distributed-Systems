%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 24.05.2019
%%%-------------------------------------------------------------------
-module(ex7_318550746).
-author("MaorAssayag").
-export([steady/1, calc/3]).

%% steady(F)
% Evaluates function F and logs all its exceptions
% Exceptions handling procedures :
%   error:Error	returns {time, error, Error}
%   exit:Exit	returns {time, exit, Exit}
%   throw:Throw	returns {time, throw, Throw}
% All exceptions must be logged in a file named “myLog_<ID>.elog”
% Do not use fwrite function.
% File needs to be saved in current directory
% Successful returns need to be logged, too
%   {time, success, ReturnValue}
steady(F) ->
  file:open("myLog_318550746.elog",[read, write, append]),
  try F() of
    % {time, success, ReturnValue}
    Eval -> file:write_file("myLog_318550746.elog", io_lib:format("{~p,success,~p}~n", [erlang:localtime(), Eval]), [append]),
    Eval
  catch
    % {time, error, Error}
    error:Error -> file:write_file("myLog_318550746.elog",io_lib:format("{~p,error,~p}~n", [erlang:localtime(), Error]), [append]),
    Error;
    % {time, exit, Exit}
    exit:Exit -> file:write_file("myLog_318550746.elog",io_lib:format("{~p,exit,~p}~n", [erlang:localtime(), Exit]), [append]),
    Exit;
    % {time, throw, Throw}
    throw:Throw -> file:write_file("myLog_318550746.elog",io_lib:format("{~p,throw,~p}~n", [erlang:localtime(), Throw]), [append]),
    Throw
  end.

%% calc(division, A, B)
% Implement only the division operator of calc module
% Protect case of divided by zero with try and catch.
calc(division, A, B) -> steady(fun() -> divide(A, B, 0, 0) end).

%% divide(A, B, Res)
% 6 digit precision
% divide 2 integers : A/B, throw error if B is 0
% Precision is a simple flat to avoid recursion
divide(_, B, _, 0) when B =:= 0 -> erlang:error('badarith in division, divide by 0');
% If A > B accumulate current Res and continue
divide(A, B, Res, Precision) when A >= B -> divide (A-B, B, Res+1, Precision);
% if 1 > A > 0 compute with 6 digit precision
divide(A, B, Res, 0) when A > 0 ->
  Res + divide(A * 1000000, B, 0, 1) * 0.000001;
% return Res
divide(_, _, Res, _) -> Res.
