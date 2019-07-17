%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 26.05.2019
%%%-------------------------------------------------------------------
-module(ex8_318550746).
-author("MaorAssayag").
-export([startChat/1, joinChat/1, steadyLink/1, steadyMon/1, steadyEval/1, call/1, send/1]).

%% startChat(Name@IP) -> PID
% A chat tool that sends messages between two hosts
% Uses rpc module for sending messages
% Returns a PID of a local process to send messages to
% Spawns a process in ‘name@IP’ if there is no such process yet only
% Messages will be then sent by this process to the receiving process in ‘name@IP’
% Upon a receive of a message, it should be printed to screen with system time
% Upon a receive of a messages stat, prints the number of received/sent messages
% Upon a receive of a message quit, kills the program immediately (the remote, too)
startChat(Name@IP)->
  % Spawning an local host process LocalPID and register it on localProcessPID
  register(localProcessPID, LocalPID = spawn(fun() -> localLoop(Name@IP, 0, 0) end)),
  % Request Name@IP to join the chat with local host IP using rpc
  rpc:call(Name@IP, ex8_318550746, joinChat, [node()]),
  % Save remote IP : Name@IP
  put(remoteName@IP, Name@IP),
  % Returns the local process PID
  LocalPID.


joinChat(LocalName@IP) ->
  % Did we already register the remote process locally ?
  case whereis(remoteProcessPID) of
    % Register and spwan the remote process
    undefined ->
      register(remoteProcessPID, spawn(fun() -> remoteLoop(LocalName@IP, 0, 0) end));

    _ -> io:format("Remote Process already connected to LocalPID chat")
  end.

call(Message)->
  rpc:call(get(remoteName@IP), ex8_318550746, send, [Message]).

send(Message) ->
  remoteProcessPID ! Message.

%% Aid function - block receive loop for the local process
localLoop(RemoteName@IP, SentIndex, ReceivedIndex)->
  receive
    % Upon a receive of a messages stat, prints the number of received/sent messages
    stats ->
      io:format("local stats: sent: ~p received: ~p~n", [SentIndex, ReceivedIndex]),
      % Block-receive
      localLoop(RemoteName@IP, SentIndex, ReceivedIndex);

    % Upon a receive of a message quit, kills the program immediately (the remote, too)
    quit ->
      % Send exit message to the remote process
      {remoteProcessPID, RemoteName@IP} ! quit,
      io:format("local Process PID ~p finished succsussfuly ~n",[self()]),
      exit(requested);

    % Receive of a message from the remote process
    {fromRemote, Message} ->
      Message,
      %io:format("Local process recieved a message from Remote process : ~p ~n",[Message]),
      localLoop(RemoteName@IP, SentIndex, ReceivedIndex + 1);

    % Upon a receive of a message, it should be printed to screen with system time
    Message ->
      {remoteProcessPID, RemoteName@IP} ! {fromLocal, Message},
      %io:format("Local process recieved a message : ~p ~n",[Message]),
      localLoop(RemoteName@IP, SentIndex + 1, ReceivedIndex)
  end.


%% Aid function - block receive loop for the remote process
remoteLoop(LocalName@IP, SentIndex, ReceivedIndex) ->
  receive
    % Upon a receive of a messages stat, prints the number of received/sent messages
    stats ->
      io:format("remote stats: sent: ~p received: ~p~n", [SentIndex, ReceivedIndex]),
      % Block-receive
      remoteLoop(LocalName@IP, SentIndex, ReceivedIndex);

    % Upon a receive of a message quit, kills the program immediately (the remote, too)
    quit->
      io:format("remote : Process PID ~p finished succsussfuly ~n",[self()]),
      exit(requested);

    % Receive of a message from the local process
    {fromLocal, _}->
      remoteLoop(LocalName@IP, SentIndex, ReceivedIndex + 1);

    Message ->
      {localProcessPID, LocalName@IP} ! {fromRemote, Message},
      remoteLoop(LocalName@IP, SentIndex + 1, ReceivedIndex)
  end.

%% steadyLink(F)
% Spawns a process to evaluate function F/0
% Links the two processes
% Terminates after 5 seconds if no exception occurs
% Returns the PID of spawned process
steadyLink(F) ->
  % Spawn and link a new process with the function F
  PID = spawn_link(F),
  % Terminates after 5 seconds if no exception occurs
  timeOut(PID).
timeOut(PID)->
  receive
    _-> ok
  after 5000 -> PID
  end.


%% steadyMon(F)
% Spawns a process to evaluate function F/0
% Monitors the spawned process
% Returns the PID of spawned process
% Catches any type of termination of F/0 and returns the corresponding result (string)
% “Normal termination of process 0.24.0 was detected”
% “An exception in process 0.24.0 was detected: <ExceptionType>”
% <ExceptionType> is one of the following: error, exit or throw
% Terminates after 5 seconds and kills the spawned process.
steadyMon(F) ->
  spawn_monitor(ex8_318550746, steadyEval, [F]),
  % Wait for termination message from the above process
  receive
    % Normal termination
    {_,_,_, PID, normal} ->
      "Normal termination of process " ++ getPID(PID) ++ " was detected";

    % Termination with an a exception
    {_,_,_, PID, Reason} ->
      "An exception in process " ++ getPID(PID) ++ " was detected: " ++ atom_to_list(Reason)
  end.

%% getPID(PID)
%  Extract the x.y.z PID number from termination message PID component
getPID(PID)->
  string:sub_string(pid_to_list(PID), 2, (string:len(pid_to_list(PID))-1)).

%% steadyEval(F)
% aid function, evalute the function F on the new process that have had been monitored
% <ExceptionType> is one of the following: error, exit or throw
steadyEval(F) ->
  try F() of
    _	-> normal
  catch
    error:_Error 	-> exit(error);
    exit:_Exit	-> exit(exit);
    throw:_Throw	-> exit(throw)
  end.
