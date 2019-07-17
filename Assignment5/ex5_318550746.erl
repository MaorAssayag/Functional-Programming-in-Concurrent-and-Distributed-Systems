%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 01.05.2019
%%%-------------------------------------------------------------------
-module(ex5_318550746).
-author("MaorAssayag").
-export([ringA/2, ringB/2, mesh/3, mesh_serial/3]).


%% When the construction of the circle is completed,
%% M messages need to be transmitted by process number 1 (P1)
%% ringA - every process creates its next neighbor, one after another
ringA(N,M) ->
  % spawn & register the next process
  InitialTime = getTime(),
  register(getProcessName(1), spawn(fun() -> receiveMsg(N, M, InitialTime) end)),
  %register(getProcessName(1), spawn(ex5_318550746, receiveMsg, [N, M, InitialTime] )),
  % send the next process to creat its neighbor process
  _ = getProcessName(1) ! {"create", 2}.


%% ringB - all processes are created by a central process
ringB(N,M) ->
  % spawn & register the next process
  InitialTime = getTime(),
  register(getProcessName(1), spawn(fun() -> receiveMsg(N, M, InitialTime) end)),
  % send the next process to creat its neighbor process
  ringB_loop(N, N, M, InitialTime).

%% kill all processes
killAllRing(N) -> killAllRing(N,N).
killAllRing(_,1) ->
  exit(whereis(getProcessName(1)),kill);
killAllRing(N,Curr) ->
  exit(whereis(getProcessName(Curr)),kill),
  killAllRing(N,Curr-1).


%% loop used to creat the process of ringB
% if the loop has ended, start the messaging cycle
ringB_loop(1, _, _, _) ->
  _ = getProcessName(1) ! {1, 0};
% otherwise keep creating the processes
ringB_loop(CurrIndex, N, M, InitialTime) ->
  register(getProcessName(CurrIndex), spawn(fun() -> receiveMsg(N, M, InitialTime) end)),
  ringB_loop((CurrIndex-1), N, M, InitialTime).


%% Receiving messages in a loop, a message can create a new process, send forward messages & finish the operation.
receiveMsg(N, M, InitialTime) ->
  receive
    % if we get a "create" message with the total number of processes
    {"create", N} ->
      register(getProcessName(N), spawn(fun() -> receiveMsg(N, M, InitialTime) end)),
      getProcessName(1) ! {1, 0},
      receiveMsg(N, M, InitialTime);

    % if Index < N : create a new process, send it "create" message and keep looping this function
    {"create", Index} ->
      register(getProcessName(Index), spawn(fun() -> receiveMsg(N, M, InitialTime) end)),
      getProcessName(Index) ! {"create", (Index + 1)},
      receiveMsg(N, M, InitialTime);

    % if the last message was receivied to the first PID finish the loop of P1
    {1, M} ->
      io:fwrite("Completed in ~p ns\n", [(getTime()-InitialTime)]),
      killAllRing(N);

    % else P_1 received a message from the ring
    {1, CurrMsg} ->
      getProcessName(2 rem N) ! {2 rem N, CurrMsg + 1},
      receiveMsg(N, M, InitialTime);

    % else its P_j when j > 1, pass the message to the next process in the ring
    {CurrIndex, CurrMsg} ->
      getProcessName((CurrIndex rem N) + 1) ! {(CurrIndex rem N) + 1, CurrMsg},
      %io:fwrite("i am ~p got ~p\n", [CurrIndex, CurrMsg]),
      receiveMsg(N, M, InitialTime)
  end.


%% Return the current Time stamp
getTime() -> os:system_time(nanosecond).


%% Return a name represent a process with the index Index
%% used later to register the PID with this name
getProcessName(Index) -> list_to_atom("pid" ++ integer_to_list(Index)).


%% mesh(N, M, C) The function creates a mesh (grid) of NxN processes that are connected
%% C is a number from 1 to 𝑁^2 that defines the master process.
%% Sending M messages from the master process C to its neighbors.
%% All nodes except C spread the message once totheir neighbors.
mesh(_, 0, _) ->
  no_messages_requested;
% main function
mesh(N, M, C) when C >= 1, C =< N*N ->
  register(main,self()),
  % create the grid
  _ = createGrid(N, M, C, 1,false),
  % wait for the operation to be finished
  receive
    {"done"} -> _ = killAll(N,C),
    unregister(main)
  end;
mesh(_,_,_) -> invalid_arguments.

mesh_serial(_,0,_) ->
  no_messages_requested;
% main function
mesh_serial(N,M,C)  when C >= 1, C =< N*N  ->
  _ = self() ! {"start"},
  _ = receiveMsgB(N, M, C, C, true);
mesh_serial(_,_,_) -> invalid_arguments.


%% kill all processes
killAll(N,C) -> killAll(N,C,1).
killAll(N,C,C) ->
  if
    C =:= N*N -> done;
    true -> killAll(N,C,C+1)
  end;
killAll(N,_,Curr) when Curr =:= N*N  -> exit(whereis(getProcessName(Curr)), kill);
killAll(N,C,Curr) -> exit(whereis(getProcessName(Curr)),kill), killAll(N,C,Curr+1).


%% Create a grid of NxN processes
% if we done creating the processes, send the start message to the process C
createGrid(N, _, C, Curr,_) when Curr =:= N*N + 1 ->
   _ = getProcessName(C) ! {"start"};
% otherwise keep creating the grid processes
createGrid(N, M, C, Curr,IsSerial) ->
  %register(getProcessName(Curr), spawn(ex5_318550746, receiveMsgB, [N,M,C,Curr,IsSerial])),
  register(getProcessName(Curr), spawn(fun() -> receiveMsgB(N,M,C,Curr,IsSerial) end)),
  createGrid(N, M, C, Curr+1,IsSerial).


%% Receiving messages in a loop, a message can create a new process, send forward messages & finish the operation.
%% Serial = true/false
receiveMsgB(N, M, C, CurrNode, IsSerial) ->
  receive
    % Messeage received
    {SenderNode, MsgIndex, MsgType}->
      analyzeMsg(get(getMsgName(SenderNode,MsgIndex,MsgType,CurrNode)), {SenderNode,MsgIndex,MsgType}, N, C, M, CurrNode, IsSerial);

    % start sending M messages after creating the grid
    {"start"} ->
      put("totalMsgs",0),
      put("responses",0),
      put("Initial_Time", getTime()),
      sendMessages(N, M, C, IsSerial),
      if
        % IsSerial = true, erase main process dictionary
        IsSerial -> _ = erase(),
                    finished;
        % IsSerial = false, keep looping
        true -> _ = receiveMsgB(N, M, C, C, IsSerial)
      end
  end.

%% C (central) process is sending M messages to the grid
sendMessages(N, M, C, IsSerial) ->
   sendMessages(N, M, C, 1, IsSerial).

% if this is the last message MsgIndex = M
sendMessages(N, M, C, M, IsSerial) ->
  % register this message with deafult value 0
  put(getMsgName(C, M, "receive", C), 0),
  % send the actual messages to all neighbors
  %      CurrNode, N, MsgIndex, MsgType, SenderNode, IsSerial, M, C
  sendNeighbors(C, N, M, "receive", C, IsSerial, M, C);

% if MsgIndex < M keep sending messages from C
sendMessages(N, M, C, MsgIndex, IsSerial) ->
  % register this message with deafult value 0
  put(getMsgName(C, MsgIndex, "receive", C), 0),
  % send the actual messages to all neighbors
  %       CurrNode, N, MsgIndex, MsgType, SenderNode, IsSerial, M, C
  sendNeighbors(C, N, MsgIndex, "receive", C, IsSerial, M, C),
  % keep sending messages
  sendMessages(N, M, C, MsgIndex + 1, IsSerial).


%% send to all neighbors of CurrNode a message MsgIndex of type MsgType from SenderNode
sendNeighbors(CurrNode, N, MsgIndex, MsgType, SenderNode, IsSerial, M, C) ->
  {I, J} = node2Point(CurrNode, N),
  % up
  sendNeighbor(point2Node(I, J+1, N), N, MsgIndex, MsgType, SenderNode, IsSerial, M, C),
  % down
  sendNeighbor(point2Node(I, J-1, N), N, MsgIndex, MsgType, SenderNode, IsSerial, M, C),
  % right
  sendNeighbor(point2Node(I+1, J, N), N, MsgIndex, MsgType, SenderNode, IsSerial, M, C),
  % left
  sendNeighbor(point2Node(I-1, J, N), N, MsgIndex, MsgType, SenderNode, IsSerial, M, C).


%% send CurrNode a message MsgIndex of type MsgType from SenderNode
% IsSerial = true, send to yourself
sendNeighbor(CurrNode, N, MsgIndex, MsgType, SenderNode, true, M, C) when CurrNode >= 1, CurrNode =< N*N ->
  self() ! {SenderNode,MsgIndex,MsgType},
  receiveMsgB(N, M, C, CurrNode, true);

% IsSerial = false, send to other processes
sendNeighbor(CurrNode, N, MsgIndex, MsgType, SenderNode, false, _, _) when CurrNode >= 1, CurrNode =< N*N ->
  ProcessName = getProcessName(CurrNode),
  % check if this process still alive
  case whereis(ProcessName) of
    undefined -> error_invalid_ProcessName;
    _ -> try ProcessName ! {SenderNode, MsgIndex, MsgType} of
          _ -> ok
        catch
          _ -> error_failed_sending
        end
  end;
sendNeighbor(_,_,_,_,_,_,_,_) -> invalid_CurrNode.


%% point(i,j) when i = 0 to N-1 , Node = 1 to N^2
point2Node(I, J, N) -> I*N + J+1.
node2Point(Node, N) -> {(Node-1) div N , (Node-1) rem N}.


%% convert message index and MsgType to list to be registerd in the process dictionary
getMsgName(_, MsgIndex, "receive", CurrNode) ->
  integer_to_list(CurrNode) ++ " receive " ++ integer_to_list(MsgIndex);

getMsgName(SenderNode, MsgIndex, "response", CurrNode) ->
  integer_to_list(CurrNode) ++ " got response from " ++ integer_to_list(SenderNode) ++ " on " ++ integer_to_list(MsgIndex).


% response message : current node is C
analyzeMsg(undefined,{SenderNode,MsgIndex,"response"},N,C,M,C,IsSerial) ->
  % increase the total count of messages
  put("totalMsgs", get("totalMsgs")+1),
  % register message with a deafult value of 0
  put(getMsgName(SenderNode,MsgIndex,"response",C),0),
  % increase the total count of response messages
  put("responses", get("responses")+1),
  TotalResponses = M*((N*N)-1),
  case get("responses") of
    TotalResponses ->
    % stop operation
      io:format("The operation took ~p ns with ~p responses sent back to C and ~p total messages\n",
      [getTime()-get("Initial_Time"), TotalResponses, get("totalMsgs")]),
      if
        IsSerial -> break;
        true -> main ! {"done"}
      end;
    _ ->
      if
        IsSerial -> break;
        % keep listening to messages
        true -> receiveMsgB(N, M, C, C, IsSerial)
      end
  end;

% response message : current node isn't C
analyzeMsg(undefined,{SenderNode,MsgIndex,"response"},N,C,M,CurrNode,IsSerial) ->
  % register message with a deafult value of 0
  put(getMsgName(SenderNode,MsgIndex,"response",CurrNode),0),
  %sendNeighbors(CurrNode, N, MsgIndex, MsgType, SenderNode)
  sendNeighbors(CurrNode,N,MsgIndex,"response",SenderNode, IsSerial, M, C),
  % keep listening to messages
  if
    IsSerial -> break;
    % keep listening to messages
    true ->   receiveMsgB(N, M, C, CurrNode,IsSerial)
  end;


% receive message : pass it + send response
analyzeMsg(undefined,{SenderNode,MsgIndex,"receive"},N,C,M,CurrNode,IsSerial) ->
  % register message with a deafult value of 0
  put(getMsgName(SenderNode,MsgIndex,"receive",CurrNode),0),
  %sendNeighbors(CurrNode, N, MsgIndex, MsgType, SenderNode)
  sendNeighbors(CurrNode,N,MsgIndex,"receive",SenderNode, IsSerial, M, C),
  % register message with a deafult value of 0
  put(getMsgName(SenderNode,MsgIndex,"response",CurrNode),0),
  %sendNeighbors(CurrNode, N, MsgIndex, MsgType, SenderNode)
  sendNeighbors(CurrNode,N,MsgIndex,"response",CurrNode, IsSerial, M, C),
  % keep listening to messages
  if
    IsSerial -> break;
    true ->   receiveMsgB(N, M, C, CurrNode,IsSerial)
  end;

% this message already been receivied in this node ("response"/"receive")
analyzeMsg(_,_,N,C,M,CurrNode,IsSerial) ->
  if
    CurrNode =:= C ->  put("totalMsgs", get("totalMsgs")+1);
    true -> pass
  end,
  if
    IsSerial -> break;
    true -> receiveMsgB(N, M, C, CurrNode,IsSerial)
  end.
