%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 29.05.2019
%%%-------------------------------------------------------------------
-module(ex9_318550746).
-author("MaorAssayag").
-export([etsBot/0]).

etsBot()->
  {_, File} = file:open("etsCommands.txt",[read]),
  [Type | Actions] = readFile(File),
  ets:new(myETS, [list_to_atom(Type -- "\n"), named_table]),
  makeAction(Actions,Type),
  generateFile().

%% readFile(File)
% Read all the lines from the file and return it
readFile(File) ->
  case file:read_line(File) of
  % Data
  {ok, Data} -> [Data | readFile(File)];
  % End of file
  eof -> []
end.

%% makeAction(Actions,Type)
% process all actions on the Data set
makeAction([],_) -> ok;
% H is the current action in Actions read from the File
makeAction([H|T], Type) ->
  % Extract the Command and Data from H which is separated by space
  [Command|Data] = string:tokens(H -- "\n"," "),
  % Make the action accordingly
  case Command of
    "update" -> updateElements(Data, Type);
    "insert" -> insertElements(Data, Type);
    "delete" -> deleteElements(Data);
    "lookup" -> lookupElements(Data)
  end,
  % Continue processing actions
  makeAction(T, Type).

%% insertElements(Data, Type)
% insert the elemenet Data to the ETS if not exists already (by Key in Data)
insertElements([], _) -> ok;
% Pattern matching to seperate the key and value from Data
insertElements([KeyString | [Value|T]] , Type)->
  Key = list_to_atom(KeyString),
  case Type of
    bag -> ets:insert(myETS, {Key,Value});
    _-> Is_exists = ets:member(myETS, Key),
        if
          % Is_exists = true, then do nothing
          Is_exists -> ok;
          % else insert the data to the ETS
          true -> ets:insert(myETS,{Key,Value})
        end
    end,
  % continue
  insertElements(T, Type).


%% updateElements(Data, Type)
% update the elemenet Data to the ETS if exists (by Key in Data)
updateElements([], _) -> ok;
% Pattern matching to seperate the key and value from Data
updateElements([KeyString | [Value|T]] , Type)->
  Key = list_to_atom(KeyString),
  case Type of
    bag -> ets:insert(myETS, {Key,Value});
    _-> Is_exists = ets:member(myETS, Key),
        if
          % Is_exists = true, then update it
          Is_exists -> ets:insert(myETS,{Key,Value});
          % else do nothing
          true -> ok
        end
    end,
  % continue
  updateElements(T, Type).

%% deleteElements(Data)
% delete the elemenet Data to the ETS if exists (by Key in Data)
deleteElements([]) -> ok;
% Pattern matching to seperate the key and value from Data
deleteElements([KeyString|T])->
  Key = list_to_atom(KeyString),
  Is_exists = ets:member(myETS, Key),
  if
    % if this elemenet exists in the ETS then delete it
    Is_exists -> ets:delete(myETS, Key);
    % else do nothing
    true -> ok
  end,
  % continue
  deleteElements(T).

%% lookupElements(Data)
% lookupElements the elemenet Data in the ETS if exists - print it
lookupElements([]) -> ok;
% Pattern matching to seperate the key and value from Data
lookupElements([KeyString|T])->
  Key = list_to_atom(KeyString),
  Is_exists = ets:member(myETS,Key),
  if
    Is_exists ->
      Items = ets:lookup(myETS, Key),
      % print each element in Items
      lists:foreach(fun({K,V})-> io:format(list_to_binary(["key : ",atom_to_list(K)," value : ",V," \n"])) end, Items);
    true -> ok
  end,
  % continue
  lookupElements(T).

generateFile()->
  % Convert Ets to a list
  EtsList = ets:tab2list(myETS),
  % create ETS file
  file:open("etsRes_318550746.ets",[write]),
  % write the list
  writeEntry(lists:sort(EtsList)).

%% writeEntry(EtsList)
% write each item in EtsList and delete the ets object
writeEntry([]) -> ets:delete(myETS),ok;
writeEntry([{Key,Value}|T])->
  file:write_file("etsRes_318550746.ets", list_to_binary([atom_to_list(Key)," ",Value,"\n"]),[append]),
  writeEntry(T).
