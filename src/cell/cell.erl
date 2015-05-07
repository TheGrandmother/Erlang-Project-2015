%% @author grandmother
%% @doc @todo Add description to cell.


-module(cell).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

spawn_cell(Position) -> 
	C0 = utils:initCell(Position),
	C1 = utils:setCellLog(C0,logger:makeLog("Cell",self())),
	logger:logEvent(getCellLog(C1),"Cell spawned.")
	receive
		 {Sender, Payload} ->
			case Payload of 
				{linkup,_Hood,_Next} ->
					logger:logEvent(getCellLog(C1),"Received linkup message."),
					C2 = setCellHood(C1,Hood),
					C3 = setCellNext(C2,Next),
					C4 = setCellMetadata(C3,{none,[]})
				_A ->
					io:format("Cell received wierd Payload(~p).Chrashing the system.~n",[Payload]),
					logger:logWarning(getCellLog(C1),"Cell recieved mallformed linkupmessage. Crashing system~n"),
					exit(failure)
			end;
		All=_ ->
			io:format("Cell received wierd message while awaiting linkup(~p).Chrashing the system.~n",[All]),
			logger:logWarning(getCellLog(C1),"Cell recieved wrong message while waiting linkup. Crashing system~n"),
			exit(failure)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


