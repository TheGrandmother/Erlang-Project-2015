%% @author grandmother
%% @doc @todo Add description to cell.


-module(cell).

%% ====================================================================
%% API functions
%% ====================================================================
-export([spawnCell/0]).

spawnCell() -> 
	C0 = utils:initCell({0,0}),
	C1 = utils:setCellLog(C0,logger:makeLog("Cell",self())),
	logger:logEvent(getCellLog(C1),"Cell spawned.")
	receive
		 Link = {Sender, Payload} ->
			logger:logMessage(getCellLog(C1),Link),
			case Payload of 
				{linkup,_Hood,_Next,{X,Y}} ->
					
					C2 = utils:setCellHood(C1,Hood),
					C3 = utils:setCellNext(C2,Next),
					C4 = utils:setCellMetadata(C3,{0,[]}),
					C5 = utils:setCellPos(C4,{X,Y}),
					cellMain(C5);
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

cellMain(In_Cell) ->
	logger:logEvent(utils:getCellLog(In_Cell),"Enterd main loop"),
	{Message,New_Buffer} = message_buffer:receiver(utils:getCellMetadata(In_Cell)),
	Cell = utils:cellSetMetadata(Cell,New_Buffer),
	logger:logMessage(getCellLog(Cell),Message),
	case Message of
		
		Request = {Sender,Reference,Payload} ->
			
			New_Cell = handleRequest(Cell,Request),
			logger:logEvent(utils:getCellLog(New_Cell),"Completed request"),
			cellMain(New_Cell);
  
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless message! Crashing system"),
			io:format("Cell(~p) Received pointless message ~p ~n Crashing system",[self(),_Any]),
			exit(failure)
			
	end,
	%should never happen
	exit(failure).

handeRequest(Cell,{Sender,Reference,Payload}) ->
	case Payload of
		{set_cell_attribute, Attribute} ->
			{New_Cell,Status} = setAttribute(Cell,Attribute),
				logger:logEvent(utils:getCellLog(Cell),"Received set attributes request."),
				case Status of
					fail ->
						logger:logEvent(utils:getCellLog(Cell),"Failed to Set attribute."),
						Sender ! {self(), make_ref(),Reference,{set_cell_attribute_reply, fail}}
						Cell;
					sucess ->
						logger:logEvent(utils:getCellLog(Cell),"Managed to set attibute"),
						Sender ! {self(), make_ref(),Reference,{set_cell_attribute_reply, sucsess}}
						New_Cell
				end;
		query_state ->
			logger:logEvent(utils:getCellLog(Cell),"Received state querry"),
			Sender ! {self(), make_ref(),Reference,{query_state_reply,utils:getCellAttributes(Cell)}}
			Cell;
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless Request! Crashing system"),
			io:format("Cell(~p) Received pointless request ~p ~n Crashing system",[self(),_Any]),
			exit(failure)
			
						
	end.

