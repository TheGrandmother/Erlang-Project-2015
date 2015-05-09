%% @author grandmother
%% @doc @todo Add description to cell.


-module(cell).
-include_lib("eunit/include/eunit.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([spawnCell/1]).

spawnCell({X,Y}) -> 
	%C0 = utils:initCell({0,0}),
	%C1 = utils:setCellLog(C0,logger:makeLog("Cell",self())),
    C1 = {self(),{X,Y},none,none,none,none,logger:makeLog(logger:makeCoolString("Cell(~p)",[{X,Y}]),self())},
	logger:logEvent(utils:getCellLog(C1),"Cell spawned."),
    awaitLinkup(C1).


awaitLinkup(Cell) ->
    logger:logEvent(utils:getCellLog(Cell), "Awaiting linkup"),
    C1 = Cell, %Stupid
    receive
        Link = {Sender, Payload} ->
            logger:logMessage(utils:getCellLog(C1),Link),
            case Payload of 
                {linkup,Hood,Next} ->
                    
                    C2 = utils:setCellHood(C1,Hood),
                    C3 = utils:setCellNext(C2,Next),
                    C4 = utils:setCellMetadata(C3,{0,[]}),
                    %C5 = utils:setCellPos(C4,{X,Y}),
                    C5=C4,
                    logger:logEvent(utils:getCellLog(C1),"Cell initialization complete!"),
                    cellMain(C5);
                _A ->
                    ?debugFmt("Cell received wierd Payload(~p).Chrashing the system.~n",[Payload]),
                    timer:sleep(100),
                    logger:logWarning(utils:getCellLog(Cell),"Cell recieved mallformed linkupmessage. Crashing system~n"),
                    exit(failure)
            end;
        
        {Sender,Reference,ping} ->
            logger:logEvent(utils:getCellLog(Cell), "Received ping before initializtion"),
            Msg = Sender ! {self(),make_ref(),Reference,pong},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            awaitLinkup(Cell);
        
        All= _ ->
            ?debugFmt("Cell received wierd message while awaiting linkup(~p).Chrashing the system.~n",[All]),
            
            logger:logWarning(utils:getCellLog(C1),"Cell recieved wrong message while waiting linkup. Crashing system~n"),
            timer:sleep(100),
            exit(failure)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

cellMain(In_Cell) ->
	logger:logEvent(utils:getCellLog(In_Cell),"Enterd main loop"),
	{Message,New_Buffer} = message_buffer:receiver(utils:getCellMetadata(In_Cell)),
	Cell = utils:setCellMetadata(In_Cell,New_Buffer),
	logger:logMessage(utils:getCellLog(Cell),Message),
	case Message of
		
		Request = {_,_,_} ->
            
			logger:logEvent(utils:getCellLog(Cell),"Got request message"),
			New_Cell = handleRequest(Cell,Request),
			logger:logEvent(utils:getCellLog(New_Cell),"Completed request"),
			cellMain(New_Cell);
  
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless message! Crashing system"),
			?debugFmt("Cell(~p) Received pointless message ~p ~n Crashing system",[self(),_Any]),
			exit(failure)
			
	end,
	%should never happen
	exit(failure).



%%THIS FUCTION CORRESPONDS TO A STATE
handleRequest(Cell,{Sender,Reference,Payload}) ->
	case Payload of
		{set_cell_attribute, Attribute} ->
			%%{New_Cell,Status} = setAttribute(Cell,Attribute), TBI!!!
            {New_Cell,Status} = {Cell,fail},
				logger:logEvent(utils:getCellLog(Cell),"Received set attributes request."),
				case Status of
					fail ->
						logger:logEvent(utils:getCellLog(Cell),"Failed to Set attribute."),
						Msg = Sender ! {self(), make_ref(),Reference,{set_cell_attribute_reply, fail}},
                        logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
						Cell;
					sucess ->
						logger:logEvent(utils:getCellLog(Cell),"Managed to set attibute"),
						Msg = Sender ! {self(), make_ref(),Reference,{set_cell_attribute_reply, sucsess}},
                        logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
						New_Cell
				end;
		query_state ->
			logger:logEvent(utils:getCellLog(Cell),"Received state querry"),
			Msg = Sender ! {self(), make_ref(),Reference,{query_state_reply,utils:getCellAttributes(Cell)}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
			Cell;
        query_hood ->
            logger:logEvent(utils:getCellLog(Cell),"Received hood querry"),
            hoodQuerry(Cell, Sender, Reference);
        ping ->
            logger:logEvent(utils:getCellLog(Cell),"Received ping"),
            Msg = Sender ! {self(), make_ref(),Reference,pong},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            logger:logEvent(utils:getCellLog(Cell),"Replied with a pong"),
            Cell;
            
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless Request! Crashing system"),
			?debugFmt("Cell(~p) Received pointless request ~p ~n Crashing system",[self(),_Any]),
            timer:sleep(100),
			exit(failure)
			
						
	end.

hoodQuerry(Cell,Sender,Reference) ->
    logger:logEvent(utils:getCellLog(Cell), "Processing hood request"),
    {Attributes,Refs} = buildHoodThing(Cell,tuple_to_list(utils:getCellHood(Cell)),[],[]),
    {New_Cell, Filled_Attributes} = hoodQuerryAux(Refs, Cell, Attributes),
    logger:logEvent(utils:getCellLog(Cell), "Recieved complete hood"),
    Msg = Sender! {self(),make_ref(),Reference,{query_hood_reply,list_to_tuple(Filled_Attributes)}},
    logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
    New_Cell.



hoodQuerryAux([],Cell,Attributes) ->
    {Cell,element(2,lists:unzip(Attributes))};



hoodQuerryAux(Refs,In_Cell,Attributes) ->  
    {Message,New_Buffer,New_Refs} = message_buffer:receiver(Refs,utils:getCellMetadata(In_Cell)),
    Cell0 = utils:setCellMetadata(In_Cell,New_Buffer),
    logger:logMessage(utils:getCellLog(Cell0),Message),
    
    case Message of
        {Pid, _,Key_Reference,{query_state_reply,New_Attribute}} ->
            hoodQuerryAux(New_Refs, Cell0, findAndReplace(Key_Reference,New_Attribute,Attributes,[]));
    
        _Any ->
            logger:logWarning(utils:getCellLog(Cell0),"Received pointless message Whilst in hoodquerry awaiting state! Crashing system"),
            ?debugFmt("Cell(~p) Received pointless message ~p whilst waiting for hood replies.~n Crashing system",[self(),_Any]),
            exit(failure)
    end.
    
    
findAndReplace(_,_,[],Acc) ->
    lists:reverse(Acc);
findAndReplace(Reference,Val,[Hd|Tl],Acc)->
    if
        Reference == element(1, Hd) ->
            findAndReplace(Reference, Val, Tl, [{Reference,Val}]++Acc);
        true ->
            findAndReplace(Reference, Val, Tl, [Hd]++Acc)
    end.
    

buildHoodThing(Cell,[],CoolList,Refs) ->
    
    {lists:reverse(CoolList),Refs};

buildHoodThing(Cell,[none|Tl],CoolList,Refs) ->
    %Ref = make_ref(),
    %Hd ! {self(),Ref,query_state},
    logger:logEvent(utils:getCellLog(Cell), "Skipping non existing cell"),
    buildHoodThing(Cell,Tl,[{none,none}]++CoolList,Refs);

%buildHoodThing([Derp],CoolList,Refs) ->
%    Ref = make_ref(),
%    Derp ! {self(),Ref,query_state},
%    buildHoodThing([],[{Ref,none}]++CoolList,[Ref]++Refs);

buildHoodThing(Cell,[Hd|Tl],CoolList,Refs) ->
    if
        Hd == self() ->
            logger:logEvent(utils:getCellLog(Cell), "Skipping cell"),        
            buildHoodThing(Cell,Tl,[{none,utils:getCellAttributes(Cell)}]++CoolList,Refs);

        true->
            Ref = make_ref(),
            Msg = Hd ! {self(),Ref,query_state},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Hd),
            buildHoodThing(Cell,Tl,[{Ref,none}]++CoolList,[Ref]++Refs)
    end.

%% ====================================================================
%% TESTS.... of doom!
%% ====================================================================

spawnTest() ->
    ?debugMsg("Testing Spawner"),
    logger:initLogger(),
    CellPid = spawn(fun () -> spawnCell({0,0})end),
    ?debugMsg("Spawned cell"),
    Reference = make_ref(),
    CellPid ! {self(),Reference,ping},
    ?debugMsg("Sent message"),
    receive
        {Pid,_,New_Ref,pong} ->
            ?debugMsg("Received reply message"),
            ?assertEqual(New_Ref,Reference),
            
            timer:sleep(250),
            exit(CellPid,sucsess),
            ?debugMsg("Slept and returning"),
            true;
        
        _ ->
            ?debugMsg("Received wrong message whilst testing"),
            exit(CellPid,failure),
            false
    
    after 1000 ->
        ?debugMsg("Ping timed out"),
        false
    
    end.


linkupTest() ->
    logger:initLogger(),
    Hood = {
            spawn(fun () -> spawnCell({-1,1}) end),spawn(fun () -> spawnCell({0,1}) end),spawn(fun () -> spawnCell({1,1}) end),
            spawn(fun () -> spawnCell({-1,0}) end),spawn(fun () -> spawnCell({0,0}) end),spawn(fun () -> spawnCell({1,0}) end),
            spawn(fun () -> spawnCell({-1,-1}) end),spawn(fun () -> spawnCell({0,-1}) end),spawn(fun () -> spawnCell({1,-1}) end)
           },
    Center_Cell = element(5,Hood),
    Next_Cell = element(6,Hood),
    Center_Cell ! {self(), {linkup,Hood,Next_Cell}},
    
    timer:sleep(500),
    lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
    
    true.

queryHoodTest() ->
    logger:initLogger(),
    Hood = {
            spawn(fun () -> spawnCell({-1,1}) end),spawn(fun () -> spawnCell({0,1}) end),spawn(fun () -> spawnCell({1,1}) end),
            spawn(fun () -> spawnCell({-1,0}) end),spawn(fun () -> spawnCell({0,0}) end),spawn(fun () -> spawnCell({1,0}) end),
            spawn(fun () -> spawnCell({-1,-1}) end),spawn(fun () -> spawnCell({0,-1}) end),spawn(fun () -> spawnCell({1,-1}) end)
           },
    Center_Cell = element(5,Hood),
    Next_Cell = element(6,Hood),
    Center_Cell ! {self(), {linkup,Hood,Next_Cell}},
    My_Pid = self(),
    lists:map(fun(X) -> X ! {My_Pid, {linkup,none,none}} end, [
                                                               element(1,Hood),element(2,Hood),element(3,Hood),
                                                               element(4,Hood),                element(6,Hood),
                                                               element(7,Hood),element(8,Hood),element(9,Hood)
                                                              ]),
    Reference = make_ref(),
    Center_Cell ! {self(),Reference,query_hood},
    receive
        Message = {Pid,_,Key_Reference,{query_hood_reply,_}} when Pid == Center_Cell, Key_Reference == Reference ->
            %?debugFmt("Received reqply message  ~p ~n",[Message]),
            true;
        _A ->
            ?debugFmt("Received wrong message  ~p ~nWas Expecting reference ~p from ~p ~n ",[_A,Reference,Center_Cell]),
            false
    end.


spawnTest_test() ->
    [?assert(spawnTest())].

linkupTest_test() ->
    [?assert(linkupTest())].

queryHood_test()->
    [?assert(queryHoodTest())].











