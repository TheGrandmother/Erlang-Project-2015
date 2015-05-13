%% @author grandmother
%% @doc Well. This is the humongous file for the cell actor.
%% Most of these functions are internal and will only be given brief comments.
%% Se the logging messsages for a more "natuaral" documentation and to understand how the program, works.


-module(cell).
-include_lib("eunit/include/eunit.hrl").
-define(DEFAULT_FEREMONE_INCREASE,1.0).
-define(DEFAULT_FEREMONE_DECAY,1.01).

%% ====================================================================
%% API functions
%% ====================================================================
-export([spawnCell/1]).


%%  @doc Trivial and boring function used to start the cell.
%%  The cell will jump into its await linkup state before it enteres its main loop.
%%  In this state no other messages except pings and linkups make sense.
-spec spawnCell({X::integer(),Y::integer()}) -> ok.
spawnCell({X,Y}) -> 
    Cell = {self(),{X,Y},none,none,none,none,logger:makeLog(logger:makeCoolString("Cell(~p)",[{X,Y}]),self())},
	logger:logEvent(utils:getCellLog(Cell),"Cell spawned."),
     awaitLinkup(Cell).  




%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc State function for awating linkup. Accepts ping messages. Goes into cellMain when linkup is complete
-spec awaitLinkup(Cell::types:cell()) -> ok.
awaitLinkup(Cell) ->
    logger:logEvent(utils:getCellLog(Cell), "Awaiting linkup"),
    receive
        
        {Sender,Reference,ping} ->
            logger:logEvent(utils:getCellLog(Cell), "Received ping before initializtion"),
            Msg = Sender ! {self(),make_ref(),Reference,pong},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            awaitLinkup(Cell);
        
        Link = {Sender,Reference, Payload} ->
            logger:logMessage(utils:getCellLog(Cell),Link),
            case Payload of 
                {linkup,Hood,Next} ->
                    
                    C2 = utils:setCellHood(Cell,Hood),
                    C3 = utils:setCellNext(C2,Next),
                    C4 = utils:setCellMetadata(C3,{0,[]}),
                    Map = #{type => plain,food => 0 ,feremones => #{base_feremone => {0.0,?DEFAULT_FEREMONE_DECAY},food_feremone=> {0.0,?DEFAULT_FEREMONE_DECAY}}},
					C5 = utils:setCellAttributes(C4,Map),
                    logger:logEvent(utils:getCellLog(Cell),"Cell initialization complete!"),
                    Msg = Sender ! {self(),make_ref(),Reference,{reply,linkup,sucsess}},
                    logger:logMessageSent(utils:getCellLog(C5),Msg,Sender),
                    cellMain(C5,getTimeStamp());
                _A ->
                    ?debugFmt("Cell received wierd Payload(~p).Chrashing the system.~n",[Payload]),
                    timer:sleep(100),
                    logger:logWarning(utils:getCellLog(Cell),"Cell recieved mallformed linkupmessage. Crashing system~n"),
                    exit(failure)
            end;
        
        
        
        All= _ ->
            ?debugFmt("Cell received wierd message while awaiting linkup(~p).Chrashing the system.~n",[All]),
            
            logger:logWarning(utils:getCellLog(Cell),"Cell recieved wrong message while waiting linkup. Crashing system~n"),
            timer:sleep(100),
            exit(failure)

    end.

%% @doc Main state function. This is the "idle" state where the cell awaits a request or a one way message
-spec cellMain(Cell::types:cell(),Time_Stamp::float()) -> ok.
cellMain(In_Cell,Time_Stamp) ->
	logger:logEvent(utils:getCellLog(In_Cell),"Enterd main loop"),
	{Message,New_Buffer} = message_buffer:receiver(utils:getCellMetadata(In_Cell)),
	Cell = utils:setCellMetadata(In_Cell,New_Buffer),
	logger:logMessage(utils:getCellLog(Cell),Message),
	case Message of
		
        {_,Payload} ->
            logger:logEvent(utils:getCellLog(Cell),"Received oneway message"),
            {New_Cell,New_Time_Stamp} = handleOneWayMessage(Cell, Payload,Time_Stamp),
            logger:logEvent(utils:getCellLog(New_Cell),"Finished with one way message"),
            cellMain(New_Cell,New_Time_Stamp);
        
		Request = {_,_,_} ->
            %logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Printing pointless crap ~p", [getTimeStamp()-Time_Stamp])),
            {Updated_Cell,New_Time_Stamp} = automaticUpdate(Cell,(getTimeStamp()-Time_Stamp)/1000000),
            %{Updated_Cell,New_Time_Stamp} = {Cell,Time_Stamp},
			logger:logEvent(utils:getCellLog(Updated_Cell),"Got request message"),
			New_Cell = handleRequest(Updated_Cell,Request),
			logger:logEvent(utils:getCellLog(New_Cell),"Completed request"),
			cellMain(New_Cell,New_Time_Stamp);
  
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless message! Crashing system"),
			?debugFmt("Cell(~p) Received pointless message ~p ~n Crashing system",[self(),_Any]),
			exit(failure)
    end.
	

%% @doc Transition function for handling one way messages.
-spec handleOneWayMessage(Cell::types:cell(), Payload::types:one_way_type(),Time_Stamp::float()) -> {New_Cell::types:cell(),New_Time_Stam::float()}.
handleOneWayMessage(Cell, Payload,Time_Stamp) ->
    case Payload of
        dump ->
            logger:logEvent(utils:getCellLog(Cell),"Got dump message. Dumping everything....."),
            dump(Cell),
            {Cell,Time_Stamp};
        
        {draw,Gui_Pid} ->
            logger:logEvent(utils:getCellLog(Cell),"Received draw message. Relaying state to GUI process."),
            {Updated_Cell,New_Time_Stamp} = automaticUpdate(Cell,(getTimeStamp()-Time_Stamp)/1000000),
            Msg = Gui_Pid ! {self(),{gui_update,utils:getCellPos(Updated_Cell),utils:getCellAttributes(Updated_Cell)}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Gui_Pid),
            {Updated_Cell,New_Time_Stamp};
        
        _Any ->     
            logger:logWarning(utils:getCellLog(Cell),"Received pointless one way message! Crashing system"),
            ?debugFmt("Cell(~p) Received pointless message ~p ~n Crashing system",[self(),_Any]),
            exit(failure)
    end.
            
    

%% @doc Transition function for handling one way messages.
-spec handleRequest(Cell::types:cell(), {Snder::pid(),Reference::reference(),Payload::types:request_type()}) -> types:cell().
handleRequest(Cell,{Sender,Reference,Payload}) ->
	case Payload of
		query_state ->
			logger:logEvent(utils:getCellLog(Cell),"Received state querry"),
			Msg = Sender ! {self(), make_ref(),Reference,{reply,query_state,utils:getCellAttributes(Cell)}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            logger:logEvent(utils:getCellLog(Cell),"Processed state querry"),
			Cell;
        query_hood ->
            logger:logEvent(utils:getCellLog(Cell),"Received hood querry"),
            hoodQuerry(Cell, Sender, Reference),
            logger:logEvent(utils:getCellLog(Cell),"Processed hood querry"),
            Cell;
        ping ->
            logger:logEvent(utils:getCellLog(Cell),"Received ping"),
            Msg = Sender ! {self(), make_ref(),Reference,pong},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            logger:logEvent(utils:getCellLog(Cell),"Replied with a pong"),
            Cell;
        {place_ant,Ant} ->
            logger:logEvent(utils:getCellLog(Cell),"Recevied place ant request"),
            New_Cell = placeAnt(Cell,Sender,Reference,Ant),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed place ant request"),
            New_Cell;
        
        {move_ant,Direction} ->
            logger:logEvent(utils:getCellLog(Cell),"Recevied move ant request"),
            New_Cell = moveAnt(Cell,Sender,Reference,Direction),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed move ant request"),
            New_Cell;
		{deposit_feremone,Feremone_Name} ->
			logger:logEvent(utils:getCellLog(Cell),"Recevied deposit feremone request"),
            New_Cell = depositFeremone(Cell,Sender,Reference,Feremone_Name),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed deposit feremone request"),
            New_Cell;
		{set_cell_attribute,Attribute} ->
			logger:logEvent(utils:getCellLog(Cell),"Recevied set cell attributerequest"),
            New_Cell = setAttribute(Cell,Sender,Reference,Attribute),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed set attribute request"),
            New_Cell;
        take_food ->
            logger:logEvent(utils:getCellLog(Cell),"Recevied take food request"),
            New_Cell = takeFood(Cell,Sender,Reference),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed take food request"),
            New_Cell;
            
            
		_Any ->
			logger:logWarning(utils:getCellLog(Cell),"Received pointless Request! Crashing system"),
			?debugFmt("Cell(~p) Received pointless request ~p ~n Crashing system",[self(),_Any]),
            timer:sleep(100),
			exit(failure)
			
						
	end.


%% @doc Dunction corresponding to take food state. Will remove one unit of food if any food exists
-spec takeFood(Cell::types:cell(),Sender::pid(),Refernce::reference()) -> types:cell().
takeFood(Cell,Sender,Reference) ->
    logger:logEvent(utils:getCellLog(Cell),"Attempting to take food"),
    Map = utils:getCellAttributes(Cell),
    Food_Amount = maps:get(food,Map),
    case Food_Amount of
        0 ->
            logger:logEvent(utils:getCellLog(Cell),"Cell contains no food."),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,take_food,fail}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            Cell;
        
        _ ->
            logger:logEvent(utils:getCellLog(Cell),"Ant stole some food!."),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,take_food,sucsess}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            New_Food = Food_Amount -1,
            New_Map = maps:put(food,New_Food,Map),
            New_Cell = utils:setCellAttributes(Cell,New_Map),
            New_Cell
    end.


%% @doc Function corresponding to set attribute. WARNING!!! Can set undefined attributes of doom!
-spec setAttribute(Cell::types:cell(),Sender::pid(),Refernce::reference(), Attribute::types:cell_attributes()) -> types:cell().
setAttribute(Cell,Sender,Reference,{Type,Value}) ->
	logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Attempting to set attribute ~p to ~p.",[Type,Value])),
	Map = utils:getCellAttributes(Cell),
	New_Map = maps:put(Type,Value,Map),
	New_Cell = utils:setCellAttributes(Cell,New_Map),
	Msg = Sender ! {self(),make_ref(),Reference,{reply,set_cell_attribute,sucsess}},
	logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
	New_Cell.

%% @doc Function corresponding to deposit feremones. Increases feremone by the default increase rate
-spec depositFeremone(Cell::types:cell(),Sender::pid(),Refernce::reference(), Feremone_Name::types:feremone_name()) -> types:cell().
depositFeremone(Cell,Sender,Reference,Feremone_Name) ->
	logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Attempting to increase feremone ~p",[Feremone_Name])),
	Map = utils:getCellAttributes(Cell),
	Old_Feremone_Map = maps:get(feremones,Map),
	{Old_Strength,Dissipation_Rate} = maps:get(Feremone_Name,Old_Feremone_Map),
  	New_Strength = Old_Strength + ?DEFAULT_FEREMONE_INCREASE,
	New_Feremone_Map = maps:put(Feremone_Name,{New_Strength,Dissipation_Rate},Old_Feremone_Map),
	New_Map = maps:put(feremones,New_Feremone_Map,Map),
	New_Cell = utils:setCellAttributes(Cell,New_Map),
	Msg = Sender ! {self(),make_ref(),Reference,{reply,deposit_feremone,sucsess}},
	logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
	logger:logEvent(utils:getCellLog(Cell),"Feremone deposited."),

	New_Cell.
	

%% @doc Cell corresponding to the move ant state.
-spec moveAnt(Cell::types:cell(),Sender::pid(),Refernce::reference(), Direction::types:direction()) -> types:cell().
moveAnt(Cell,Sender,Reference,Direction) ->
    Ant = maps:get(ant,utils:getCellAttributes(Cell),none),
    if 
        Ant == none -> 
            logger:logEvent(utils:getCellLog(Cell),"Cell has no ant!"),
            Msg = Sender ! {self(), make_ref(), Reference,{reply,move_ant,fail}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            Cell;
        Ant /= Sender ->
            logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("It's the wrong ant! Cell has ant ~p  but got request from ant ~p ", [Ant, Sender])),
            Msg = Sender ! {self(), make_ref(), Reference,{reply,move_ant,fail}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            Cell;
        
        true ->
            logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Atempting to move ant to the ~p", [Direction])),
            Destination = utils:getOneDirection(Cell,Direction),
            case Destination of
                none -> 
                    logger:logEvent(utils:getCellLog(Cell),"Cant move ant to empty cell"),
                    Msg = Sender ! {self(), make_ref(), Reference,{reply,move_ant,fail}},
                    logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
                    Cell;
                _ ->
                    %% Await move reply state
                    New_Ref = make_ref(),
                    Msg = Destination ! {self(),New_Ref,{place_ant,Ant}},
					logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
                    {Message, New_Buffer} = message_buffer:receiver(New_Ref,Destination,utils:getCellMetadata(Cell)),					
                    New_Cell0 = utils:setCellMetadata(Cell,New_Buffer),
                    logger:logMessage(utils:getCellLog(Cell),Message),
        
                    case Message of
                        {_,_,_,{reply,place_ant,fail}} ->
                            logger:logEvent(utils:getCellLog(Cell),"Ant was not alowed to move to cell"),
                            Msg3 = Sender ! {self(), make_ref(), Reference,{reply,move_ant,fail}},
                            logger:logMessageSent(utils:getCellLog(Cell),Msg3,Sender),
                            New_Cell0;
                        {_,_,_,{reply,place_ant,sucsess}} ->
                            logger:logEvent(utils:getCellLog(Cell),"Ant was not allowed to move!"),
                            New_Map = maps:put(ant,none,utils:getCellAttributes(Cell)),
                            New_Cell1 = utils:setCellAttributes(New_Cell0,New_Map),
                            Msg1 = Sender ! {self(), make_ref(), Reference,{reply,move_ant,{sucsess,Destination}}},
                            logger:logMessageSent(utils:getCellLog(Cell),Msg1,Sender),
                            New_Cell1
                    end
            end
    end.

    
    
%% @doc Corresponds to the place ant state
-spec placeAnt(Cell::types:cell(),Sender::pid(),Reference::reference(),Ant::pid()) -> types:cell().
placeAnt(Cell,Sender,Reference,New_Ant) ->
    logger:logEvent(utils:getCellLog(Cell),"Attempting to place ant"),
    Ant = maps:get(ant,utils:getCellAttributes(Cell),none),
    Type = maps:get(type,utils:getCellAttributes(Cell),none),
    case {Ant,Type} of
        {_,block} ->
            logger:logEvent(utils:getCellLog(Cell),"Could not place ant. Cell is block"),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,place_ant,fail}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            Cell;
        {none,_} ->
            logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Placing ant ~p", [New_Ant])),
            New_Map = maps:put(ant,New_Ant,utils:getCellAttributes(Cell)),
            New_Cell = utils:setCellAttributes(Cell,New_Map),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,place_ant,sucsess}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            New_Cell;
        {_,_} ->
            logger:logEvent(utils:getCellLog(Cell),logger:makeCoolString("Could not place ant since ant is ~p.", [Ant])),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,place_ant,fail}},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
            Cell
    end.

            

            
%% @doc Corresponds to the query hood state.
%%  Will brodcast querry_state messages to all other cells in the neighbouthood.
-spec hoodQuerry(Cell::types:cell(),Sender::pid(), Reference::reference()) -> types:cell().
hoodQuerry(Cell,Sender,Reference) ->
    logger:logEvent(utils:getCellLog(Cell), "Processing hood request"),
    {Attributes,Refs} = buildHoodThing(Cell,tuple_to_list(utils:getCellHood(Cell)),[],[]),
    {New_Cell, Filled_Attributes} = hoodQuerryAux(Refs, Cell, Attributes),
    logger:logEvent(utils:getCellLog(Cell), "Recieved complete hood"),
    Msg = Sender! {self(),make_ref(),Reference,{reply,query_hood,list_to_tuple(Filled_Attributes)}},
    logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
    New_Cell.


%% @doc Corresponds to the await query hood replies.
%% Will wait for the replies to the requests sent to all the neighbouring cells.
-spec hoodQuerryAux([reference()],Cell::types:cell(),Attributes::types:cell_atributes()) -> {Cell::types:cell(),[types:cel_attributes()]}.
hoodQuerryAux([],Cell,Attributes) ->
    {Cell,element(2,lists:unzip(Attributes))};

hoodQuerryAux(Refs,In_Cell,Attributes) when is_list(Refs)->  
    {Message,New_Buffer,New_Refs} = message_buffer:receiver(Refs,element(1,lists:unzip(Attributes)),utils:getCellMetadata(In_Cell)),
    Cell0 = utils:setCellMetadata(In_Cell,New_Buffer),
    logger:logMessage(utils:getCellLog(Cell0),Message),
    
    case Message of
        {_, _,Key_Reference,{reply,query_state,New_Attribute}} ->
            hoodQuerryAux(New_Refs, Cell0, findAndReplace(Key_Reference,New_Attribute,Attributes,[]));
    
        _Any ->
            logger:logWarning(utils:getCellLog(Cell0),"Received pointless message Whilst in hoodquerry awaiting state! Crashing system"),
            ?debugFmt("Cell(~p) Received pointless message ~p whilst waiting for hood replies.~n Crashing system",[self(),_Any]),
            exit(failure)
    end.
    
%% @doc Function used for automatic decay of feremones and other types of time dependent updates.
-spec automaticUpdate(Cell::types:cell(),Seconds_Diff::float()) -> {New_Cell::types:cell(),New_Time::integer()} .
automaticUpdate(Cell,Seconds_Diff) ->
    logger:logEvent(utils:getCellLog(Cell), logger:makeCoolString("Performing automatic decay with a time diff of ~p seconds",[Seconds_Diff])),
    Map = utils:getCellAttributes(Cell),
    Feremones = maps:get(feremones,Map),
    
    {Base_Strength,Base_Decay} = maps:get(base_feremone,Feremones),
    New_Base_Strength = Base_Strength*(1/(math:pow(Base_Decay,Seconds_Diff))),
    
    {Food_Strength,Food_Decay} = maps:get(food_feremone,Feremones),
    New_Food_Strength = Food_Strength*(1/(math:pow(Food_Decay,Seconds_Diff))),
    
    New_Feremones1 = maps:put(base_feremone,{New_Base_Strength,Base_Decay},Feremones),
    New_Feremones2 = maps:put(food_feremone,{New_Food_Strength,Food_Decay},New_Feremones1),
    
    New_Map = maps:put(feremones,New_Feremones2,Map),
    New_Cell = utils:setCellAttributes(Cell,New_Map),
    logger:logEvent(utils:getCellLog(Cell), logger:makeCoolString("Performed decay. New base feremone strength is ~p and food strength is ~p.",[New_Base_Strength,New_Food_Strength])),
    {New_Cell,getTimeStamp()}.
    

%% ====================================================================
%% Auxfunction. Not documented
%% ====================================================================

getTimeStamp() ->
    {MegaSecs,Secs,MicroSecs} = erlang:now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.


dump({Pid,Position,Hood,Next_Cell, Attributes,{Length,Buffer},_}) ->
    S0 = "~n=======================================~nDUMP OF CELL ~p(Connected to ~p) at coordinates ~p~n",
    S1 = "NEIGHBOURHOOD = ~n~p~n",
    S2 = "ATTRIBUTES = ~n~p~n",
    S3 = "MESSAGE BUFFER (~p waiting messages) = ~n~p~n",
    S4 = "=======================================~n",
    Big_String = string:join([S0,S1,S2,S3,S4],""),
    Args = [Pid,Next_Cell,Position,Hood,Attributes,Length,Buffer],
    %logger:logEvent(Log, logger:makeCoolString(Big_String, Args)),
    ?debugFmt(Big_String,Args).
    %logger:logEvent(Log,re:replace(logger:makeCoolString("~ts~ts~ts~ts~ts",[S0,S1,S2,S3,S4])),"\\","~").

findAndReplace(_,_,[],Acc) ->
    lists:reverse(Acc);
findAndReplace(Reference,Val,[Hd|Tl],Acc)->
    if
        Reference == element(1, Hd) ->
            findAndReplace(Reference, Val, Tl, [{Reference,Val}]++Acc);
        true ->
            findAndReplace(Reference, Val, Tl, [Hd]++Acc)
    end.
    

buildHoodThing(_,[],CoolList,Refs) ->
    
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

buildTestWorld() ->
    logger:initLogger(),
        Hood = {
            spawn(fun () -> spawnCell({-1,1}) end),spawn(fun () -> spawnCell({0,1}) end),spawn(fun () -> spawnCell({1,1}) end),
            spawn(fun () -> spawnCell({-1,0}) end),spawn(fun () -> spawnCell({0,0}) end),spawn(fun () -> spawnCell({1,0}) end),
            spawn(fun () -> spawnCell({-1,-1}) end),spawn(fun () -> spawnCell({0,-1}) end),spawn(fun () -> spawnCell({1,-1}) end)
           },
    Center_Cell = element(5,Hood),
    Next_Cell = element(6,Hood),
    Ref = make_ref(),
    Center_Cell ! {self(), Ref,{linkup,Hood,Next_Cell}},
    receive
        _ ->
            ok
    end,
    My_Pid = self(),
    lists:map(fun(X) -> X ! {My_Pid, Ref,{linkup,none,none}} end, [
                                                               element(1,Hood),element(2,Hood),element(3,Hood),
                                                               element(4,Hood),                element(6,Hood),
                                                               element(7,Hood),element(8,Hood),element(9,Hood)
                                                              ]),
    ignoreMessages(8),
    {Center_Cell,Next_Cell,Hood}.

spawnTest() ->
    %?debugMsg("Testing Spawner"),
    logger:initLogger(),
    CellPid = spawn(fun () -> spawnCell({0,0})end),
    %?debugMsg("Spawned cell"),
    Reference = make_ref(),
    CellPid ! {self(),Reference,ping},
    %?debugMsg("Sent message"),
    receive
        {_,_,New_Ref,pong} ->
            %?debugMsg("Received reply message"),
            ?assertEqual(New_Ref,Reference),
            exit(CellPid,sucsess),
            %?debugMsg("Slept and returning"),
            true;
        
        _ ->
            %?debugMsg("Received wrong message whilst testing"),
            exit(CellPid,failure),
            false
    
    after 1000 ->
        %?debugMsg("Ping timed out"),
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
    Reference = make_ref(),
    Center_Cell ! {self(),Reference,{linkup,Hood,Next_Cell}},
    
    {Msg,_} = message_buffer:receiver(Reference,Center_Cell,{0,[]}),
    case Msg of
        {Pid,_,Key_Ref,{reply,linkup,sucsess}} when Pid == Center_Cell, Key_Ref == Reference ->
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            true;
        _->
            ?debugMsg("Received maformed message,Fail"),
            false    
    end.

queryHoodTest() ->
	{Center_Cell,_,Hood} = buildTestWorld(),
    
    Reference = make_ref(),
    Center_Cell ! {self(),Reference,query_hood},
    receive
        {Pid,_,Key_Reference,{reply,query_hood,_}} when Pid == Center_Cell, Key_Reference == Reference ->
            %?debugFmt("Received reqply message  ~p ~n",[Message]),
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            true;
        _A ->
            ?debugFmt("Received wrong message  ~p ~nWas Expecting reference ~p from ~p ~n ",[_A,Reference,Center_Cell]),
            false
    end.

dumbAnt() ->
    dumbAnt().
    

placeAntTest()->
    {Center_Cell,_,Hood} = buildTestWorld(),
    Ant = spawn(fun() -> dumbAnt() end),
    
    Message = sendAndReceive(Center_Cell, {place_ant,Ant}),
    case Message of
        {_,_,_,{reply,place_ant,sucsess}}->
            %lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            %exit(Ant,succsess),
            true;
        _A ->
            ?debugFmt("Received wrong message  ~p ~n ",[_A]),
            false
    end,
    
    Ant2 = spawn(fun() -> dumbAnt() end),
    
    Message1 = sendAndReceive(Center_Cell, {place_ant,Ant2}),
     case Message1 of
        {_,_,_,{reply,place_ant,fail}} ->
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            exit(Ant2,succsess),
            true;
        _AA ->
            ?debugFmt("Received wrong message  ~p ~n ",[_AA]),
            false
    end.
            
coolAnt(Mainy) ->
    receive
        _A ->
            Mainy ! _A
    end,
    coolAnt(Mainy).
                     
moveAntTest() ->
    {Center_Cell,Next,_} = buildTestWorld(),
    My_Pid = self(),
    Ant1 = spawn(fun() -> coolAnt(My_Pid) end),
    Ant2 = spawn(fun() -> coolAnt(My_Pid) end),
    
    
    %place first ant in the center
    Place_Ref1 = make_ref(), 
    Center_Cell ! {Ant1,Place_Ref1,{place_ant,Ant1}},
    {Message,_} = message_buffer:receiver(Place_Ref1,Center_Cell,{0,[]}),
    case Message of
        {_,_,_,{reply,place_ant,sucsess}} ->
            ok;
        _A0 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A0,Place_Ref1,Ant1]),
            ?assert(false)
    end,
    
    
    %move ant to the east
    Move_Ref1 = make_ref(), 
    Center_Cell ! {Ant1,Move_Ref1,{move_ant,east}},
    {Message1,_} = message_buffer:receiver(Move_Ref1,Center_Cell,{0,[]}),
    case Message1 of
        {_,_,_,{reply,move_ant,{sucsess,Pid}}} when Pid == Next->
            ok;
        _A1 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A1,Move_Ref1,Ant1]),
            ?assert(false)
    end,

    
    %place second ant in the center
    Place_Ref2 = make_ref(), 
    Center_Cell ! {Ant2,Place_Ref2,{place_ant,Ant2}},
    {Message2,_} = message_buffer:receiver(Place_Ref2,Center_Cell,{0,[]}),
    case Message2 of
        {_,_,_,{reply,place_ant,sucsess}} ->
            ok;
        _A2 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A2,Place_Ref2,Ant2]),
            ?assert(false)
    end,
    
    %try to move second ant to the east. which should not be allowed.
    Move_Ref2 = make_ref(), 
    Center_Cell ! {Ant2,Move_Ref2,{move_ant,east}},
    {Message3,_} = message_buffer:receiver(Move_Ref2,Center_Cell,{0,[]}),
    case Message3 of
        {_,_,_,{reply,move_ant,fail}} ->
            ok;
        _A3 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A3,Move_Ref2,Ant2]),
            ?assert(false)
    end,
    
    %try to move second ant "from" the east wich should not be allowed due to wrong ant
    Move_Ref3 = make_ref(), 
    Next ! {Ant2,Move_Ref3,{move_ant,east}},
    {Message4,_} = message_buffer:receiver(Move_Ref3,Next,{0,[]}),
    case Message4 of
        {_,_,_,{reply,move_ant,fail}} ->
            ok;
        _A4 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A4,Move_Ref3,Ant2]),
            ?assert(false)
    end,

    true.
    
depositFeremoneTest() ->
	{Center_Cell,_,_} = buildTestWorld(),

	Reference = make_ref(),
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
    
    %timer:sleep(200),
	%Center_Cell ! {123,dump},
    
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	
    %Center_Cell ! {123,dump},
    
	ignoreMessages(8),

	Message = sendAndReceive(Center_Cell, query_state),
	case Message of
		{_,_,_,{reply,query_state,Attributes}} ->
			Feremone_Map = maps:get(feremones,Attributes),
			{Base_Strength,_} = maps:get(base_feremone,Feremone_Map),
			?assert(Base_Strength > 3),
			{Food_Strength,_} = maps:get(food_feremone,Feremone_Map),
			?assert(Food_Strength > 3)
	end,
	
	true.

setAttributeTest() ->
	{Center_Cell,_,_} = buildTestWorld(),
	
	Message = sendAndReceive(Center_Cell, {set_cell_attribute,{type,nest}}),
	case Message of
		{_,_,_,{reply,set_cell_attribute,sucsess}} ->
			ok;
		_ ->
			?assert(false)
	end,
    
    Message1 = sendAndReceive(Center_Cell, query_state),
	case Message1 of
		{_,_,_,{reply,query_state,Attributes}} ->
			Type = maps:get(type,Attributes,none),
			?assertEqual(Type,nest)
	end,
	
	
	%Center_Cell ! {self(),dump},
	%timer:sleep(50),
	true.
	

takeFoodTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Ref = make_ref(),
    
    Center_Cell ! {self(),Ref,{set_cell_attribute,{food,10}}},
    ignoreMessages(1),

    Message1 = sendAndReceive(Center_Cell, take_food),
    case Message1 of
        {_,_,_,{reply,take_food,sucsess}} ->
            ok;
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message1]),
            ?assert(fail)
    end,
    
    Message2 = sendAndReceive(Center_Cell, query_state),
    case Message2 of
        {_,_,_,{reply,query_state,Attributes}} ->
            Amount = maps:get(food,Attributes,none),
            ?assertEqual(Amount,9);
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message2]),
            ?assert(fail)
    end,
    
    Center_Cell ! {self(),Ref,{set_cell_attribute,{food,0}}},
    ignoreMessages(1),
    
    Message3 = sendAndReceive(Center_Cell, take_food),
    case Message3 of
        {_,_,_,{reply,take_food,fail}} ->
            ok;
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message3]),
            ?assert(fail)
    end,
    
    Message4 = sendAndReceive(Center_Cell, query_state),
    case Message4 of
        {_,_,_,{reply,query_state,Attributes1}} ->
            Amount1 = maps:get(food,Attributes1,none),
            ?assertEqual(Amount1,0);
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message4]),
            ?assert(fail)
    end,

    true.
	
automaticDecayTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Ref = make_ref(),
    
    Center_Cell ! {self(),Ref,{deposit_feremone,base_feremone}},
    Center_Cell ! {self(),Ref,{deposit_feremone,food_feremone}},
    Center_Cell ! {self(),Ref,ping},
    Center_Cell ! {self(),Ref,ping},
    ignoreMessages(4),
    
    Message = sendAndReceive(Center_Cell, query_state),
    case Message of
        {_,_,_,{reply,query_state,Attributes}} ->
            Feremone_Map = maps:get(feremones,Attributes),
            {Base_Strength,_} = maps:get(base_feremone,Feremone_Map),
            ?assert(Base_Strength < 1.0),
            ?assert(Base_Strength > 0.5),
            {Food_Strength,_} = maps:get(food_feremone,Feremone_Map),
            ?assert(Food_Strength < 1.0),
            ?assert(Food_Strength > 0.5)
    end,
    
    true.

drawTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Center_Cell ! {self(),{draw,self()}},
    
    receive
        {_,{gui_update,Position,_}} ->
            ?assertEqual({0,0},Position);
        _ ->
            ?assert(false)
    end,
    true.
            

sendAndReceive(Destination,Message)->
    Ref = make_ref(),
    Destination ! {self(),Ref,Message},
    {Received,_} = message_buffer:receiver(Ref,Destination,{0,[]}),
    Received.

ignoreMessages(0) ->
    ok;
ignoreMessages(N) ->
    receive
        _ ->
            ignoreMessages(N-1)
    end.
    

spawnTest_test() ->
    [?assert(spawnTest())].

linkupTest_test() ->
    [?assert(linkupTest())].

queryHood_test()->
    [?assert(queryHoodTest())].

testPlaceAnt_test()->
    [?assert(placeAntTest())].

testMoveAnt_test()->
    [?assert(moveAntTest())].

depositFeremone_test()->
    [?assert(depositFeremoneTest())].

setAttribute_test()->
    [?assert(setAttributeTest())].

takeFood_test()->
    [?assert(takeFoodTest())].

automaticDecay_test()->
    [?assert(automaticDecayTest())].

draw_test()->
    [?assert(drawTest())].





