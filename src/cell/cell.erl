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
                    Map = #{
							type => plain,
							food => 0 ,
							feremones => #{
										   base_feremone => {0.0,?DEFAULT_FEREMONE_DECAY},
										   food_feremone=> {0.0,?DEFAULT_FEREMONE_DECAY}},
							gui_module => none,
							ant => none,
							ant_state => idling
						   },
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
            Msg = Gui_Pid ! {self(),{gui_update,{utils:getCellPos(Updated_Cell),utils:getCellAttributes(Updated_Cell)}}},
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
			sendGuiUpdate(New_Cell),
            New_Cell;
        
        {move_ant,Direction} ->
            logger:logEvent(utils:getCellLog(Cell),"Recevied move ant request"),
            New_Cell = moveAnt(Cell,Sender,Reference,Direction),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed move ant request"),
			sendGuiUpdate(New_Cell),
            New_Cell;
		{deposit_feremone,Feremone_Name} ->
			logger:logEvent(utils:getCellLog(Cell),"Recevied deposit feremone request"),
            New_Cell = depositFeremone(Cell,Sender,Reference,Feremone_Name),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed deposit feremone request"),
			sendGuiUpdate(New_Cell),
            New_Cell;
		{set_cell_attribute,Attribute} ->
			logger:logEvent(utils:getCellLog(Cell),"Recevied set cell attributerequest"),
            New_Cell = setAttribute(Cell,Sender,Reference,Attribute),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed set attribute request"),
			sendGuiUpdate(New_Cell),
            New_Cell;
        take_food ->
            logger:logEvent(utils:getCellLog(Cell),"Recevied take food request"),
            New_Cell = takeFood(Cell,Sender,Reference),
            logger:logEvent(utils:getCellLog(New_Cell),"Processed take food request"),
			sendGuiUpdate(New_Cell),
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
moveAnt(Cell,Sender,Reference,{Direction,State}) ->
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
                    Msg = Destination ! {self(),New_Ref,{place_ant,{Ant,State}}},
					logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender),
                    {Message, New_Buffer} = message_buffer:receiver(New_Ref,Destination,utils:getCellMetadata(Cell),"Trying to move ant"),					
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
placeAnt(Cell,Sender,Reference,{New_Ant,State}) ->
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
			New_Map1 = maps:put(ant_state,State,New_Map),
            New_Cell = utils:setCellAttributes(Cell,New_Map1),
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
    {Attributes,Refs,Pids} = buildHoodThing(Cell,tuple_to_list(utils:getCellHood(Cell)),[],[],[]),
    {New_Cell, Filled_Attributes} = hoodQuerryAux(Refs, Cell, Attributes,Pids),
    case Filled_Attributes of
		fail ->
			Msg1 = Sender! {self(),make_ref(),Reference,{reply,query_hood,fail}},
   			logger:logMessageSent(utils:getCellLog(Cell),Msg1,Sender);
		_Any ->			
		    Msg = Sender! {self(),make_ref(),Reference,{reply,query_hood,list_to_tuple(Filled_Attributes)}},
		    logger:logMessageSent(utils:getCellLog(Cell),Msg,Sender)
	end,
    New_Cell.


%% @doc Corresponds to the await query hood replies.
%% Will wait for the replies to the requests sent to all the neighbouring cells.
-spec hoodQuerryAux([reference()],Cell::types:cell(),Attributes::types:cell_atributes(),Pids::[pid()]) -> {Cell::types:cell(),[types:cel_attributes()]}.
hoodQuerryAux([],Cell,Attributes,_) ->
    {Cell,element(2,lists:unzip(Attributes))};

hoodQuerryAux(Refs,In_Cell,Attributes,Pids) when is_list(Refs)->  
    {Message,New_Buffer,New_Refs} = message_buffer:receiver(Refs,Pids,utils:getCellMetadata(In_Cell),"Awaiting hood replies"),
    Cell0 = utils:setCellMetadata(In_Cell,New_Buffer),
    logger:logMessage(utils:getCellLog(Cell0),Message),
    
    case Message of
    	{_, _,Key_Reference,{reply,query_state,fail}} ->
			logger:logWarning(utils:getCellLog(Cell0),"DEADLOCK IN HOOD QUERRY!!!!!!"),

            %Uncomment these lines to se a major performance degradation :)
            %Flushed_Buffer = hoodQuerryRollback(New_Buffer, Pids, New_Refs),
    	    %{utils:setCellMetadata(In_Cell,Flushed_Buffer),fail};
            hoodQuerryAux(New_Refs, Cell0, findAndReplace(Key_Reference,none,Attributes,[]),Pids);
		
		{_, _,Key_Reference,{reply,query_state,New_Attribute}} ->
            hoodQuerryAux(New_Refs, Cell0, findAndReplace(Key_Reference,New_Attribute,Attributes,[]),Pids);
		
	
        _Any ->
            logger:logWarning(utils:getCellLog(Cell0),"Received pointless message Whilst in hoodquerry awaiting state! Crashing system"),
            ?debugFmt("Cell(~p) Received pointless message ~p whilst waiting for hood replies.~n Crashing system",[self(),_Any]),
            exit(failure)
    end.

%hoodQuerryRollback(Buffer,_,[]) ->
	%?debugMsg("Performed rollback"),
%	Buffer;
	
%hoodQuerryRollback(Buffer,Pids,Refs) ->
%	{_,New_Buffer,New_Refs} = message_buffer:receiver(Refs,Pids,Buffer),
%	hoodQuerryRollback(New_Buffer,Pids,New_Refs).
	

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

sendGuiUpdate(Cell) ->
	Gui_Pid = maps:get(gui_module,utils:getCellAttributes(Cell)),
	case Gui_Pid of
		none ->
			ok;
		_ ->
			Msg = Gui_Pid ! {self(),{gui_update,{utils:getCellPos(Cell),utils:getCellAttributes(Cell)}}},
			logger:logMessageSent(utils:getCellLog(Cell),Msg,Gui_Pid)
	end.

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
    ?debugFmt(Big_String,Args).

findAndReplace(_,_,[],Acc) ->
    lists:reverse(Acc);
findAndReplace(Reference,Val,[Hd|Tl],Acc)->
    if
        Reference == element(1, Hd) ->
            findAndReplace(Reference, Val, Tl, [{Reference,Val}]++Acc);
        true ->
            findAndReplace(Reference, Val, Tl, [Hd]++Acc)
    end.
    

buildHoodThing(_,[],CoolList,Refs,Pids) ->
    
    {lists:reverse(CoolList),Refs,Pids};

buildHoodThing(Cell,[none|Tl],CoolList,Refs,Pids) ->
    logger:logEvent(utils:getCellLog(Cell), "Skipping non existing cell"),
    buildHoodThing(Cell,Tl,[{none,none}]++CoolList,Refs,Pids);

buildHoodThing(Cell,[Hd|Tl],CoolList,Refs,Pids) ->
    if
        Hd == self() ->
            logger:logEvent(utils:getCellLog(Cell), "Skipping cell"),        
            buildHoodThing(Cell,Tl,[{none,utils:getCellAttributes(Cell)}]++CoolList,Refs,Pids);

        true->
            Ref = make_ref(),
            Msg = Hd ! {self(),Ref,query_state},
            logger:logMessageSent(utils:getCellLog(Cell),Msg,Hd),
            buildHoodThing(Cell,Tl,[{Ref,none}]++CoolList,[Ref]++Refs,[Hd]++Pids)
    end.

