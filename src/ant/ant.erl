%% @author Tanshinan
%% @todo Add further functionality of ant

-module(ant).

-export([spawn_Ant/2]).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_IDLE_TIME,50).
-define(SELECTION_PROBABILITY,0.5).

%% Attempts to spawn ant in given Cell
spawn_Ant(Cell_Pid, Attributes) ->
	spawn_link(fun() -> ant_Init(Cell_Pid, Attributes) end).


%% Creates needed information for ant, and places it in given cell
ant_Init(Cell_Pid, _) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	Ant_ID = self(),
	State = idling,
	Log = logger:makeLog(logger:makeCoolString("Ant ~p",[Ant_ID]),self()),
	Buffer = {0, []},  
	Ref = make_ref(),
	Msg = Cell_Pid ! {Ant_ID, Ref, {place_ant, Ant_ID}},
	{{_, _, _, Response}, New_Buffer} = message_buffer:receiver(Ref, Cell_Pid,Buffer),
	Ant = {self(), Cell_Pid, State, #{food => 0}, New_Buffer, Log},
	logger:logEvent(utils:getAntLog(Ant),"Ant spawned."),
	logger:logMessageSent(utils:getAntLog(Ant),Msg,Cell_Pid),
	case Response of
		{reply,place_ant, fail} ->
			logger:logEvent(utils:getAntLog(Ant),"Ant failed to get placed and is killed."),
			exit(fail);
		{reply,place_ant, sucsess} ->

			logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Ant placed in Cell ~p ", [Cell_Pid])),
            antMain(Ant);
		_ ->
			logger:logWarning(utils:getAntLog(Ant),"Ant recived malformed message whilst awaiting initial placement"),
			exit(fail)
	end.

%% Main loop of ant
antMain(In_Ant) ->
	%% Start with a nap
    logger:logEvent(utils:getAntLog(In_Ant),logger:makeCoolString("Ant antered main and is ~p",[utils:getAntState(In_Ant)])),
    Has_Messages = message_buffer:hasMessages(utils:getAntMetadata(In_Ant)),
    Is_Idle = utils:getAntState(In_Ant) == idling, 
    if  
        Has_Messages or Is_Idle ->
            {Message,New_Buffer} = message_buffer:receiver(utils:getAntMetadata(In_Ant)),
            logger:logMessage(utils:getAntLog(In_Ant), Message),
            Ant = utils:setAntMetadata(In_Ant,New_Buffer),
            case Message of
                {Sender,Reference,Payload} ->
                    logger:logEvent(utils:getAntLog(Ant),"Received request message"),
                    New_Ant = handleRequest(Ant,{Sender,Reference,Payload}),
                    logger:logEvent(utils:getAntLog(Ant),"Processed request message"),
                    antMain(New_Ant);
                {_,Payload} ->
                    logger:logEvent(utils:getAntLog(Ant),"Received oneway message"),
                    New_Ant = handleOneWayMessage(Ant, Payload),
                    logger:logEvent(utils:getAntLog(Ant),"Processed oneway message"),
                    antMain(New_Ant);
                
                _Any ->
                    logger:logWarning(utils:getAntLog(Ant),"Received pointless message in main loop! Crashing system :("),
                    ?debugFmt("Ant(~p) Received pointless message ~p ~n Crashing system",[self(),_Any]),
                    timer:sleep(100),
                    exit(failure)
            end;
                    
        true ->
            case utils:getAntState(In_Ant) of
                 
                searching_for_food->
                    logger:logEvent(utils:getAntLog(In_Ant),"Attempting to search for food."),
                    New_Ant = searchForFood(In_Ant),
                    logger:logEvent(utils:getAntLog(In_Ant),"Search atempt completed."),
                    antMain(New_Ant);
                
                returning_with_food->
                    logger:logEvent(utils:getAntLog(In_Ant),"Attempting return with food."),
                    New_Ant = returnWithFood(In_Ant),
                    logger:logEvent(utils:getAntLog(In_Ant),"Return Attempt Completed."),
                    antMain(New_Ant);
                
                _Any ->
                    logger:logWarning(utils:getAntLog(In_Ant),logger:makeCoolString("Ant is in ridcoulus state ~p. Lol...", [_Any])),
                    ?debugFmt("Ant(~p) is in silly state ~p",[self(),_Any]),
                    timer:sleep(100),
                    exit(failure)
            end
                    
                    
            
	end.

searchForFood(Ant)->
    searchForFood(Ant,examining_current_cell).

searchForFood(Ant,examining_current_cell) ->
Cell_Pid = utils:getAntCell(Ant),
    logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Examining its own cell ~p",[Cell_Pid])),
    {Message,New_Ant} = sendAndReceive(Ant,query_state),

    case Message of
        
        {reply,query_state,fail} ->
            logger:logEvent(utils:getAntLog(New_Ant),"Failed to retreive cell state."),
            New_Ant;
        
        {reply,query_state,Attributes} ->
            Food_Amount = maps:get(food,Attributes),
            case Food_Amount of
                0 ->
                    logger:logEvent(utils:getAntLog(New_Ant),"Cell contained no food. Continuing search"),
                    searchForFood(New_Ant,examine_hood);
                _ ->
                    logger:logEvent(utils:getAntLog(New_Ant),logger:makeCoolString("Cell contained ~p units of food. Attempting to snatch some.",[Food_Amount])),
                    searchForFood(New_Ant,snatch_food)
            end;


        _Any ->
            logger:logWarning(utils:getAntLog(New_Ant),"Received idiotic message whilst examining cell. Crashing system"),
            ?debugFmt("Ant(~p) got dumb message whilst examinig cell ~p",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
    end;

searchForFood(Ant,snatch_food) ->                                       
    Cell_Pid = utils:getAntCell(Ant),
    logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Trying to snatch food from ~p",[Cell_Pid])), 

    {Message,New_Ant} = sendAndReceive(Ant,take_food),
    case Message of
        {reply,take_food,sucsess} ->
            logger:logEvent(utils:getAntLog(New_Ant),"ANT SCORED FOOD! Returning in triumph :D"),
            Map = utils:getAntAttributes(New_Ant),
            New_Map = maps:put(food,1,Map),
            New_Ant1 = utils:setAntAttributes(Ant,New_Map),
            utils:setAntState(New_Ant1,returning_with_food);
        
        {reply,take_food,fail} ->
            logger:logEvent(utils:getAntLog(New_Ant),"Ant could not get the food :(. Continuing with search like a boss."),
            examineHoodAntTakeAction(New_Ant, food_feremone, base_feremone);
        
        _Any ->
            logger:logWarning(utils:getAntLog(New_Ant),"Received idiotic message whilst trying to pick up food. Crashing system"),
            ?debugFmt("Ant(~p) got dumb message whilst picking upp food ~p",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
    end.

examineHoodAntTakeAction(Ant,Search_Feremone,Drop_Feremone) ->
    Cell_Pid = utils:getAntCell(Ant),
    logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Examining the surroundings of ~p",[Cell_Pid])),
    {Message,New_Ant} = sendAndReceive(Ant,query_hood),
    case Message of
        {reply,query_hood,Hood} ->
            logger:logEvent(utils:getAntLog(New_Ant),"Ant took a look at the hood"),
            {Status,New_Ant} = contemplateHood(Ant, Hood, Search_Feremone),
            case Status of 
                sucsess ->
                    logger:logEvent(utils:getAntLog(New_Ant),"Ant depositing feremone at former cell"),
                    {Message2,New_Ant1} = sendAndReceive(Cell_Pid,{deposit_feremone,Drop_Feremone}),
                    case Message2 of
                        {reply,drop_feremone,sucsess} ->
                            logger:logEvent(utils:getAntLog(New_Ant),"Ant deposited feremone."),
                            New_Ant1;
                        {reply,drop_feremone,fail} ->
                            logger:logEvent(utils:getAntLog(New_Ant),"Ant could not deposit pheremone. Not that anyone cares though."),
                            New_Ant1;
                        _Any ->
                            logger:logWarning(utils:getAntLog(New_Ant1),"Received idiotic message whilst dropping feremone. Crashing system"),
                            ?debugFmt("Ant(~p) got dumb message whilst dropping feremone ~p",[self(),_Any]),
                            timer:sleep(100),
                            exit(failure)
                    end;
                fail ->
                    New_Ant
            end;

        {reply,query_hood,fail} ->
            logger:logEvent(utils:getAntLog(New_Ant),"Ant could not see its surroundings. Giving up search."),
            New_Ant;
        
        _Any ->
            logger:logWarning(utils:getAntLog(New_Ant),"Received idiotic message whilst examing hood. Crashing system"),
            ?debugFmt("Ant(~p) got dumb message whilst examing hood ~p",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
    end.
                                           

returnWithFood(Ant)->
    logger:logEvent(utils:getAntLog(Ant),"Return with food not implemented. Idling."),
    utils:setAntState(Ant,idling).

contemplateHood(Ant,Hood,Feremone) ->
    Cell_Pid = utils:getAntCell(Ant),
    logger:logEvent(utils:getAntLog(Ant),"Ant is contemplating the nature of 'The Hood'."),
    Sorted_Hood = processHood(Hood, Feremone),
    Direction = pickDirection(Sorted_Hood),
    logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("And sorted hood like so: ~p with regards to '~p' and decided to go to the ~p",[Sorted_Hood, Feremone, Direction])),
    {Message,New_Ant} = sendAndReceive(Ant, {move_ant,Direction}),
    case Message of
        {reply,move_ant,{sucsess,New_Pid}} ->
            logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Ant succseeded in moving to the ~p and is now at ~p",[Direction,New_Pid])),
            New_Ant1 = utils:setAntCell(New_Ant,New_Pid),
            {sucsess,New_Ant1};
            
        {reply,move_ant,fail} ->
            logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Ant ant failed to move to the ~p",[Direction])),
            {fail,New_Ant};
        
        _Any ->
            logger:logWarning(utils:getAntLog(New_Ant),"Received idiotic message whilst contemplating hood. Crashing system"),
            ?debugFmt("Ant(~p) got dumb message whilst contemplating hood ~p",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
        
    end,
    ok.
    

handleOneWayMessage(Ant,Payload) ->
    case Payload of
        dump ->
            logger:logEvent(utils:getAntLog(Ant),"Received dump message"),
            dump(Ant),
            Ant;
        
        start_ant ->
            logger:logEvent(utils:getAntLog(Ant),"Received start ant message. Setting state to 'searching_for_food' "),
            New_Ant = utils:setAntState(Ant,searching_for_food),
            New_Ant;
        
        stop_ant ->
            logger:logEvent(utils:getAntLog(Ant),"Received start ant message. Setting state to 'idling' "),
            New_Ant = utils:setAntState(Ant,idling),
            New_Ant;
        
        _Any ->
            logger:logWarning(utils:getAntLog(Ant),"Received pointless oneway message! Crashing system :("),
            ?debugFmt("Ant(~p) Received pointless one way message ~p ~n Crashing system",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
    end.
            

handleRequest(Ant,{Sender,Reference,Payload}) ->
    case Payload of
        query_state ->
            logger:logEvent(utils:getAntLog(Ant),"Received state querry"),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,state_query,utils:getAntAttributes(Ant)}},
            logger:logMessageSent(utils:getAntLog(Ant),Msg,Sender),
            Ant;
        
        ping ->
            logger:logEvent(utils:getAntLog(Ant),"Received ping"),
            Msg = Sender ! {self(),make_ref(),Reference,pong},
            logger:logMessageSent(utils:getAntLog(Ant),Msg,Sender),
            Ant;
        
        {set_ant_attribute,{Type,Value}} ->
            logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Atempting to set attribute ~p to ~p",[Type,Value])),
            Map = utils:getAntAttributes(Ant),
            New_Map = maps:put(Type,Value,Map),
            New_Ant = utils:setAntAttribute(Ant,New_Map),
            logger:logEvent(utils:getAntLog(Ant),"Updated attributes"),
            Msg = Sender ! {self(),make_ref(),Reference,{reply,set_ant_attribute,sucsess}},
            logger:logMessageSent(utils:getAntLog(Ant),Msg,Sender),
            New_Ant;
        
        _Any ->
            logger:logWarning(utils:getAntLog(Ant),"Received pointless Request! Crashing system :("),
            ?debugFmt("Ant(~p) Received pointless request ~p ~n Crashing system",[self(),_Any]),
            timer:sleep(100),
            exit(failure)
    end.
            
%% =====================================================================================
%% Internals
%% =====================================================================================
      
processHood(Hood,Feremone) ->
    List0 = tuple_to_list(Hood),
    {Front,Back} = lists:split(4,List0),
    List1 = Front ++ tl(Back),
    List2 = lists:map(fun(X) -> hoodFilter(X, Feremone) end, List1),
    Directions = [northwest, north, northeast, west, east, southwest ,south, southeast],
    List3 = lists:zip(Directions,List2),
    Sorted = sorter:sort(list_to_tuple(List3)),
    Sorted.

hoodFilter(Hood_Element,Feremone) ->
    case Hood_Element of
        none ->
            -1.0;
        _ ->
            Type = maps:get(type,Hood_Element),
            case Type of
                block -> 0.0;
                _ -> 
                    Feremone_Map = maps:get(feremones,Hood_Element),
                    {Strength,_} = maps:get(base_feremone,Feremone_Map),
                    Strength
            end
    end.

    

pickDirection([Hd|[]]) ->
  Hd;
pickDirection([Hd|Tl]) ->
    Value = random:uniform(),
    if
        Value < ?SELECTION_PROBABILITY ->
            Hd;
        true ->
            pickDirection(Tl)
    end;    

pickDirection(Sorted_Hood) ->
    pickDirection(element(1,lists:unzip(tuple_to_list(Sorted_Hood)))).


sendAndReceive(Ant,Message) ->
    Cell_Pid = utils:getAntCell(Ant),
    Reference = make_ref(),
    Msg = Cell_Pid ! {self(), Reference, Message},
    logger:logMessageSent(utils:getAntLog(Ant),Msg,Cell_Pid),
    {{_,_,_,Payload}=Received_Mesage,New_Buffer} = message_buffer:receiver(Reference,Cell_Pid,utils:getAntMetadata(Ant)),
    New_Ant = utils:setAntMetadata(Ant,New_Buffer),
    logger:logMessage(utils:getAntLog(Ant),Received_Mesage),
    {Payload,New_Ant}.

    
dump({Pid, Cell_Pid, State, Attributes, {Length,Buffer}, _}) ->
    S0 = "~n=======================================~nDUMP OF ANT ~p at cell ~p ~n",
    S1 = "IN STATE = ~n~p~n",
    S2 = "ATTRIBUTES = ~n~p~n",
    S3 = "MESSAGE BUFFER (~p waiting messages) = ~n~p~n",
    S4 = "=======================================~n",
    Big_String = string:join([S0,S1,S2,S3,S4],""),
    Args = [Pid,Cell_Pid,State,Attributes,Length,Buffer],
    %logger:logEvent(Log, logger:makeCoolString(Big_String, Args)),
    ?debugFmt(Big_String,Args).
    %logger:logEvent(Log,re:replace(logger:makeCoolString("~ts~ts~ts~ts~ts",[S0,S1,S2,S3,S4])),"\\","~").


%% =====================================================================================
%% Tests
%% =====================================================================================


spawnTest() ->
    logger:initLogger(),
	Grid = grid_init:initGrid({2,1}),
	Cell_ID = grid_init:getGridElement({0,0},{2,1},Grid),
	Ant = spawn_Ant(Cell_ID, []),
	timer:sleep(2),
	Ant ! {self(),make_ref(),ping},
    receive
        {_,_,_,pong} ->
            ok;
        _Any ->
            ?debugFmt("Received idiotic message ~p instead of pong",[_Any]),
            ?assert(false)
    end,
    true.

hoodProcessor_test() ->
    Really_Annoying_To_Write = {
                                none, 
                                #{feremones => #{food_feremone => {4.0,0}, base_feremone => {4.0,0}},type => plain}, 
                                #{feremones => #{food_feremone => {7.0,0}, base_feremone => {7.0,0}},type => plain},
                                #{feremones => #{food_feremone => {2.0,0}, base_feremone => {2.0,0}},type => plain}, 
                                #{feremones => #{food_feremone => {5.0,0}, base_feremone => {5.0,0}},type => plain}, 
                                #{feremones => #{food_feremone => {8.0,0}, base_feremone => {8.0,0}},type => plain}, 
                                #{feremones => #{food_feremone => {3.0,0}, base_feremone => {3.0,0}},type => block}, 
                                #{feremones => #{food_feremone => {6.0,0}, base_feremone => {6.0,0}},type => plain}, 
                                #{feremones => #{food_feremone => {9.0,0}, base_feremone => {9.0,0}},type => plain} 
                               },
    True = {southeast,east,northeast,south,north,west,southwest,northwest},
    Result1 = list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,base_feremone))))),
    Result2 = list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,food_feremone))))),
    [?assertEqual(True,list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,base_feremone)))))),
     ?assertEqual(True, list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,food_feremone))))))].

directionPickerAux(0,Cool_Tuple) ->
    Cool_Tuple;
directionPickerAux(N,Cool_Tuple) ->
    Thing = {{1,0},{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},{8,0}},
    Direction = pickDirection(Thing),
    Old_Count = element(2,element(Direction,Cool_Tuple)),
    New_Tuple = setelement(Direction,Cool_Tuple,  {Direction,Old_Count+1}),
    directionPickerAux(N-1,New_Tuple).
    

directionPicker_test() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Thing = {{1,0},{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},{8,0}},
    {A,B,C,D,E,F,_,_}  = directionPickerAux(10000, Thing),
    Fixed = {A,B,C,D,E,F,{7,1},{8,0}},
    %?debugFmt("Cool histogram is : ~p",[Fixed]),
    Sorted= sorter:sort(Fixed),
    %?debugFmt("Sorted histogram is : ~p",[Sorted]).
    Lol = element(1,lists:unzip(tuple_to_list(Sorted))),
    [?assertEqual([1,2,3,4,5,6,7,8],Lol)].

searchForFoodTest()->
    logger:initLogger(),
    Grid = grid_init:initGrid({3,3}),
    Cell_ID = grid_init:getGridElement({1,1},{3,3},Grid),
    Ant = spawn_Ant(Cell_ID, []),
    Ant ! {self(),dump},
    timer:sleep(10),
    Cell_With_Food = grid_init:getGridElement({0,0},{3,3},Grid),
    Ref = make_ref(),
    Cell_With_Food ! {self(),Ref,{set_cell_attribute,{food,1}}},
    receive
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    Ant ! {self(),start_ant},
    timer:sleep(2000),
    Ref2 = make_ref(),
    Cell_With_Food ! {self(),Ref2,query_state},
    receive
        {_,_,_,{reply,query_State,Attributes}} ->
            Type = maps:get(food,Attributes,none),
            ?assertEqual(Type,0),
            ok;
        _ ->
            ?debugMsg("Failed to check cell food contents :("),
            ?assert(false)
    end,
    true.


spawn_test() ->
    [?assert(spawnTest())].

searchForFood_test() ->
    [?assert(searchForFoodTest())].



