%% @author Tanshinan
%% @todo Add further functionality of ant

-module(ant).

-import(message_buffer, [receiver/2]).
-import_type([ant/0, ant_state/0, ant_attributes/0, message_buffer/0, log/0]).
-export([spawn_Ant/2, spawn_test/0]).
-include_lib("eunit/include/eunit.hrl").
%% Attempts to spawn ant in given Cell
spawn_Ant(Cell_Pid, Attributes) ->
	spawn_link(fun() -> ant_Init(Cell_Pid, Attributes) end).


%% Creates needed information for ant, and places it in given cell
ant_Init(Cell_Pid, Attributes) ->
	Ant_ID = self(),
	State = idling,
	Log = logger:makeLog(logger:makeCoolString("Ant ~p",[Ant_ID]),self()),
	Buffer = {0, []},  
	Ref = make_ref(),
	Msg = Cell_Pid ! {Ant_ID, Ref, {place_ant, Ant_ID}},
	{{Sender, _, _, Response}, New_Buffer} = message_buffer:receiver(Ref, Buffer),
	Ant = {self(), Cell_Pid, State, Attributes, New_Buffer, Log},
	logger:logEvent(utils:getAntLog(Ant),"Ant spawned."),
	logger:logMessageSent(utils:getAntLog(Ant),Msg,Cell_Pid),
	case Response of
		{reply,place_ant, fail} ->
			logger:logEvent(utils:getAntLog(Ant),"Ant failed to get placed and is killed."),
			exit(fail);
		{reply,place_ant, sucsess} ->

			logger:logEvent(utils:getAntLog(Ant),logger:makeCoolString("Ant placed in Cell ~p ", [Cell_pid])),
            antMain(Ant);
		_ ->
			logger:logWarning(utils:getAntLog(Ant),"Ant recived malformed message whilst awaiting initial placement"),
			exit(fail)
	end.

%% Main loop of ant
antMain(In_Ant) ->
	%% Start with a nap
    logger:logEvent(utils:getAntLog(In_Ant),"Ant entered main loop"),
    case message_buffer:hasMessages(utils:getAntMetadata(In_Ant)) of
        true->
            logger:logEvent(utils:getAntLog(In_Ant),"Ant has messages to process"),
            {Message,New_Buffer} = message_buffer:receiver(utils:getAntMetadata),
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
                    
        false ->
            tbi
                    
            
	end.

handleOneWay(Ant,Payload) ->
    case Payload of
        dump ->
            logger:logEvent(utils:getAntLog(Ant),"Received dump message"),
            

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
        
        {set_ant_attribute,{Attribute,{Type,Value}}} ->
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
            
            
    
dump(Ant) ->
    


%% Spawns a 1 by 2 grid and places an ant that runs around for 5 seconds
spawn_test() ->
	Grid = grid_init:initGrid({2,1}),
	Cell_ID = grid_init:getGridElement({0,0},{2,1},Grid),
	Ant = spawn_Ant(Cell_ID, []),
	timer:sleep(5000),
	exit(Ant, kill).


