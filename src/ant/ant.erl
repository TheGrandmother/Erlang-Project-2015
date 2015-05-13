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
	State = searching_for_food,
	Log = logger:makeLog(logger:makeCoolString("Ant ~p",[Ant_ID]),self()),
	Buffer = {0, []},  
	Ref = make_ref(),
	Msg = Cell_Pid ! {Ant_ID, Ref, {place_ant, Ant_ID}},
	{{Sender, _, _, Response}, New_Buffer} = message_buffer:receiver(Ref, Buffer),
	Ant = {self(), Cell_Pid, State, Attributes, New_Buffer, Log},
	logger:logEvent(utils:getAntLog(Ant),"Ant spawned."),
	logger:logMessageSent(utils:getAntLog(Ant),Msg,Cell_Pid),
	case Response of
		{place_ant_reply, fail} ->
			logger:logEvent(utils:getAntLog(Ant),"Ant failed to get placed and is killed."),
			exit(fail);
		{place_ant_reply, sucsess} ->
			if 
				Sender =/= Cell_Pid ->
				   io:format("Responder is not given cell ~n"),
				   exit(fail);
				true ->
					logger:logEvent(utils:getAntLog(Ant),"Ant successfully placed in "),
					logger:logEvent(utils:getAntLog(Ant),Cell_Pid),
					ant_Main(Ant)
			end;
		_ ->
			logger:logEvent(utils:getAntLog(Ant),"Ant fucked up."),
			exit(fail)
	end.

%% Main loop of ant
%% It is currently just walking around randomly
ant_Main(Ant) ->
	%% Start with a nap
	timer:sleep(100),
	State = utils:getAntState(Ant),
	Cell = utils:getAntCell(Ant),
	%% Tons of printout 
	logger:logEvent(utils:getAntLog(Ant),(Cell)),
	Buffer = utils:getAntMetadata(Ant),
	case State of
		searching_for_food ->
			logger:logEvent(utils:getAntLog(Ant),"Ant is searching for food"),
			Directions = [northwest, north, northeast, west, east, southwest, south, southeast],
			Magic = random:uniform(8),
			Direction = lists:nth(Magic, Directions),
			logger:logEvent(utils:getAntLog(Ant),"Ant trying to move"),
			logger:logEvent(utils:getAntLog(Ant),Direction),
			Ref = make_ref(), 
			Msg = Cell ! {self(), Ref, {move_ant, Direction}},
			logger:logMessageSent(utils:getAntLog(Ant),Msg,Cell),
			{Message, New_Buffer} = message_buffer:receiver(Ref,Buffer),
			New_Ant = utils:setAntMetadata(Ant, New_Buffer),
			case Message of
				{_, _, _,{move_ant_reply,{sucsess, Dest}}} ->
					Newer_Ant = utils:setAntCell(New_Ant, Dest),
					logger:logEvent(utils:getAntLog(Ant),"Ant is moving"),
					ant_Main(Newer_Ant);
				{_, _, _,{move_ant_reply,{fail, _}}} ->
					logger:logEvent(utils:getAntLog(Ant),"Ant failed to move~n"),
					ant_Main(New_Ant);
				_ ->
					logger:logEvent(utils:getAntLog(Ant),"Ant got fucked up~n"),
					ant_Main(New_Ant)
			end;
		_ ->
			ant_Main(Ant)
	end.


%% Spawns a 1 by 2 grid and places an ant that runs around for 5 seconds
spawn_test() ->
	Grid = grid_init:initGrid({2,1}),
	Cell_ID = grid_init:getGridElement({0,0},{2,1},Grid),
	Ant = spawn_Ant(Cell_ID, []),
	timer:sleep(5000),
	exit(Ant, kill).


