

-module(ant).

-import(message_buffer, [receiver/2]).
-import_type([ant/0, ant_state/0, ant_attributes/0, message_buffer/0, log/0]).
-export([spawn_Ant/2, spawn_test/0]).

%% Attempts to spawn ant in given Cell
spawn_Ant(Cell_Pid, Attributes) ->
	spawn_link(fun() -> ant_Init(Cell_Pid, Attributes) end).


ant_Init(Cell_Pid, Attributes) ->
	Ant_ID = self(),
	State = searching_for_food,
	Buffer = {0, []},  
	Ref = make_ref(),
	Cell_Pid ! {Ant_ID, Ref, {place_ant, Ant_ID}},
	{{Sender, _, _, Response}, New_Buffer} = message_buffer:receiver(Ref, Buffer),
%	{Message, New_Buffer} = message_buffer:reciever(Ref, Buffer),
	Ant = {self(), Cell_Pid, State, Attributes, New_Buffer},
	case Response of
		{place_ant_reply, fail} ->
			io:format("Ant placement failed~n"),
			exit(fail);
		{place_ant_reply, sucsess} ->
			if 
				Sender =/= Cell_Pid ->
				   io:format("Responder is not given cell ~n"),
				   exit(fail);
				true ->
					ant_Main(Ant)
			end;
		_ ->
			io:format("Unexpected response during ant placement~n"),
			exit(fail)
	end.

ant_Main(Ant) ->
	timer:sleep(100),
	State = utils:getAntState(Ant),
	Cell = utils:getAntCell(Ant),
	io:format("Ant in cell ~p ~n",[Cell]),
	Buffer = utils:getAntMetadata(Ant),
	case State of
		searching_for_food ->
			Directions = [northwest, north, northeast, west, east, southwest, south, southeast],
			Magic = random:uniform(8),
			Direction = lists:nth(Magic, Directions),
			Ref = make_ref(), 
			Cell ! {self(), Ref, {move_ant, Direction}},
			{Message, New_Buffer} = message_buffer:receiver(Ref,Buffer),
			New_Ant = utils:setAntMetadata(Ant, New_Buffer),
			case Message of
				{_, _, _,{move_ant_reply,{sucsess, Dest}}} ->
					Newer_Ant = utils:setAntCell(New_Ant, Dest),
					ant_Main(Newer_Ant);
				{_, _, _,{move_ant_reply,{fail, _}}} ->
					ant_Main(New_Ant);
				_ ->
					io:format("Unexpected response during ant movement ~n"),
					ant_Main(New_Ant)
			end;
		_ ->
			ant_Main(Ant)
	end.




spawn_test() ->
	Grid = grid_init:initGrid({2,1}),
	Cell_ID = grid_init:getGridElement({0,0},{2,1},Grid),
	spawn_Ant(Cell_ID, []).


