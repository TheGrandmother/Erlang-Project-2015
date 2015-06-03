%% @author Aleksander Lunqvist and Henrik Sommerland	
%% @doc Module used to initialize the world
%%


-module(grid_init).

-include_lib("eunit/include/eunit.hrl").


-export([initGrid/1,buildAndStartSimpleWorld/1]).

%% =====================================================================================
%% Exported functions
%% =====================================================================================

-type grid()::{Array::array:array(), {Width::integer(), Height::integer()}}.


-spec initGrid({Width::integer(),Height::integer()}) -> ok.
%% Initializes the grid on which the simulation is run
initGrid(Size) ->
    logger:initLogger(),
    Grid = newGrid(Size),
    Filled_Grid = fillGrid(Size, Grid),
    {Array,Refs} = linkup(Size,Filled_Grid),
    awaitReplies(Refs,lists:flatten(lists:map(fun(X) -> array:to_list(X) end,array:to_list(Array))),{0,[]}),
    Array.


%% set_Grid_Element should be moved to another module
-spec setGridElement({X::integer(), Y::integer()},_Value, Array::array:array()) -> array:array().
%% Sets an element in the provided two-dimensional array to given value
setGridElement({X, Y}, Value, Array) ->
    Y_Array = array:get(X, Array),
    New_Y_Array = array:set(Y, Value, Y_Array),
    array:set(X, New_Y_Array, Array).

%  0 1 2 3 4 5 6
%0 . . F F F . . 
%1 . . . . . . .
%2 . . . . . . .
%3 . . B B B . .
%4 . . . . . . .
%5 . . . . . . .
%6 N N N N N N N 

%%@doc Builds and starts a nice little test world and such.
%%
%%`Gui' Is  the pid to the gui module
-spec buildAndStartSimpleWorld(Gui::pid)-> ok.
buildAndStartSimpleWorld(Gui) ->
    
    Width = 25,
    Height = 25,
	Size = {Width,Height},
	Array = initGrid(Size),
    io:format("started grid"),
	Foods = [{X,0} || X <- lists:seq(0,Width-1)],
	Blocks =  [{X,Height div 3} || X <- lists:seq(Width div 3,(2*Width) div 3)]
                ++[{X,(2*Height) div 3} || X <- lists:seq(Width div 3,(2*Width) div 3)]
                ++[{(Width) div 3, Y} || Y <- lists:seq(Height div 3,(2*Height) div 3)]
                ++[{(2*Width) div 3, Y} || Y <- lists:seq(Height div 3,(2*Height) div 3)],
	Nests = [{X,Height-1} || X <- lists:seq(0,Width-1)],
    Ant_Positions = [{X,Height-1} || X <- lists:seq(0,Width-1),X rem 2 == 0], 
    %Nests = [{6,6}],
    
    case is_pid(Gui) of
        false ->
            io:format("spawning dummy gui~n"),
            Gui_Module = spawn(fun() ->  dummyGui() end),
            io:format("Dummy gui spawned~n");
        true ->
            Gui_Module = Gui
    end,
    io:format("Gui module = ~p ~n",[Gui_Module]),
    Gui_Module ! {self(), {gui_init,Size}},
	broadcast(Array,Size,{self(),make_ref,{set_cell_attribute,{gui_module,Gui_Module}}}),
    io:format("Broadcasted Gui to cells~n"),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{type,block}}} end,Blocks),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{type,nest}}} end,Nests),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{food,1000}}} end,Foods),
    io:format("Filled cells~n"),
	utils:ignoreMessages(length(Nests)+length(Foods)+length(Blocks)),
	Queen = spawn_link(fun() -> dummyQueen(0, 0, #{},0,getTimeStamp(),0) end),
    io:format("Spawned Queen~n"),
    Ants = lists:map(fun(X) -> ant:spawnAnt(getGridElement(X,Size,Array), Queen)end, Ant_Positions),
    io:format("Spawned ants~n"),
    lists:map(fun(X) -> X ! {self(), start_ant} end,Ants),
    io:format("Started ants~n"),
    %timer:sleep(50000).
    ok.
    

%% =====================================================================================
%% Internal functions
%% =====================================================================================

dummyGui() ->
    receive
        _ ->
            ok
    end,
    dummyGui().


getTimeStamp() ->
    {MegaSecs,Secs,MicroSecs} = erlang:now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.


dummyQueen(Foods_Picked_Up,Foods_Deposited,Map,Events,Previous_Time,Total_Time) ->
	receive
		{Pid,{found_food,Steps}} ->		
			New_Map = maps:put(Pid,Steps,Map),
            Time = getTimeStamp(),
            Time_Diff = Time - Previous_Time,
            New_Total = Total_Time + Time_Diff,
			dummyQueen(Foods_Picked_Up+1,Foods_Deposited,New_Map,Events+1,Time,New_Total);
		
		{Pid,{returned_with_food,Steps}} ->
			New_Map = maps:put(Pid,Steps,Map),
			Time = getTimeStamp(),
            Time_Diff = Time - Previous_Time,
            New_Total = Total_Time + Time_Diff,
            dummyQueen(Foods_Picked_Up,Foods_Deposited+1,New_Map,Events+1,Time,New_Total);
		
		_A ->
			?debugFmt("Our little ant sent me an odd message ~p",[_A]),
			?assert(false)
	end.
		
-spec newGrid({Width::integer(), Height::integer()}) -> grid().
%% Creates a two-dimensional array with given size
newGrid({Width, Height}) ->
    array:new([{size, Width}, {default, array:new([{size, Height}, {default, none}])}]).


-spec fillGrid({Width::integer(), Height::integer()}, Array::array:array()) -> array:array().
%% Fills the grid with PIDs to their workers
fillGrid({Width, Height}, Array) ->
    fillGridAux({0,0}, {Width,Height}, Array).



-spec getGridElement({X::integer(), Y::integer()},{Width::integer(),Height::integer()}, Array::array:array()) -> _A.
%% Gets the value in a given coordinate in given two-dimensional array, if given coordinate exists
getGridElement({X,Y},{Width,Height},Array) when X < Width, X >= 0, Y < Height, Y >= 0 ->
    array:get(Y, array:get(X,Array));
getGridElement(_,_,_) ->
    none.


-spec linkup({Width::integer(), Height::integer()},Array) -> {Array,_Refs}.
%% Makes sure that every worker in given two-dimensional array is up and running
linkup({Width, Height},Array) ->
    {Array,_ } = linkupAux({0,0},{Width, Height},Array,[]).


%% =====================================================================================
%% Helper and utility Functions
%% =====================================================================================

broadcast(Grid,Size,Message) ->
	broadcastAux({0,0},Size,Grid,Message).

broadcastAux({_,Y},{Width,Height},_,_) when Y == Height ->
	utils:ignoreMessages(Width*Height);

broadcastAux({X, Y},{Width, Height}, Array,Message) when X == Width->
    broadcastAux({0, Y+1},{Width,Height}, Array, Message);

broadcastAux({X,Y},Grid_Size,Array,Message) ->
        Pid =getGridElement({X,Y},Grid_Size, Array), 
        io:format("broadcasting to ~p  at ~p~n",[Pid,{X,Y}]),
		Pid ! Message,
		broadcastAux({X+1,Y},Grid_Size,Array,Message).

awaitReplies([],_,_) ->
    ok;
awaitReplies(Refs,Pids,Buffer) ->
    {Message,New_Buffer,New_Refs} = message_buffer:receiver(Refs,Pids,Buffer),
    case Message of 
        {_,_,_,{reply,linkup,sucsess}} ->
            awaitReplies(New_Refs,Pids,New_Buffer);
        _Any ->
            io:format("Received stupid message whilst awaiting linkup confirmation"),
            exit(failure)
    end.


%% Recursive help function for linkup()
linkupAux({_,Y},{_,Height}, Array,Refs) when Y == Height ->
    {Array,Refs};
linkupAux({X, Y},{Width, Height}, Array,Refs) when X == Width ->
    linkupAux({0, Y+1},{Width,Height}, Array, Refs);
linkupAux({X,Y},Grid_Size,Array,Refs) ->
    Center = getGridElement({X,Y},Grid_Size,Array),
    if 
        Center == none ->
            ok;
        true ->
            Hood = getHood({X,Y},Grid_Size,Array),
            Next = getNextGridElement({X,Y},Grid_Size),
            Ref = make_ref(),
            Center ! {self(),Ref, {linkup, Hood, Next}},
            linkupAux({X+1, Y}, Grid_Size, Array, [Ref]++Refs)
    end.


%% Returns a tuple with PIDs to a workers neighborhood
getHood({X,Y},Grid_Size,Array) ->
    {getGridElement({X-1, Y+1},Grid_Size,Array),
     getGridElement({X  , Y+1},Grid_Size,Array),
     getGridElement({X+1, Y+1},Grid_Size,Array),
     getGridElement({X-1, Y  },Grid_Size,Array),
     getGridElement({X  , Y  },Grid_Size,Array),
     getGridElement({X+1, Y  },Grid_Size,Array),
     getGridElement({X-1, Y-1},Grid_Size,Array),
     getGridElement({X  , Y-1},Grid_Size,Array),
     getGridElement({X+1, Y-1},Grid_Size,Array)}.

%% Returns "next" position in two-dimensional array, in a bottom to top, left to right order
getNextGridElement({X,Y},{Width,Height}) when X == (Width-1), Y == (Height-1) ->
    none;
getNextGridElement({X,Y},{Width,_}) when X == (Width-1) ->
    {0, Y+1};
getNextGridElement({X,Y},_) ->
    {X+1, Y}.


%% Recursive helper function to fill_Grid
fillGridAux({_,Y}, {_, Height},Array) when Y == Height ->
    Array;
fillGridAux({X,Y}, {Width, Height},Array) when X == Width ->
    fillGridAux({0, Y+1},{Width, Height}, Array);
fillGridAux({X,Y},{Width, Height}, Array) ->
    New_Array = setGridElement({X,Y}, spawn_link(fun() -> cell:spawnCell({X,Y}) end), Array),
    fillGridAux({X+1, Y},{Width, Height},New_Array).


%% =====================================================================================
%% Tests
%% =====================================================================================

linkupTest_test() ->
    initGrid({10,10}).

%bob() ->
%    buildAndStartSimpleWorld(none).

%removeMe_test_()->    
%    {timeout, 120, [fun bob/0]}.

   


