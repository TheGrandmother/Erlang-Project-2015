%% @author Tanshinan
%% @todo Add proper documentation, replace PLACEHOLDER() with actual cellstarting function
%% @todo possibly replacing a bunch of internal functions with util-functions if applicable


-module(grid_init).

-include_lib("eunit/include/eunit.hrl").


-export([initGrid/1,setGridElement/3, getGridElement/3]).

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

%% @todo Add ants and stuff



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
buildAndStartSimpleWorld(Gui_Module) ->
	Size = {7,7},
	Array = initGrid(Size),
	Foods = [{2,0},{3,0},{4,0}],
	Blocks =  [{2,3},{3,3},{4,3}],
	Nests = [{0,6},{1,6},{2,6},{3,6},{4,6},{5,6},{6,6}],
	Gui_Module ! {self(), {gui_init,Size}},
	broadcast(Array,Size,{self(),make_ref,{set_cell_attribute,{gui_module,Gui_Module}}}),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{type,block}}} end,Blocks),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{type,nest}}} end,Nests),
	lists:map(fun(X) -> getGridElement(X,Size,Array)! {self(),make_ref,{set_cell_attribute,{food,1000}}} end,Foods),
	utils:ignoreMessgaes(length(Nests)+length(Foods)+length(Blocks)),
	Queen = spawn(fun() -> dummyQueen(0, 0, #{}) end).
	Ants

%% =====================================================================================
%% Internal functions
%% =====================================================================================
dummyQueen(Foods_Picked_Up,Foods_Deposited,Map) ->
	receive
		{Pid,{found_food,Steps}} ->
			Old_Steps = maps:get(Pid,Map,0),
			Diff = Steps-Old_Steps,
			New_Map = maps:put(Pid,Steps,Map),
			?debugFmt("Our little ant ~p found food in ~p steps",[Pid,Diff]),
			?debugFmt("We have now found ~p foods in a total of ~p steps",[Foods_Picked_Up+1,getTotalSteps(Map)]),
			dummyQueen(Foods_Picked_Up+1,Foods_Deposited,New_Map);
		
		{Pid,{returned_with_food,Steps}} ->
			Old_Steps = maps:get(Pid,Map,0),
			Diff = Steps-Old_Steps,
			New_Map = maps:put(Pid,Steps,Map),
			?debugFmt("Our little ant ~p returned with food in ~p steps",[Pid,Diff]),
			?debugFmt("We have now returned with ~p foods in a total of ~p steps",[Foods_Deposited+1,getTotalSteps(Map)]),
			dummyQueen(Foods_Picked_Up,Foods_Deposited+1,New_Map);
		
		_A ->
			?debugFmt("Our little ant sent me an odd message ~p",[_A]),
			?assert(false)
	end.
		
getTotalSteps(Map) ->
	lists:sum(element(2,lists:unzip(maps:to_list(Map)))).
	


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
    {Array,Refs } = linkupAux({0,0},{Width, Height},Array,[]).


%% =====================================================================================
%% Helper and utility Functions
%% =====================================================================================

broadcast(Grid,Size,Message) ->
	broadcastAux({0,0},Size,Grid,Message).

broadcastAux({X,Y},{Width,Height},Grid,Message) when X == Width -1, Y == Height ->
	utils:ignoreMessgaes(X*Y);

broadcastAux({X, Y},{Width, Height}, Array,Message) when X == Width ->
    broadcastAux({0, Y+1},{Width,Height}, Array, Message);
broadcastAux({X,Y},Grid_Size,Array,Message) ->
		getNextGridElement({X,Y}, Array) ! Message,
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


