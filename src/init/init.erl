%% @author Tanshinan
%% @todo Add proper documentation, replace PLACEHOLDER() with actual cellstarting function
%% @todo possibly replacing a bunch of internal functions with util-functions if applicable


-module(init).

-export([init_Grid/1,set_Grid_Element/3]).

%%
%% Exported functions
%%

-type grid()::{Array::array:array(), {Width::integer(), Height::integer()}}.


-spec init_Grid({Width::integer(),Height::integer()}) -> ok.
%% Initializes the grid on which the simulation is run
init_Grid(Size) ->
	Grid = new_Grid(Size),
	Filled_Grid = fill_Grid(Size, Grid),
	linkup(Size,Filled_Grid).
%% @todo Add ants and stuff



%% set_Grid_Element should be moved to another module
-spec set_Grid_Element({X::integer(), Y::integer()},_Value, Array::array:array()) -> array:array().
%% Sets an element in the provided two-dimensional array to given value
set_Grid_Element({X, Y}, Value, Array) ->
	Y_Array = array:get(X, Array),
	New_Y_Array = array:set(Y, Value, Y_Array),
	array:set(X, New_Y_Array, Array).

%% 
%% Internal functions
%%


-spec new_Grid({Width::integer(), Height::integer()}) -> grid().
%% Creates a two-dimensional array with given size
new_Grid({Width, Height}) ->
	array:new([{size, Width}, {default, array:new([{size, Height}, {default, none}])}]).


-spec fill_Grid({Width::integer(), Height::integer()}, Array::array:array()) -> array:array().
%% Fills the grid with PIDs to their workers
fill_Grid({Width, Height}, Array) ->
	fill_Grid_Aux({0,0}, {Width,Height}, Array).



-spec get_Grid_Element({X::integer(), Y::integer()},{Width::integer(),Height::integer()}, Array::array:array()) -> _A.
%% Gets the value in a given coordinate in given two-dimensional array, if given coordinate exists
get_Grid_Element({X,Y},{Width,Height},Array) when X < Width, X >= 0, Y < Height, Y >= 0 ->
	array:get(Y, array:get(X,Array));
get_Grid_Element(_,_,_) ->
	none.


-spec linkup({Width::integer(), Height::integer()},Array) -> Array.
%% Makes sure that every worker in given two-dimensional array is up and running
linkup({Width, Height},Array) ->
	linkup_Aux({0,0},{Width, Height},Array).


%% 
%% Helper and utility Functions
%%

%% Recursive help function for linkup()
linkup_Aux({_,Y},{_,Height}, Array) when Y == Height ->
	Array;
linkup_Aux({X, Y},{Width, Height}, Array) when X == Width ->
	linkup_Aux({0, Y+1},{Width,Height}, Array);
linkup_Aux({X,Y},Grid_Size,Array) ->
	Center = get_Grid_Element({X,Y},Grid_Size,Array),
	if 
		Center == none ->
			ok;
		true ->
			Hood = get_Hood({X,Y},Grid_Size,Array),
			Next = get_Next_Grid_Element({X,Y},Grid_Size),
			Center ! {self(), {linkup, Hood, Next}},
			linkup_Aux({X+1, Y}, Grid_Size, Array)
	end.


%% Returns a tuple with PIDs to a workers neighborhood
get_Hood({X,Y},Grid_Size,Array) ->
	{get_Grid_Element({X-1, Y+1},Grid_Size,Array),
	 get_Grid_Element({X  , Y+1},Grid_Size,Array),
	 get_Grid_Element({X+1, Y+1},Grid_Size,Array),
	 get_Grid_Element({X-1, Y  },Grid_Size,Array),
	 get_Grid_Element({X  , Y  },Grid_Size,Array),
	 get_Grid_Element({X+1, Y  },Grid_Size,Array),
	 get_Grid_Element({X-1, Y-1},Grid_Size,Array),
	 get_Grid_Element({X  , Y-1},Grid_Size,Array),
	 get_Grid_Element({X+1, Y-1},Grid_Size,Array)}.

%% Returns "next" position in two-dimensional array, in a bottom to top, left to right order
get_Next_Grid_Element({X,Y},{Width,Height}) when X == (Width-1), Y == (Height-1) ->
	none;
get_Next_Grid_Element({X,Y},{Width,_}) when X == (Width-1) ->
	{0, Y+1};
get_Next_Grid_Element({X,Y},_) ->
	{X+1, Y}.


%% Recursive helper function to fill_Grid
fill_Grid_Aux({_,Y}, {_, Height},Array) when Y == Height ->
	Array;
fill_Grid_Aux({X,Y}, {Width, Height},Array) when X == Width ->
	fill_Grid_Aux({0, Y+1},{Width, Height}, Array);
fill_Grid_Aux({X,Y},{Width, Height}, Array) ->
	New_Array = setGridElement({X,Y}, spawnlink(fun() -> PLACEHOLDER() end), Array),
	%% PLACEHOLDER() to be replaced with actual cellstarting function
	fill_Grid_Aux({X+1, Y},{Width, Height},New_Array).


