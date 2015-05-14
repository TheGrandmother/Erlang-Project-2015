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

%% =====================================================================================
%% Internal functions
%% =====================================================================================


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


