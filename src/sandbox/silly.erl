%% @author grandmother
%% @doc @todo Add description to silly.


-module(silly).
-import(test,[cellStarter/0,spawnAnt/2]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([testShit/0,get2D/2]).

-type grid()::{Array::array:array(),{Width::integer(),Height::integer()}}.

-spec testShit() -> ok.
testShit() ->
    Array = newGrid({10,10}),
    {Width,Height} = Size = {10,10},
    A0 = fillGrid(Size,Array),
    linkup(Size, A0),
    io:format("Linkup complete ~n"),
	DatCell = get2D({2,3},A0),
	Ant_Pid = spawnAnt({1,3}, A0),
	DatCell ! {self(), set_state, black},  
	receive 
		_ ->
			ok
	end,
	timer:sleep(100),
    coolPrint(get2D({0,0},A0),[Width,0]).

-spec newGrid({Width::integer(),Height::integer()}) -> grid().
newGrid({Width,Height}) ->
   array:new([{size, Width}, {default, array:new( [{size, Height},{default,none}] )}]).
    
    
    
    
-spec linkup({Width::integer(),Height::integer()},Array) -> Array.
linkup({Width,Height},Array)->
    linkup_aux({0,0},{Width,Height},Array).
    
-spec linkup_aux({X::integer(),Y::integer()},{Width::integer(),Height::integer()},Array) -> Array.
linkup_aux({X,Y},{Width,Height},Array) when Y == Height ->
    Array;
linkup_aux({X,Y},{Width,Height},Array) when X == Width ->
    linkup_aux({0,Y+1},{Width,Height},Array);

linkup_aux({X,Y},_Size,Array) ->
    

    Center = testToGet({X,Y},_Size,Array),
    if 
        Center == none -> 
            ok;
        true ->
                %{Ul,Um,Ur,Ml,Mm,Mr,Ll,Lm,Lr}
                Hood = {
                        testToGet({X-1, Y+1},_Size,Array),
                        testToGet({X  , Y+1},_Size,Array),
                        testToGet({X+1, Y+1},_Size,Array),
                        testToGet({X-1, Y  },_Size,Array),
                        testToGet({X ,  Y  },_Size,Array),
                        testToGet({X+1, Y  },_Size,Array),
                        testToGet({X-1, Y-1},_Size,Array),
                        testToGet({X  , Y-1},_Size,Array),
                        testToGet({X+1, Y-1},_Size,Array)       
                        },
                Next = testToGet(getNext({X,Y},_Size),_Size,Array),
                %io:format("I want to send to ~w ~n",[Center]),
                Center ! {self(), hood_addresses, {Hood,Next,{X,Y}} },
                linkup_aux({X+1,Y},_Size,Array)
    end.




-spec fillGrid({Width::integer(),Height::integer()},Array::array:array()) -> array:array().
fillGrid({Width,Height},Array) ->
    fillGrid_aux({0,0},{Width,Height},Array).



-spec fillGrid_aux({X::integer(),Y::integer()},{Width::integer(),Height::integer()},Array::array:array()) -> array:array().
fillGrid_aux({X,Y},{Width,Height},Array) when Y == Height -> %This is ugly
    io:format("Filled the entire grid ~n"),
    Array;

fillGrid_aux({X,Y},{Width,Height},Array) when X == Width ->
    fillGrid_aux({0,Y+1},{Width,Height},Array);

fillGrid_aux({X,Y},{Width,Height},Array) ->
    A0 = set2D({X,Y},spawn(fun() -> cellStarter() end), Array),
    %io:format("Filled ~w ~n",[{X,Y}]),
    %io:format("And at ~w we have ~w ~n ",[{X,Y},{get2D({X,Y},A0)}])
    fillGrid_aux({X+1,Y},{Width,Height},A0).

-spec mapOnGrid(Fun::fun((pid(),list()) -> ok ),Args::list(),Current::pid()|none) -> ok.
mapOnGrid(_,_,none) -> 
    ok;
mapOnGrid(Fun,Args,Current) ->
    Fun(Current,Args),
    Current ! {self(),get_next},
    receive
        {_,next_reply,Next} ->
            mapOnGrid(Fun,Args,Next);
        _A ->
            io:format("Map On grid received pointless ~w message ~n",[_A])
    end.

coolPrint(none,_) ->
    ok;
coolPrint(Pid,[Width,I]) ->
    Pid ! {self(),state_querry},
    receive
        {_, state_querry_reply, _, _} ->
			if 
                (I rem (Width)) == 0 -> 
                    io:format("~n",[]);
                true ->
                    ok
            end,
            io:format(" ant  "),
            Pid ! {self(),get_next},
            receive
                {_,next_reply,Next} ->
                    coolPrint(Next,[Width,I+1]);
                _B ->
                    io:format("Received pointless message ~w ~n",[_B])
            end;

        {_, state_querry_reply, State} ->
            if 
                (I rem (Width)) == 0 -> 
                    io:format("~n",[]);
                true ->
                    ok
            end,
            io:format("~w ",[State]),
            Pid ! {self(),get_next},
            receive
                {_,next_reply,Next} ->
                    coolPrint(Next,[Width,I+1]);
                _B ->
                    io:format("Received pointless message ~w ~n",[_B])
            end;
        _A ->
            io:format("recieved pointless message ~w ~n",[_A])
    end.



testToGet({X,Y},{Width,Height},Array) when X < Width, X >= 0, Y < Height, Y >= 0 ->
    get2D({X,Y},Array);

testToGet(_,_,_)->
    none.


getNext({X,Y},{Width,Height}) when X == (Width-1),Y == (Height-1 ) ->
    none;
getNext({X,Y},{Width,_}) when X == (Width-1) ->
    {0,Y+1};
getNext({X,Y},_) ->
    {X+1,Y}.
    
    
-spec get2D({X::integer(),Y::integer()},Array::array:array()) -> _A.
get2D({X,Y},Array) ->
    array:get( Y, array:get(X, Array) ).

-spec set2D({X::integer(),Y::integer()},_Value,Array::array:array()) -> array:array().
set2D({X,Y},Value,Array) ->
    %io:format("Trying to set ~w with ~w ~n",[{X,Y},Value]),
    Y_array = array:get( X, Array ),
    New_y_array = array:set( Y, Value, Y_array ),
    array:set( X, New_y_array, Array ).

%% ====================================================================
%% Internal functions
%% ====================================================================


