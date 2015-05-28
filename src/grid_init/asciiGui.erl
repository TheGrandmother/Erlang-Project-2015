%% @author grandmother
%% @doc @todo Add description to asciiGui.


-module(asciiGui).

%% ====================================================================
%% API functions
%% ====================================================================
-export([initThingy/0]).


initThingy() ->
    My_Pid = self(),
    World = spawn(fun() -> grid_init:buildAndStartSimpleWorld(My_Pid) end),
     io:format("LOOOOOOPfewewfewffew"),
    receive
        {_,{gui_init,{X,Y}}} ->
            io:format("LOOOOOOPdsfwertwrtw5t32254"),
            loopMe(makeGrid(X,Y),{X,Y},getTimeStamp());
        _A->
            io:format("Derp  ~p ~n",[_A])
    end.

loopMe(Grid,Size,Timer) ->
    receive
       {_,{gui_update,{Pos,Map}}} ->
            New_Grid = setGridElement(Pos, getSymbol(Map), Grid),
            
            Time = getTimeStamp() - Timer,
            if
                Time >= 10000 ->
                    draw(Size, New_Grid),
                    loopMe(New_Grid, Size,getTimeStamp());
                true ->
                    loopMe(New_Grid, Size,Timer)
                    
            end;
                    
            
        _ ->
            io:format("FAIL AT LAJF!"),
            exit(failure)
    end.

getTimeStamp() ->
    {MegaSecs,Secs,MicroSecs} = erlang:now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.



getSymbol(Map) ->
    case is_pid(maps:get(ant,Map,none)) of
        true ->
            case maps:get(ant_state,Map,none) of
				idling ->
					"I";
				searching_for_food ->
					"S";
				returning_with_food ->
					"R";
				_A ->
					io:format("Received idiotic ant state ~p.",[_A]),
					exit(failure)
			
			end;
        false ->
            case maps:get(type,Map) of
                block ->
                    "B";
                nest ->
                    "N";
                plain ->
                    case maps:get(food,Map) of
                        0 ->
                            ".";
                        _ ->
                            "F"
                    end
            end     
    end.
        


draw(Size,Grid) ->
    io:format("\x1b[2J\x1b[1;1H"),
    drawAux({0,0},Size,Grid).

drawAux({X,Y},{Width,Height},Grid) when Y == Height ->
    ok;

drawAux({X, Y},{Width, Height}, Grid) when X == Width->
    io:format("~n"),
    drawAux({0, Y+1},{Width,Height},Grid);

drawAux({X,Y},Grid_Size,Grid) ->
        io:format("~c ",getGridElement({X,Y}, Grid_Size, Grid)),
        drawAux({X+1,Y},Grid_Size,Grid).



makeGrid(Width,Height) ->
    array:new([{size, Width}, {default, array:new([{size, Height}, {default, "."}])}]).

getGridElement({X,Y},{Width,Height},Array) when X < Width, X >= 0, Y < Height, Y >= 0 ->
    array:get(Y, array:get(X,Array));
getGridElement(_,_,_) ->
    none.

%% set_Grid_Element should be moved to another module
-spec setGridElement({X::integer(), Y::integer()},_Value, Array::array:array()) -> array:array().
%% Sets an element in the provided two-dimensional array to given value
setGridElement({X, Y}, Value, Array) ->
    Y_Array = array:get(X, Array),
    New_Y_Array = array:set(Y, Value, Y_Array),
    array:set(X, New_Y_Array, Array).

%% ====================================================================
%% Internal functions
%% ====================================================================


