-module(utils).
-import(test,[cellstarter/0]).
-import(silly,[testShit/0,get2D/2,coolPrint/2]).

-export([startupShiat/0]).

-spec startupShiat() -> ok.
startupShiat() ->
    getProperty([{red,black}, {white,yellow},{color,abuse}],color),
    A0 = testShit(),
    C2d = get2D({4,1}, A0),
    coolPrint(get2D({0,0},A0),[5,0]),
    getHood(C2d).

%%-spec changeColor(Cell::cell(),Color::color()) -> ok.
%%changeState/2
%changes the state to color on current cell,
changeState(Pid, Color) ->
    Pid ! {self(), set_state, Color},
    receive 
	_->
	    ok
    end.

%%getHood/1
%gets cells around current cell(Pid)
getHood(Pid) ->
    Pid ! {self(), querry_hood},
    receive
	Cell ->
	    Cell;
	_ ->
	    none
    end.

%%getNext/1
%Returns the next cell from current Pid
getNext(Pid) ->
    Pid ! {self(), get_next},
    receive
	{_,_,NextPid} ->
	    NextPid;
	_ ->
	    none
    end.
%

    
%%getProperty/2
% Returns the Tuple in list that matches property.
getProperty([],Property) ->
    none;
getProperty([(L = {Type, _Val})],Property) ->
    if
	Type == Property ->
	    L;
	true ->
	    none
    end;
getProperty([(L = {Type, _Val}) | Tl],Property) ->
    if 
        Type == Property ->
	    L;
	true ->
	    getProperty(Tl, Property)
    end.

