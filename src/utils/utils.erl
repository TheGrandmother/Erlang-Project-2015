-module(utils).
-import(test,[cellstarter/0]).
-import(silly,[testShit/0,get2D/2,coolPrint/2]).
-import_type(types,[cell/0]). 
-export([getHood/1,setHood/2,getPos/1,setPos/2, getPid/1,getNext/1,getAttributes/1,setAttributes/2,getMetadata/1,getProperty/2]).


%%Get and setters of Cell type
getHood(Cell = {_,_,Hood,_,_,_}) ->
    Hood.
setHood(Cell = {Pid, Position,_,Next_Cell,Attributes,Metadata}, New_Hood) ->
    {Pid,Position,New_Hood, Next_Cell,Attributes,Metadata}.
getPos(Cell = {_,Pos = {X,Y},_,_,_,_}) ->
    Pos.
setPos(Cell = {Pid,_,Hood, Next_Cell, Attributes, Metadata},New_Pos = {X,Y}) ->   
    {Pid, New_Pos, Hood, Next_Cell, Attributes, Metadata}.
getPid(Cell = {Pid,_,_,_,_,_}) ->
    Pid.
getNext(Cell = {_,_,_,Next_Cell,_,_}) ->
    Next_Cell.
getAttributes(Cell = {_,_,_,_,Attributes,_}) ->
    Attributes.
setAttributes(Cell = {Pid,Position,Hood, Next_Cell, _, Metadata}, New_Attr) ->
    {Pid,Position,Hood, Next_Cell,New_Attr,Metadata}.
getMetadata(Cell = {_,_,_,_,_,Metadata}) ->
    Metadata.
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








%%-spec changeColor(Cell::cell(),Color::color()) -> ok.
%%changeState/2
%%changes the state to color on current cell,
%changeState(Pid, Color) ->
%    Pid ! {self(), set_state, Color},
%    receive 
%	_->
%	    ok
%    end.

%%getHood/1
%%gets cells around current cell(Pid)
%getHood(Pid) ->
%    Pid ! {self(), querry_hood},
%    receive
%	Cell ->
%	    Cell;
%	_ ->
%	    none
%    end.

%%getNext/1
%%Returns the next cell from current Pid
%getNext(Pid) ->
%    Pid ! {self(), get_next},
%    receive
%	{_,_,NextPid} ->
%	    NextPid;
%	_ ->
%	    none
%    end.
%


getAnt_Pid(_Ant = {Pid, _, _, _, _}) ->
	Pid.

getAnt_Cell(_Ant = {_, Cell, _, _, _}) ->
	Cell.

getAnt_State(_Ant = {_, _, State, _, _}) ->
	State.

getAnt_Attributes(_Ant = {_, _ , _, Attributes, _}) -> 
	Attributes.

getAnt_Metadata(_Ant = {_, _, _, _, Metadata}) ->
	Metadata.

setAnt_Cell(_Ant = {Pid, _, State, Attributes, Metadata}, NewCell) ->
	{Pid, NewCell, State, Attributes, Metadata}.

setAnt_State(_Ant = {Pid, Cell, _, Attributes, Metadata}, NewState) ->
	{Pid, Cell, NewState, Attributes, Metadata}.

setAnt_Attributes(_Ant = {Pid, Cell, State, _, Metadata}, NewAttributes) ->
	{Pid, Cell, State, NewAttributes, Metadata}.
	
setAnt_Metadata(_Ant = {Pid, Cell, State, Attributes, _}, NewMetadata) ->
	{Pid, Cell, State, Attributes, NewMetadata}.
	
	