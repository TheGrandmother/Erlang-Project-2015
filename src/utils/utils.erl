-module(utils).
-export([getCell_Hood/1,setCell_Hood/2,getCell_Pos/1,setCell_Pos/2, getCell_Pid/1,setCell_Pid/2,getCell_Next/1,setCell_Next/2,getCell_Attributes/1,setCell_Attributes/2,getCell_Metadata/1,setCell_Metadata/2,getProperty/2,initCell/1,getOneDirection/2]).

%%initCell -> Cell
initCell(Position = {_X,_Y}) ->
    {self(), Position,none,none,none,none}.

%%Get and setters 0f nichghbourhood
getOneDirection(Cell,northwest) ->
    {NW,_,_,_,_,_,_,_,_} = getCell_Hood(Cell),
    NW;
getOneDirection(Cell,north) ->
    {_,N,_,_,_,_,_,_,_} = getCell_Hood(Cell),
    N;
getOneDirection(Cell,northeast) ->
    {_,_,NE,_,_,_,_,_,_} = getCell_Hood(Cell),
    NE;
getOneDirection(Cell,west) ->
    {_,_,_,W,_,_,_,_,_} = getCell_Hood(Cell),
    W;
getOneDirection(Cell,center) ->
    {_,_,_,_,C,_,_,_,_} = getCell_Hood(Cell),
    C;
getOneDirection(Cell, east) ->
    {_,_,_,_,_,E,_,_,_} = getCell_Hood(Cell),
    E;
getOneDirection(Cell, southwest) ->
    {_,_,_,_,_,_,SW,_,_} = getCell_Hood(Cell),
    SW;
getOneDirection(Cell, south) ->
    {_,_,_,_,_,_,_,S,_} = getCell_Hood(Cell),
    S;
getOneDirection(Cell, southeast) ->
    {_,_,_,_,_,_,_,_,SE} = getCell_Hood(Cell),
    SE;
getOneDirection(_Cell, _) ->
    none.

%%Get and setters of Cell type
getCell_Hood(_Cell = {_,_,Hood,_,_,_}) ->
    Hood.
setCell_Hood(_Cell = {Pid, Position,_,Next_Cell,Attributes,Metadata}, New_Hood) ->
    {Pid,Position,New_Hood, Next_Cell,Attributes,Metadata}.
getCell_Pos(_Cell = {_,Pos = {_X,_Y},_,_,_,_}) ->
    Pos.
setCell_Pos(_Cell = {Pid,_,Hood, Next_Cell, Attributes, Metadata},New_Pos = {_X,_Y}) ->   
    {Pid, New_Pos, Hood, Next_Cell, Attributes, Metadata}.
getCell_Pid(_Cell = {Pid,_,_,_,_,_}) ->
    Pid.
setCell_Pid(_Cell = {_,Position, Hood,Next_Cell, Attributes, Metadata},New_Pid) ->
    {New_Pid,Position,Hood,Next_Cell,Attributes, Metadata}.
getCell_Next(_Cell = {_,_,_,Next_Cell,_,_}) ->
    Next_Cell.
setCell_Next(_Cell = {Pid,Position,Hood, _, Attributes, Metadata},New_Nextcell) ->
    {Pid,Position,Hood, New_Nextcell, Attributes, Metadata}.
getCell_Attributes(_Cell = {_,_,_,_,Attributes,_}) ->
    Attributes.
setCell_Attributes(_Cell = {Pid,Position,Hood, Next_Cell, _, Metadata}, New_Attr) ->
    {Pid,Position,Hood, Next_Cell,New_Attr,Metadata}.
getCell_Metadata(_Cell = {_,_,_,_,_,Metadata}) ->
    Metadata.
setCell_Metadata(_Cell = {Pid,Position,Hood, Next_Cell, Attributes, _}, New_metadata) ->
    {Pid,Position,Hood,Next_Cell,Attributes,New_metadata}.

%%getProperty/2
% Returns the Tuple in list that matches property.
getProperty([],_Property) ->
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
	
	