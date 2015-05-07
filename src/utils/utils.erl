-module(utils).
-export([getCellHood/1, setCellHood/2,
		 getCellPos/1, setCellPos/2, getCellPid/1, setCellPid/2,
		 getCellNext/1, setCellNext/2, getCellAttributes/1, setCellAttributes/2,
		 getCellMetadata/1, setCellMetadata/2, getProperty/2, initCell/1, getOneDirection/2,
		 getAntAttributes/1, getAntState/1, getAntMetadata/1, getAntCell/1, getAntPid/1,
		 setAntCell/2, setAntState/2, setAntAttributes/2, setAntMetadata/2,getCellLog/1,setCellLog/2,getAntProperty/2]).

%%@doc initiates a new cell with the position coordinates
-spec initCell(Position::types:position()) -> types:cell().
initCell(Position = {_X,_Y}) ->
    {self(), Position,none,none,none,none}.

%%@doc returns the Pid of the argumented direction from the neighborhood
-spec getOneDirection(Cell::types:cell(),Direction::types:direction()) -> Pid.
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

%%@doc Returns the neighborhood of the cell
-spec getCellHood(Cell::types:cell()) -> types:neighbourhood().
getCellHood(_Cell = {_,_,Hood,_,_,_,_}) ->
    Hood.
%%@doc sets the neighborhood in cell, and returns the cell
setCellHood(_Cell = {Pid, Position,_,Next_Cell,Attributes,Metadata,Log}, New_Hood) ->
    {Pid,Position,New_Hood, Next_Cell,Attributes,Metadata,Log}.
%%@doc Returns the coordinates tuple from cell
getCellPos(_Cell = {_,Pos = {_X,_Y},_,_,_,_,_}) ->
    Pos.
%%@doc sets the coordinates of the cell position, and returns the new cell
setCellPos(_Cell = {Pid,_,Hood, Next_Cell, Attributes, Metadata, Log},New_Pos = {_X,_Y}) ->   
    {Pid, New_Pos, Hood, Next_Cell, Attributes, Metadata,Log}.
%%@doc Returns the Cell pid
getCellPid(_Cell = {Pid,_,_,_,_,_,_}) ->
    Pid.
%%@doc sets the cell pid and returns the new cell
setCellPid(_Cell = {_,Position, Hood,Next_Cell, Attributes, Metadata,Log},New_Pid) ->
    {New_Pid,Position,Hood,Next_Cell,Attributes, Metadata,Log}.
%%@doc returns the cell that's next to current one
getCellNext(_Cell = {_,_,_,Next_Cell,_,_,_}) ->
    Next_Cell.
%%@doc Sets which cell that's next to current one, and returns the new cell
setCellNext(_Cell = {Pid,Position,Hood, _, Attributes, Metadata,Log},New_Nextcell) ->
    {Pid,Position,Hood, New_Nextcell, Attributes, Metadata,Log}.
%%@doc Returns the attributes list
getCellAttributes(_Cell = {_,_,_,_,Attributes,_,_}) ->
    Attributes.
%%@doc Sets the cell attributes, and returns the new cell
setCellAttributes(_Cell = {Pid,Position,Hood, Next_Cell, _, Metadata,Log}, New_Attr) ->
    {Pid,Position,Hood, Next_Cell,New_Attr,Metadata,Log}.
%%@doc Returns the Metadata of the cell
getCellMetadata(_Cell = {_,_,_,_,_,Metadata,_}) ->
    Metadata.
%%@doc Sets the metadata of the cell, and returns the new cell
setCellMetadata(_Cell = {Pid,Position,Hood, Next_Cell, Attributes, _,Log}, New_metadata) ->
    {Pid,Position,Hood,Next_Cell,Attributes,New_metadata,Log}.
%%@doc Returns the log of cell
getCellLog(_Cell = {_,_,_,_,_,_,Log}) ->
    Log.
%%@doc Sets the log of the cell and returns the new cell
setCellLog(_Cell = {Pid,Position,Hood, Next_Cell, Attributes, Metadata,_}, New_Log) ->
    {Pid,Position,Hood,Next_Cell,Attributes,Metadata,New_Log}.

%%getProperty/2
% Returns the Tuple in list that matches property.
%%@doc Returns the tuple in the list that matches property
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

%%@doc Returns the ant pid
getAntPid(_Ant = {Pid, _, _, _, _}) ->
	Pid.
%%@doc Returns the Ants cell
getAntCell(_Ant = {_, Cell, _, _, _}) ->
	Cell.
%%@doc Returns the state of ant
getAntState(_Ant = {_, _, State, _, _}) ->
	State.
%%@doc Returns the ant attributes
getAntAttributes(_Ant = {_, _ , _, Attributes, _}) -> 
	Attributes.
%%@doc Returns the ants metadata
getAntMetadata(_Ant = {_, _, _, _, Metadata}) ->
	Metadata.
%%@doc Sets the cell of ant, and returns the new ant
setAntCell(_Ant = {Pid, _, State, Attributes, Metadata}, NewCell) ->
	{Pid, NewCell, State, Attributes, Metadata}.
%%@doc Sets the state of ant, and returns the new ant
setAntState(_Ant = {Pid, Cell, _, Attributes, Metadata}, NewState) ->
	{Pid, Cell, NewState, Attributes, Metadata}.
%%@doc Sets the ants atributes, and returns the new ant
setAntAttributes(_Ant = {Pid, Cell, State, _, Metadata}, NewAttributes) ->
	{Pid, Cell, State, NewAttributes, Metadata}.
%%@doc Sets the ants metadata, and returns the new ant	
setAntMetadata(_Ant = {Pid, Cell, State, Attributes, _}, NewMetadata) ->
	{Pid, Cell, State, Attributes, NewMetadata}.


getAntProperty(_Ant = {_, _, _, Attributes, _, _},Keyword) ->	
    maps:get(Keyword,Attributes).
