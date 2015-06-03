%% @author Oscar Wallster
%% @doc Module for handling the gui and erlang-python communication

-module(gui).

-export([initGui/0]).
-define(DEFAULT_UPDATE_TIME,5).


%%@doc This function will start the python Gui with a simple test world :D
initGui() ->
    My_Pid = self(),
    {ok, P} = python:start([{python, "python3"}]),
    spawn(fun() -> grid_init:buildAndStartSimpleWorld(My_Pid) end),
    main([],{9,9},P).
%%@doc Receives messages from actors and puts them in list that will be sent to Python, Width and Height argument is 
%% the size of entire grid. When receiving message X,Y is location of the cell.
main(_,{Width, Height},P)->
    receive
	    {_Pid,{gui_update,{{X,Y},Attributes}}} -> 
% 	    io:format("received message, updating list with cells"),
		    %L = addToList(AddList,{X,Y},{Width,Height},modifyAttributes2(Attributes)),
		    sendToPyt({{X,Y},modifyAttributes2(Attributes)},P),
		    main(none,{Width,Height},P);
	    {_Pid, {gui_init, {X, Y}}} ->
		    sendInitPyt({X,Y},P),
%                     io:format("received message, building grid ~p",[{X,Y}]),
		    %L = initList(X*Y, []),
		    main(none , {X,Y},P);
	    _Any ->
	    io:format("WRONG MESSAGE: ~p ~n",[_Any]),
	    exit(fail)
	    %%main(AddList,{Width,Height})
    after ?DEFAULT_UPDATE_TIME ->
	    %sendToPyt(AddList,P),
% 	    io:format("Dumping list to python"),
	    main(none,{Width,Height},P)
		
    end.
%%@doc Initiates the list with empty cells
initList(0, L) ->
    L;
initList(Size, L) ->
    B = L ++ [none],
    initList(Size-1,B).
%%@doc Adds the attributes to the right location of the list with cells.
addToList(L,{X,Y},{Width,_Height},Attributes) ->
    Place = Y*Width+X,
    {Head,[_Hd | Tl]} = lists:split(Place, L),
    Head ++ [Attributes] ++ Tl.
    
%%@doc Sends the list to python, in python a message handler is required.    
sendToPyt(L,P) ->
   python:cast(P,L).

%%@doc Sends initial information to python, only called first at start
sendInitPyt(_Size = {X,Y},P) ->
    python:call(P, xd, createGrid, [X, Y]),
    python:call(P,xd,register_handler,[self()]).
    

%%@doc Retrieves information from the map and returns a tuple of information
modifyAttributes2(Attributes) ->
    StateAnt = isAnt(maps:get(ant, Attributes,none)),
    StateFood = isFood(maps:get(food, Attributes)),
    StatePheromone = checkPheromone(maps:get(feremones, Attributes)),
    StateType = maps:get(type, Attributes),
	StateAntState = maps:get(ant_state, Attributes,none),
    modifyAux({StateType,StateAnt,StateFood,StatePheromone,StateAntState}).
    
%%@doc Takes out the single most important value for the cell
modifyAux(_L = {block,_Ant,_Food,_Feremone,_}) ->
    block;
modifyAux(_L = {nest,_State,_Food,_Feremone,_}) ->
    nest;
modifyAux(_L = {_Type,ant,food,_Feremone,_}) ->
    foodant;
modifyAux(_L = {_Type,ant,_Food,_Feremone,AntState}) ->
    case AntState of 
		idling ->
			idle;
		searching_for_food ->
			searching;
		returning_with_food ->
			returning;
		_A ->
			io:format("Recieved idiotic thing ~p",[_A])
	end;

modifyAux(_L = {_Type,_State,food,_Feremone,_}) ->
    food;
modifyAux(_L = {plain,_State,_Food,_Feremone,_}) ->
    {plain,_Feremone}.

    
%%@doc if its a pid its an ant.
isAnt(none) ->
    none;
isAnt(_Pid) ->
    ant.
%%@doc If it's greater than zero its food.
isFood(0) ->
    none;
isFood(_) ->
    food.
%%@doc Returns the pheromones in a tuple
checkPheromone(none) ->
    none;
checkPheromone(Feromones) ->
    Base = element(1,maps:get(base_feremone,Feromones)),
    Food = element(1,maps:get(food_feremone,Feromones)),
    {Base,Food}.


    
%    
%test({X,Y}) ->
%    L = initList((X*Y),[]),
%    L2 = addToList(L,{0,0},{X,Y},modifyAttributes(#{ant => none, feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
%    L3 = addToList(L2,{2,0},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
%    L4 = addToList(L3,{1,0},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 0, type => plain})),
%    L4.
%
%testPrint2([],_,_) ->
%    io:format("");
%testPrint2([L],_,_) ->
%    io:format("~p ~n",[L]);
%testPrint2([Hd | Tl],X,Acc) ->
%    io:format(" ~p ",[Hd]),
%    if
%      ((Acc rem X) == 0) ->
%	  io:format(" ~n"),
%	  testPrint2(Tl,X,Acc+1);
%      true ->
%	  testPrint2(Tl,X,Acc+1)
%    end.
    
