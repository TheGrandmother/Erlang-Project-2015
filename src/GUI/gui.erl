-module(gui).

-export([main/3,initList/2,addToList/4,sendToPyt/2,test/1,sendInitPyt/2,gui_init/0]).
-define(DEFAULT_UPDATE_TIME,5).

gui_init() ->
    My_Pid = self(),
    {ok, P} = python:start([{python, "python3"}]),
    spawn(fun() -> grid_init:buildAndStartSimpleWorld(My_Pid) end),
    main([],{9,9},P).
%%@ Receives messages from actors and puts them in list that will be sent to Python, Width and Height argument is 
%% the size of entire grid. When receiving message X,Y is location of the cell.
main(AddList,{Width, Height},P)->
    receive
	    {_Pid,{gui_update,{{X,Y},Attributes}}} -> 
% 	    io:format("received message, updating list with cells"),
		    L = addToList(AddList,{X,Y},{Width,Height},modifyAttributes2(Attributes)),
		    sendToPyt(AddList,P),
		    main(L,{Width,Height},P);
	    {_Pid, {gui_init, {X, Y}}} ->
		    sendInitPyt({X,Y},P),
%                     io:format("received message, building grid ~p",[{X,Y}]),
		    L = initList(X*Y, []),
		    main(L , {X,Y},P);
	    _Any ->
	    io:format("WRONG MESSAGE: ~p ~n",[_Any]),
	    exit(fail)
	    %%main(AddList,{Width,Height})
    after ?DEFAULT_UPDATE_TIME ->
	    sendToPyt(AddList,P),
% 	    io:format("Dumping list to python"),
	    main(AddList,{Width,Height},P)
		
    end.
%%@ Initiates the list with empty cells
initList(0, L) ->
    L;
initList(Size, L) ->
    B = L ++ [none],
    initList(Size-1,B).
%%@ Adds the attributes to the right location of the list with cells.
addToList(L,{X,Y},{Width,_Height},Attributes) ->
    Place = Y*Width+X,
    {Head,[_Hd | Tl]} = lists:split(Place, L),
    Head ++ [Attributes] ++ Tl.
    
%%@Sends the list to python, in python a message handler is required.    
sendToPyt(L,P) ->
   python:cast(P,L).

%%@Sends initial information to python, only called first at start
sendInitPyt(_Size = {X,Y},P) ->
    python:call(P, xd, createGrid, [X, Y]),
    python:call(P,xd,register_handler,[self()]).
    
%%@Retrieves information from the map and returns single atom(This is used for testing).
modifyAttributes(Attributes) ->
    StateAnt = maps:get(ant, Attributes,none),
    StateFood = maps:get(food, Attributes),
    StateType = maps:get(type, Attributes),
    checkState(StateAnt,StateFood,StateType).	
%%@Retrieves information from the map and returns a tuple of information
modifyAttributes2(Attributes) ->
    StateAnt = isAnt(maps:get(ant, Attributes,none)),
    StateFood = isFood(maps:get(food, Attributes)),
    StatePheromone = checkPheromone(maps:get(feremones, Attributes)),
    StateType = maps:get(type, Attributes),
    modifyAux({StateType,StateAnt,StateFood,StatePheromone}).
    
%%@Takes out the single most important value for the cell
modifyAux(_L = {block,_Ant,_Food,_Feremone}) ->
    block;
modifyAux(_L = {nest,_State,_Food,_Feremone}) ->
    nest;
modifyAux(_L = {_Type,ant,food,_Feremone}) ->
    foodant;
modifyAux(_L = {_Type,ant,_Food,_Feremone}) ->
    ant;
modifyAux(_L = {_Type,_State,food,_Feremone}) ->
    food;
modifyAux(_L = {plain,_State,_Food,_Feremone}) ->
    plain.

    
%%@if its a pid its an ant.
isAnt(none) ->
    none;
isAnt(_Pid) ->
    ant.
%%@If it's greater than zero its food.
isFood(0) ->
    none;
isFood(_) ->
    food.
%%@Returns the pheromones in a tuple
checkPheromone(none) ->
    none;
checkPheromone(Feromones) ->
    Base = maps:get(base_feremone,Feromones),
    Food = maps:get(food_feremone,Feromones),
    {Base,Food}.
%%@ Aux function to modifyAttributes, helps with patternmatching

checkState(none, Food,_Type) when Food > 0 ->
    "food";
checkState(_Pid, Food,_Type) when Food > 0 ->
    "foodant";    
checkState(none,0,plain) ->
    "plain";
checkState(_Pid,0,_Type) ->
    "ant".

    
    
test({X,Y}) ->
    L = initList((X*Y),[]),
    L2 = addToList(L,{0,0},{X,Y},modifyAttributes(#{ant => none, feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
    L3 = addToList(L2,{2,0},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
    L4 = addToList(L3,{1,0},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 0, type => plain})),
    L4.

testPrint2([],_,_) ->
    io:format("");
testPrint2([L],_,_) ->
    io:format("~p ~n",[L]);
testPrint2([Hd | Tl],X,Acc) ->
    io:format(" ~p ",[Hd]),
    if
      ((Acc rem X) == 0) ->
	  io:format(" ~n"),
	  testPrint2(Tl,X,Acc+1);
      true ->
	  testPrint2(Tl,X,Acc+1)
    end.
    
