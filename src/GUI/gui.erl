-module(gui).

-export([main/2,initList/2,addToList/4,sendToPyt/1,test/1,sendInitPyt/1,gui_init/0]).
-define(DEFAULT_UPDATE_TIME,200).

gui_init() ->
    My_Pid = self(),
    spawn(fun() -> grid_init:buildAndStartSimpleWorld(My_Pid) end),
    main([],{9,9}).
%%@ Receives messages from actors and puts them in list that will be sent to Python, Width and Height argument is 
%% the size of entire grid. When receiving message X,Y is location of the cell.
main(AddList,{Width, Height})->
    receive
	    {_Pid,{gui_update,{{X,Y},Attributes}}} -> 
	    io:format("received message, updating list with cells"),
		    L = addToList(AddList,{X,Y},{Width,Height},modifyAttributes(Attributes)),
		    main(L,{Width,Height});
	    {_Pid, {gui_init, {X, Y}}} ->
		    %sendInitPyt({X,Y}),
	    io:format("received message, building grid ~p",[{X,Y}]),
		    L = initList(X*Y, []),
		    main(L , {X,Y});
	    _Any ->
	    io:format("WRONG MESSAGE: ~p ~n",[_Any]),
	    exit(fail)
	    %%main(AddList,{Width,Height})
    after ?DEFAULT_UPDATE_TIME ->
	    %sendToPyt(AddList)
	    io:format("Dumping list to python"),
	    testPrint2(AddList, Width, 0),
	    main(AddList,{Width,Height})
		
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
sendToPyt(L) ->
    {ok, P} = python:start(),
    python:call(P, handler, register_handler, [self()]),
    python:cast(P,L),
    %flush(),
    python:stop(P).

%%@Sends initial information to python, only called first at start
sendInitPyt(Size = {_X,_Y}) ->
    {ok, P} = python:start(),
    python:call(P, init_handler, init_register_handler,[self()]),
    python:cast(P,Size),
    %flush(),
    python:stop(P).
    
%%@Retrieves information from the map and returns single atom(This is used for testing).
modifyAttributes(Attributes) ->
    StateAnt = maps:get(ant, Attributes,none),
    StateFood = maps:get(food, Attributes),
    checkState(StateAnt,StateFood).	
%%@Retrieves information from the map and returns a tuple of information
modifyAttributes2(Attributes) ->
    StateAnt = isAnt(maps:get(ant, Attributes,none)),
    StateFood = isFood(maps:get(food, Attributes)),
    StatePheromone = checkPheromone(maps:get(feremones, Attributes)),
    StateType = maps:get(type, Attributes),
    {StateType,StateAnt,StateFood,StatePheromone}.
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
checkState(none, Food) when Food > 0 ->
    "food";
checkState(_Pid, Food) when Food > 0 ->
    "foodant";    
checkState(none,0) ->
    "none";
checkState(_Pid,0) ->
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
    
