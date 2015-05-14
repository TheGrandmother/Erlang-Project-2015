-module(gui).

-export([main/2,initList/2,addToList/4,sendToPyt/1,test/1]).
-define(DEFAULT_UPDATE_TIME,200).
%%@ Receives messages from actors and puts them in list that will be sent to Python, Width and Height argument is 
%% the size of entire grid. When receiving message X,Y is location of the cell.
main(AddList,{Width, Height})->
    receive
	    {_Pid,{gui_update,{X,Y},Attributes}} -> 
		    L = addToList(AddList,{X,Y},{Width,Height},modifyAttributes(Attributes)),
		    main(L,{Width,Height});
	    {_Pid, {gui_init, {X, Y}}} ->
		    sendInitPyt({X,Y}),
		    L = initList(X*Y, []),
		    main(L , {X,Y});
	    _Any ->
		    exit(fail)
    after ?DEFAULT_UPDATE_TIME ->
	    sendToPyt(AddList)
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
    {Head,[Hd | Tl]} = lists:split(Place, L),
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
    python:stop(p).
    
%%@Modifys the map to a tuple of information    
modifyAttributes(Attributes) ->
    StateAnt = maps:get(ant, Attributes),
    StateFood = maps:get(food, Attributes),
    checkState(StateAnt,StateFood).	
%%@ Aux function to modifyAttributes, helps with patternmatching
checkState(none, Food) when Food > 0 ->
    food;
checkState(_Pid, Food) when Food > 0 ->
    antfood;    
checkState(none,0) ->
    none;
checkState(_Pid,0) ->
    ant.

    
    
test({X,Y}) ->
    L = initList((X*Y),[]),
    L2 = addToList(L,{1,0},{X,Y},modifyAttributes(#{ant => none, feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
    L3 = addToList(L2,{0,2},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 1, type => plain})),
    L4 = addToList(L3,{2,1},{X,Y},modifyAttributes(#{ant => self(), feremones => #{base_feremone => {0.0,1.01},food_feremone => {0.0,1.01}}, food => 0, type => plain})),
    testPrint2(L4,X,1).

testPrint2([],_,_) ->
    io:format("");
testPrint2([L],_,_) ->
    io:format("~p",[L]);
testPrint2([Hd | Tl],X,Acc) ->
    io:format(" ~p ",[Hd]),
    if
      ((Acc rem X) == 0) ->
	  io:format(" ~n"),
	  testPrint2(Tl,X,Acc+1);
      true ->
	  testPrint2(Tl,X,Acc+1)
    end.
    
testPrint([]) ->
  none;
testPrint([ant]) ->
    io:format( "ant");
testPrint([antFood]) ->
    io:format( "antFood");
testPrint([food]) ->
    io:format( "food");
testPrint([none]) ->
    io:format( "none");
testPrint([ant | Tl]) ->
    io:format(" ant "),
    testPrint(Tl);
testPrint([antFood | Tl]) ->
    io:format(" antFood "),
    testPrint(Tl);
testPrint([food | Tl]) ->
    io:format(" food "),
    testPrint(Tl);
testPrint([none | Tl]) ->
    io:format(" none "),
    testPrint(Tl).
    