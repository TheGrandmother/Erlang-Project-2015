-module(grafikerl).
-export([runPythonInstance/0]).

-import(gui, [test/1]).
 
runPythonInstance() ->
 
    {ok, P} = python:start([{python, "python3"}]),
    python:call(P, nr1, createGrid, [5, 5]),
    python:call(P, nr1, register_handler, [self()]),
    python:cast(P, test({5,5})),
    timer:sleep(5000),
    python:cast(P, ["food", "foodant", "foodant", "ant", "foodant", "food", "plain", "plain", "plain", "ant", "foodant", "ant", "foodant", "ant", "plain", "food", "ant", "plain", "plain", "foodant", "food", "foodant", "foodant", "ant", "food"]).

