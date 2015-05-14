-module(erlPY).
-export([runPythonInstance/0]).

list1 = ["food", "ant", "foodant", "plain", "foodant", "food", "plain", "plain", "plain", "foodant", "foodant", "ant", "plain", "plain", "plain", "food", "ant", "plain", "plain", "foodant", "food", "ant", "ant", "plain", "food"]

list2 = ["foodant", "ant", "plain", "plain", "foodant", "plain", "plain", "plain", "plain", "foodant", "foodant", "ant", "plain", "plain", "plain", "plain", "ant", "plain", "plain", "foodant", "food", "plain", "plain", "plain", "food"]

 
runPythonInstance() ->
 
    {ok, P} = python:start([{python, "python3"}]), 
    python:call(P, grafikpy, createGrid, [5, 5]),
    python:call(P, grafikpy, drawGrid, [list1]),
    timer:sleep(10000),
    python:call(P, grafikpy, drawGrid, [list2]).
