-module(grafikerl). 
-export([runPythonInstance/0]). 

runPythonInstance() -> 

    {ok, P} = python:start([{python, "python3"}]), python:call(P, grafikpy, drawAnt, []).
