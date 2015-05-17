%% @author grandmother
%% @doc @todo Add description to ant_tests.


-module(ant_tests).
-include_lib("eunit/include/eunit.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% =====================================================================================
%% Tests
%% =====================================================================================


spawnTest() ->
    logger:initLogger(),
    Grid = grid_init:initGrid({2,1}),
    Cell_ID = grid_init:getGridElement({0,0},{2,1},Grid),
    Ant = ant:spawnAnt(Cell_ID, []),
    timer:sleep(2),
    Ant ! {self(),make_ref(),ping},
    receive
        {_,_,_,pong} ->
            ok;
        _Any ->
            ?debugFmt("Received idiotic message ~p instead of pong",[_Any]),
            ?assert(false)
    end,
    true.

%hoodProcessor_test() ->
%    Really_Annoying_To_Write = {
%                                none, 
%                                #{feremones => #{food_feremone => {4.0,0}, base_feremone => {4.0,0}},type => plain}, 
%                                #{feremones => #{food_feremone => {7.0,0}, base_feremone => {7.0,0}},type => plain},
%                                #{feremones => #{food_feremone => {2.0,0}, base_feremone => {2.0,0}},type => plain}, 
%                                #{feremones => #{food_feremone => {5.0,0}, base_feremone => {5.0,0}},type => plain}, 
%                                #{feremones => #{food_feremone => {8.0,0}, base_feremone => {8.0,0}},type => plain}, 
%                                #{feremones => #{food_feremone => {3.0,0}, base_feremone => {3.0,0}},type => block}, 
%                                #{feremones => #{food_feremone => {6.0,0}, base_feremone => {6.0,0}},type => plain}, 
%                                #{feremones => #{food_feremone => {9.0,0}, base_feremone => {9.0,0}},type => plain} 
%                               },
%    True = {southeast,east,northeast,south,north,west,southwest,northwest},
%    Result1 = list_to_tuple(element(1,lists:unzip(tuple_to_list(ant:processHood(Really_Annoying_To_Write,base_feremone))))),
%    Result2 = list_to_tuple(element(1,lists:unzip(tuple_to_list(ant:processHood(Really_Annoying_To_Write,food_feremone))))).
    %[?assertEqual(True,list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,base_feremone)))))),
    %?assertEqual(True, list_to_tuple(element(1,lists:unzip(tuple_to_list(processHood(Really_Annoying_To_Write,food_feremone))))))].

directionPickerAux(0,Cool_Tuple) ->
    Cool_Tuple;
directionPickerAux(N,Cool_Tuple) ->
    Thing = {{1,0},{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},{8,0}},
    Direction = ant:pickDirection(Thing),
    Old_Count = element(2,element(Direction,Cool_Tuple)),
    New_Tuple = setelement(Direction,Cool_Tuple,  {Direction,Old_Count+1}),
    directionPickerAux(N-1,New_Tuple).
    

%directionPicker_test() ->
%    {A1,A2,A3} = now(),
%    random:seed(A1, A2, A3),
%    Thing = {{1,0},{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},{8,0}},
%    {A,B,C,D,E,F,_,_}  = directionPickerAux(10000, Thing),
%    Fixed = {A,B,C,D,E,F,{7,1},{8,0}},
%    %?debugFmt("Cool histogram is : ~p",[Fixed]),
%    Sorted= sorter:sort(Fixed),
%    %?debugFmt("Sorted histogram is : ~p",[Sorted]).
%    Lol = element(1,lists:unzip(tuple_to_list(Sorted))),
%    [?assertEqual([1,2,3,4,5,6,7,8],Lol)].

queenWaitingForPickup(Main) ->
    receive
        {Pid,{found_food,Steps}} ->
            ?debugFmt("Our little ant found food in ~p steps",[Steps]),
            Pid ! {self(),stop_ant},
            Main ! {ok};
        _A ->
            ?debugFmt("Our little ant sent me an odd message ~p",[_A]),
            ?assert(false)
    end.

searchForFoodTest()->
    logger:initLogger(),
    Grid = grid_init:initGrid({5,5}),
    Cell_ID = grid_init:getGridElement({2,2},{3,3},Grid),
    My_Pid = self(),
    Queen = spawn(fun() -> queenWaitingForPickup(My_Pid) end),
    Ant = ant:spawnAnt(Cell_ID, Queen),
    Cell_With_Food = grid_init:getGridElement({0,0},{3,3},Grid),
    Ref = make_ref(),
    Cell_With_Food ! {self(),Ref,{set_cell_attribute,{food,1}}},
    receive
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    Ant ! {self(),start_ant},
    receive
        {ok} ->
            ?debugMsg("all is well :)");
        _ ->
            ?assert(false)
    end,

    true.

queenWaitingForPickupAndReturn(Main) ->
    receive
        {Pid,{found_food,Steps}} ->
            ?debugFmt("Our little ant found food in ~p steps",[Steps]),
            
            Main ! {ok},
            receive
                {Pid,{returned_with_food,Steps1}} ->
                    ?debugFmt("Our little returned with the  food in ~p steps",[Steps1]),
                    Pid ! {self(),stop_ant},
                    Main ! {ok};
                _A ->
                    ?debugFmt("Our little ant sent me an odd message ~p",[_A]),
                    ?assert(false)
            end;

        _A ->
            ?debugFmt("Our little ant sent me an odd message ~p",[_A]),
            ?assert(false)
    end.


searchAndReturnTest()->
    logger:initLogger(),
    Grid = grid_init:initGrid({5,5}),
    Cell_ID = grid_init:getGridElement({4,4},{5,5},Grid),
    SillyRef = make_ref(),
    Cell_ID ! {self(),SillyRef,{set_cell_attribute,{type,nest}}},
    receive 
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    My_Pid = self(),
    Queen = spawn(fun() -> queenWaitingForPickupAndReturn(My_Pid) end),
    Ant = ant:spawnAnt(Cell_ID, Queen),
    Cell_With_Food = grid_init:getGridElement({0,0},{5,5},Grid),
    Ref = make_ref(),
    Cell_With_Food ! {self(),Ref,{set_cell_attribute,{food,1}}},
    receive
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            %?debugFmt("Added food to ~p",[Cell_With_Food]);
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    Ant ! {self(),start_ant},

    receive
        {ok} ->
            ?debugMsg("We found food :)");
        _ ->
            ?assert(false)
    end,
    receive
        {ok} ->
            ?debugMsg("all is well :)");
        _ ->
            ?assert(false)
    end,
    true.

backAndFourth()->
    logger:initLogger(),
    Grid = grid_init:initGrid({6,6}),
    Cell_ID = grid_init:getGridElement({5,5},{6,6},Grid),
    SillyRef = make_ref(),
    Cell_ID ! {self(),SillyRef,{set_cell_attribute,{type,nest}}},
    receive 
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            %?debugFmt("Added food to ~p",[Cell_With_Food]);
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    My_Pid = self(),
    Queen = spawn(fun() -> complicatedQueen(My_Pid,0,[],7) end),
    Ant = ant:spawnAnt(Cell_ID, Queen),
    Cell_With_Food = grid_init:getGridElement({0,0},{6,6},Grid),
    Ref = make_ref(),
    Cell_With_Food ! {self(),Ref,{set_cell_attribute,{food,100}}},
    receive
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    Ant ! {self(),start_ant},

    receive
        {ok,Diffs} ->
            ?debugMsg("We sured did a lot of things :)"),
            ?debugFmt("This is how many steps every thing took ~p",[lists:reverse(Diffs)]),
            timer:sleep(500);
        _ ->
            ?assert(false)
    end,
    true.


derpQueen(Main,_,Find_Diffs,Return_Diffs,0) ->
    Main ! {ok,Find_Diffs,Return_Diffs};
derpQueen(Main,Map,Find_Diffs,Return_Diffs,N) ->
    receive
        {Pid,{found_food,Steps}} ->
            Old_Steps = maps:get(Pid,Map,0),
            Diff = Steps-Old_Steps,
            New_Map = maps:put(Pid,Steps,Map),
            ?debugFmt("Our little ant ~p found food in ~p steps",[Pid,Diff]),
            derpQueen(Main,New_Map,[Diff]++Find_Diffs,Return_Diffs,N-1);
        
        {Pid,{returned_with_food,Steps}} ->
            Old_Steps = maps:get(Pid,Map,0),
            Diff = Steps-Old_Steps,
            New_Map = maps:put(Pid,Steps,Map),
            ?debugFmt("Our little ant ~p returned the food in ~p steps",[Pid,Diff]),
            derpQueen(Main,New_Map,Find_Diffs,[Diff]++Return_Diffs,N-1);
        
        _A ->
            ?debugFmt("Our little ant sent me an odd message ~p",[_A]),
            ?assert(false)
    end.
    
complicatedQueen(Main,_,Diffs,0) ->
    Main ! {ok,Diffs};
complicatedQueen(Main,Last_Step,Diffs,N) ->
    receive
        {_,{found_food,Steps}} ->
            ?debugFmt("Our little ant found food in ~p steps",[Steps-Last_Step]),
            complicatedQueen(Main,Steps,[Steps-Last_Step]++Diffs,N-1);
        
        {_,{returned_with_food,Steps}} ->
            ?debugFmt("Our little ant returned the food in ~p steps",[Steps-Last_Step]),
            complicatedQueen(Main,Steps,[Steps-Last_Step]++Diffs,N-1);
        _A ->
            ?debugFmt("Our little ant sent me an odd message ~p",[_A]),
            ?assert(false)
    end.


multiAntTest()->
    logger:initLogger(),
    Grid = grid_init:initGrid({6,6}),
    Cell_ID = grid_init:getGridElement({5,5},{6,6},Grid),
    SillyRef = make_ref(),
    Cell_ID ! {self(),SillyRef,{set_cell_attribute,{type,nest}}},
    receive 
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            %?debugFmt("Added food to ~p",[Cell_With_Food]);
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    My_Pid = self(),
    Queen = spawn(fun() -> derpQueen(My_Pid,#{},[],[],10) end),
    Ant1 = ant:spawnAnt( grid_init:getGridElement({5,5},{6,6},Grid), Queen),
    Ant2 = ant:spawnAnt( grid_init:getGridElement({5,4},{6,6},Grid), Queen),
    Ant3 = ant:spawnAnt( grid_init:getGridElement({4,5},{6,6},Grid), Queen),
    Cell_With_Food = grid_init:getGridElement({0,0},{6,6},Grid),
    Ref = make_ref(),
    Cell_With_Food ! {self(),Ref,{set_cell_attribute,{food,100}}},
    receive
        {_,_,_,{reply,set_cell_attribute,sucsess}} ->
            ok;
        _ ->
            ?debugMsg("Failed to place food at cell :("),
            ?assert(false)
    end,
    Ant1 ! {self(),start_ant},
    Ant2 ! {self(),start_ant},
    Ant3 ! {self(),start_ant},

    receive
        {ok,Find_Diffs,Return_Diffs} ->
            ?debugMsg("We sured did a lot of things :)"),
            ?debugFmt("This is how many steps every thing took to find ~p",[lists:reverse(Find_Diffs)]),
            ?debugFmt("This is how many steps every thing took to return ~p",[lists:reverse(Return_Diffs)]),
            timer:sleep(500);
        _ ->
            ?assert(false)
    end,
    true.

spawn_test() ->
    [?assert(spawnTest())].

searchForFood_test_() ->
         {timeout, 20, [fun searchForFoodTest/0]}.

searchAndReturn_test_() ->
         {timeout, 30, [fun searchAndReturnTest/0]}.

multiCycle_test_() ->
         {timeout, 100, [fun backAndFourth/0]}.

multiAnt_test_() ->
         {timeout, 100, [fun multiAntTest/0]}.

%% ====================================================================
%% Internal functions
%% ====================================================================


