-module(cell_tests).
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% TESTS.... of doom!
%% ====================================================================

buildTestWorld() ->
    logger:initLogger(),
        Hood = {
            spawn(fun () -> cell:spawnCell({-1,1}) end),spawn(fun () -> cell:spawnCell({0,1}) end),spawn(fun () -> cell:spawnCell({1,1}) end),
            spawn(fun () -> cell:spawnCell({-1,0}) end),spawn(fun () -> cell:spawnCell({0,0}) end),spawn(fun () -> cell:spawnCell({1,0}) end),
            spawn(fun () -> cell:spawnCell({-1,-1}) end),spawn(fun () -> cell:spawnCell({0,-1}) end),spawn(fun () -> cell:spawnCell({1,-1}) end)
           },
    Center_Cell = element(5,Hood),
    Next_Cell = element(6,Hood),
    Ref = make_ref(),
    Center_Cell ! {self(), Ref,{linkup,Hood,Next_Cell}},
    receive
        _ ->
            ok
    end,
    My_Pid = self(),
    lists:map(fun(X) -> X ! {My_Pid, Ref,{linkup,none,none}} end, [
                                                               element(1,Hood),element(2,Hood),element(3,Hood),
                                                               element(4,Hood),                element(6,Hood),
                                                               element(7,Hood),element(8,Hood),element(9,Hood)
                                                              ]),
    ignoreMessages(8),
    {Center_Cell,Next_Cell,Hood}.

spawnTest() ->
    %?debugMsg("Testing Spawner"),
    logger:initLogger(),
    CellPid = spawn(fun () -> cell:spawnCell({0,0})end),
    %?debugMsg("Spawned cell"),
    Reference = make_ref(),
    CellPid ! {self(),Reference,ping},
    %?debugMsg("Sent message"),
    receive
        {_,_,New_Ref,pong} ->
            %?debugMsg("Received reply message"),
            ?assertEqual(New_Ref,Reference),
            exit(CellPid,sucsess),
            %?debugMsg("Slept and returning"),
            true;
        
        _ ->
            %?debugMsg("Received wrong message whilst testing"),
            exit(CellPid,failure),
            false
    
    after 1000 ->
        %?debugMsg("Ping timed out"),
        false
    
    end.


linkupTest() ->
    logger:initLogger(),
    Hood = {
            spawn(fun () -> cell:spawnCell({-1,1}) end),spawn(fun () -> cell:spawnCell({0,1}) end),spawn(fun () -> cell:spawnCell({1,1}) end),
            spawn(fun () -> cell:spawnCell({-1,0}) end),spawn(fun () -> cell:spawnCell({0,0}) end),spawn(fun () -> cell:spawnCell({1,0}) end),
            spawn(fun () -> cell:spawnCell({-1,-1}) end),spawn(fun () -> cell:spawnCell({0,-1}) end),spawn(fun () -> cell:spawnCell({1,-1}) end)
           },
    Center_Cell = element(5,Hood),
    Next_Cell = element(6,Hood),
    Reference = make_ref(),
    Center_Cell ! {self(),Reference,{linkup,Hood,Next_Cell}},
    
    {Msg,_} = message_buffer:receiver(Reference,Center_Cell,{0,[]}),
    case Msg of
        {Pid,_,Key_Ref,{reply,linkup,sucsess}} when Pid == Center_Cell, Key_Ref == Reference ->
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            true;
        _->
            ?debugMsg("Received maformed message,Fail"),
            false    
    end.

queryHoodTest() ->
	{Center_Cell,_,Hood} = buildTestWorld(),
    
    Reference = make_ref(),
    Center_Cell ! {self(),Reference,query_hood},
    receive
        {Pid,_,Key_Reference,{reply,query_hood,_}} when Pid == Center_Cell, Key_Reference == Reference ->
            %?debugFmt("Received reqply message  ~p ~n",[Message]),
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            true;
        _A ->
            ?debugFmt("Received wrong message  ~p ~nWas Expecting reference ~p from ~p ~n ",[_A,Reference,Center_Cell]),
            false
    end.

dumbAnt() ->
    dumbAnt().
    

placeAntTest()->
    {Center_Cell,_,Hood} = buildTestWorld(),
    Ant = spawn(fun() -> dumbAnt() end),
    
    Message = sendAndReceive(Center_Cell, {place_ant,Ant}),
    case Message of
        {_,_,_,{reply,place_ant,sucsess}}->
            %lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            %exit(Ant,succsess),
            true;
        _A ->
            ?debugFmt("Received wrong message  ~p ~n ",[_A]),
            false
    end,
    
    Ant2 = spawn(fun() -> dumbAnt() end),
    
    Message1 = sendAndReceive(Center_Cell, {place_ant,Ant2}),
     case Message1 of
        {_,_,_,{reply,place_ant,fail}} ->
            lists:map(fun(X) -> exit(X,sucsess) end, tuple_to_list(Hood)),
            exit(Ant2,succsess),
            true;
        _AA ->
            ?debugFmt("Received wrong message  ~p ~n ",[_AA]),
            false
    end.
            
coolAnt(Mainy) ->
    receive
        _A ->
            Mainy ! _A
    end,
    coolAnt(Mainy).
                     
moveAntTest() ->
    {Center_Cell,Next,_} = buildTestWorld(),
    My_Pid = self(),
    Ant1 = spawn(fun() -> coolAnt(My_Pid) end),
    Ant2 = spawn(fun() -> coolAnt(My_Pid) end),
    
    
    %place first ant in the center
    Place_Ref1 = make_ref(), 
    Center_Cell ! {Ant1,Place_Ref1,{place_ant,Ant1}},
    {Message,_} = message_buffer:receiver(Place_Ref1,Center_Cell,{0,[]}),
    case Message of
        {_,_,_,{reply,place_ant,sucsess}} ->
            ok;
        _A0 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A0,Place_Ref1,Ant1]),
            ?assert(false)
    end,
    
    
    %move ant to the east
    Move_Ref1 = make_ref(), 
    Center_Cell ! {Ant1,Move_Ref1,{move_ant,east}},
    {Message1,_} = message_buffer:receiver(Move_Ref1,Center_Cell,{0,[]}),
    case Message1 of
        {_,_,_,{reply,move_ant,{sucsess,Pid}}} when Pid == Next->
            ok;
        _A1 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A1,Move_Ref1,Ant1]),
            ?assert(false)
    end,

    
    %place second ant in the center
    Place_Ref2 = make_ref(), 
    Center_Cell ! {Ant2,Place_Ref2,{place_ant,Ant2}},
    {Message2,_} = message_buffer:receiver(Place_Ref2,Center_Cell,{0,[]}),
    case Message2 of
        {_,_,_,{reply,place_ant,sucsess}} ->
            ok;
        _A2 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A2,Place_Ref2,Ant2]),
            ?assert(false)
    end,
    
    %try to move second ant to the east. which should not be allowed.
    Move_Ref2 = make_ref(), 
    Center_Cell ! {Ant2,Move_Ref2,{move_ant,east}},
    {Message3,_} = message_buffer:receiver(Move_Ref2,Center_Cell,{0,[]}),
    case Message3 of
        {_,_,_,{reply,move_ant,fail}} ->
            ok;
        _A3 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A3,Move_Ref2,Ant2]),
            ?assert(false)
    end,
    
    %try to move second ant "from" the east wich should not be allowed due to wrong ant
    Move_Ref3 = make_ref(), 
    Next ! {Ant2,Move_Ref3,{move_ant,east}},
    {Message4,_} = message_buffer:receiver(Move_Ref3,Next,{0,[]}),
    case Message4 of
        {_,_,_,{reply,move_ant,fail}} ->
            ok;
        _A4 ->
            ?debugFmt("Received wrong message  ~p ~n Was Expecting reference ~p from ~p ~n ",[_A4,Move_Ref3,Ant2]),
            ?assert(false)
    end,

    true.
    
depositFeremoneTest() ->
	{Center_Cell,_,_} = buildTestWorld(),

	Reference = make_ref(),
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,base_feremone}},
    
    %timer:sleep(200),
	%Center_Cell ! {123,dump},
    
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	Center_Cell ! {self(),Reference,{deposit_feremone,food_feremone}},
	
    %Center_Cell ! {123,dump},
    
	ignoreMessages(8),

	Message = sendAndReceive(Center_Cell, query_state),
	case Message of
		{_,_,_,{reply,query_state,Attributes}} ->
			Feremone_Map = maps:get(feremones,Attributes),
			{Base_Strength,_} = maps:get(base_feremone,Feremone_Map),
			?assert(Base_Strength > 3),
			{Food_Strength,_} = maps:get(food_feremone,Feremone_Map),
			?assert(Food_Strength > 3)
	end,
	
	true.

setAttributeTest() ->
	{Center_Cell,_,_} = buildTestWorld(),
	
	Message = sendAndReceive(Center_Cell, {set_cell_attribute,{type,nest}}),
	case Message of
		{_,_,_,{reply,set_cell_attribute,sucsess}} ->
			ok;
		_ ->
			?assert(false)
	end,
    
    Message1 = sendAndReceive(Center_Cell, query_state),
	case Message1 of
		{_,_,_,{reply,query_state,Attributes}} ->
			Type = maps:get(type,Attributes,none),
			?assertEqual(Type,nest)
	end,
	
	
	%Center_Cell ! {self(),dump},
	%timer:sleep(50),
	true.
	

takeFoodTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Ref = make_ref(),
    
    Center_Cell ! {self(),Ref,{set_cell_attribute,{food,10}}},
    ignoreMessages(1),

    Message1 = sendAndReceive(Center_Cell, take_food),
    case Message1 of
        {_,_,_,{reply,take_food,sucsess}} ->
            ok;
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message1]),
            ?assert(fail)
    end,
    
    Message2 = sendAndReceive(Center_Cell, query_state),
    case Message2 of
        {_,_,_,{reply,query_state,Attributes}} ->
            Amount = maps:get(food,Attributes,none),
            ?assertEqual(Amount,9);
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message2]),
            ?assert(fail)
    end,
    
    Center_Cell ! {self(),Ref,{set_cell_attribute,{food,0}}},
    ignoreMessages(1),
    
    Message3 = sendAndReceive(Center_Cell, take_food),
    case Message3 of
        {_,_,_,{reply,take_food,fail}} ->
            ok;
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message3]),
            ?assert(fail)
    end,
    
    Message4 = sendAndReceive(Center_Cell, query_state),
    case Message4 of
        {_,_,_,{reply,query_state,Attributes1}} ->
            Amount1 = maps:get(food,Attributes1,none),
            ?assertEqual(Amount1,0);
        _ ->
            ?debugFmt("Received wrong message ~p ",[Message4]),
            ?assert(fail)
    end,

    true.
	
automaticDecayTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Ref = make_ref(),
    
    Center_Cell ! {self(),Ref,{deposit_feremone,base_feremone}},
    Center_Cell ! {self(),Ref,{deposit_feremone,food_feremone}},
    Center_Cell ! {self(),Ref,ping},
    Center_Cell ! {self(),Ref,ping},
    ignoreMessages(4),
    
    Message = sendAndReceive(Center_Cell, query_state),
    case Message of
        {_,_,_,{reply,query_state,Attributes}} ->
            Feremone_Map = maps:get(feremones,Attributes),
            {Base_Strength,_} = maps:get(base_feremone,Feremone_Map),
            ?assert(Base_Strength < 1.0),
            ?assert(Base_Strength > 0.5),
            {Food_Strength,_} = maps:get(food_feremone,Feremone_Map),
            ?assert(Food_Strength < 1.0),
            ?assert(Food_Strength > 0.5)
    end,
    
    true.

drawTest() ->
    {Center_Cell,_,_} = buildTestWorld(),
    Center_Cell ! {self(),{draw,self()}},
    
    receive
        {_,{gui_update,{Position,_}}} ->
            ?assertEqual({0,0},Position);
        _M->
			?debugFmt("Received silly message from Gui ~p",[_M]),
            ?assert(false)
    end,
    true.
            

sendAndReceive(Destination,Message)->
    Ref = make_ref(),
    Destination ! {self(),Ref,Message},
    {Received,_} = message_buffer:receiver(Ref,Destination,{0,[]}),
    Received.

ignoreMessages(0) ->
    ok;
ignoreMessages(N) ->
    receive
        _ ->
            ignoreMessages(N-1)
    end.

testGuiUpdater()->
	buildTestWorld(),
	{Center_Cell,_,_} = buildTestWorld(),
	My_Pid = self(),
	Gui_Thing = spawn(fun() -> guiMock(My_Pid) end),
	Center_Cell ! {self(),0,{set_cell_attribute,{type,nest}}},
    ignoreMessages(1),
	Center_Cell ! {self(),0,{set_cell_attribute,{gui_module,Gui_Thing}}},
	ignoreMessages(1),
	receive
		{attr,Attributes} ->
			%?debugFmt("Received attributes ~p from GUI",[Attributes]),
			Type = maps:get(type,Attributes,none),
			?assertEqual(Type,nest);
		_M ->
			?debugFmt("Received silly message from Gui ~p",[_M]),
			?assert(false)
	end,
	true.
    

guiMock(Pid) ->
	receive
		{_,{_,Attributes}} ->
			Pid ! {attr,Attributes};
		_M ->
			?debugFmt("Gui receivied idiotic message ~p",[_M]),
			?assert(false)
	end.
	
spawnTest_test() ->
    [?assert(spawnTest())].

linkupTest_test() ->
    [?assert(linkupTest())].

queryHood_test()->
    [?assert(queryHoodTest())].

testPlaceAnt_test()->
    [?assert(placeAntTest())].

testMoveAnt_test()->
    [?assert(moveAntTest())].

depositFeremone_test()->
    [?assert(depositFeremoneTest())].

setAttribute_test()->
    [?assert(setAttributeTest())].

takeFood_test()->
    [?assert(takeFoodTest())].

automaticDecay_test()->
    [?assert(automaticDecayTest())].

draw_test()->
    [?assert(drawTest())].

guiUpdate_test()->
    [?assert(testGuiUpdater())].




