%% @author grandmother
%% @doc @todo Add description to sorter.


-module(sorter).

%% ====================================================================
%% API functions
%% ====================================================================
-export([sort/1]).
-include_lib("eunit/include/eunit.hrl").

sort(T) ->
    T0 = swap(T,0),
    T1 = swap(T0,1),
    T2 = swap(T1,2),
    T3 = swap(T2,3),
    T4 = swap(T3,4),
    T5 = swap(T4,5),
    T6 = swap(T5,6),
    T7 = swap(T6,7),
    T8 = swap(T7,8),
    T9 = swap(T8,9),
    T10 = swap(T9,10),
    T11 = swap(T10,11),
    T12 = swap(T11,12),
    T13 = swap(T12,13),
    T14 = swap(T13,14),
    T15 = swap(T14,15),
    T16 = swap(T15,16),
    T17 = swap(T16,17),
    swap(T17,18).

%% ====================================================================
%% Internal functions
%% ====================================================================

%SWAP(0, 1);
swap({E0,E1,E2,E3,E4,E5,E6,E7},0) ->
    {_,VU} = E0,
    {_,VL} = E1,
    if
        VL > VU ->
            {E1,E0,E2,E3,E4,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(2, 3);
swap({E0,E1,E2,E3,E4,E5,E6,E7},1) ->
    {_,V1} = E2,
    {_,V2} = E3,
    if
        V2 > V1 ->
            {E0,E1,E3,E2,E4,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   
%SWAP(0, 2);
swap({E0,E1,E2,E3,E4,E5,E6,E7},2) ->
    {_,VU} = E0,
    {_,VL} = E2,
    if
        VL > VU ->
            {E2,E1,E0,E3,E4,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;  

%SWAP(1, 3);
swap({E0,E1,E2,E3,E4,E5,E6,E7},3) ->
    {_,VU} = E1,
    {_,VL} = E3,
    if
        VL > VU ->
            {E0,E2,E3,E1,E4,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   

%SWAP(1, 2);
swap({E0,E1,E2,E3,E4,E5,E6,E7},4) ->
    {_,VU} = E1,
    {_,VL} = E2,
    if
        VL > VU ->
            {E0,E2,E1,E3,E4,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   

%SWAP(4, 5);
swap({E0,E1,E2,E3,E4,E5,E6,E7},5) ->
    {_,VU} = E4,
    {_,VL} = E5,
    if
        VL > VU ->
            {E0,E1,E2,E3,E5,E4,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(6, 7);
swap({E0,E1,E2,E3,E4,E5,E6,E7},6) ->
    {_,VU} = E6,
    {_,VL} = E7,
    if
        VL > VU ->
            {E0,E1,E2,E3,E4,E5,E7,E6};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   

%SWAP(4, 6);
swap({E0,E1,E2,E3,E4,E5,E6,E7},7) ->
    {_,VU} = E4,
    {_,VL} = E6,
    if
        VL > VU ->
            {E0,E1,E2,E3,E6,E5,E4,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;  

%SWAP(5, 7);
swap({E0,E1,E2,E3,E4,E5,E6,E7},8) ->
    {_,VU} = E5,
    {_,VL} = E7,
    if
        VL > VU ->
            {E0,E1,E2,E3,E4,E7,E6,E5};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   

%SWAP(5, 6);
swap({E0,E1,E2,E3,E4,E5,E6,E7},9) ->
    {_,VU} = E5,
    {_,VL} = E6,
    if
        VL > VU ->
            {E0,E1,E2,E3,E4,E6,E5,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   
%SWAP(0, 4);
swap({E0,E1,E2,E3,E4,E5,E6,E7},10) ->
    {_,VU} = E0,
    {_,VL} = E4,
    if
        VL > VU ->
            {E4,E1,E2,E3,E0,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   
%SWAP(1, 5);
swap({E0,E1,E2,E3,E4,E5,E6,E7},11) ->
    {_,VU} = E1,
    {_,VL} = E5,
    if
        VL > VU ->
            {E0,E5,E2,E3,E4,E1,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;   

%SWAP(1, 4);
swap({E0,E1,E2,E3,E4,E5,E6,E7},12) ->
    {_,VU} = E1,
    {_,VL} = E4,
    if
        VL > VU ->
            {E0,E4,E2,E3,E1,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(2, 6);
swap({E0,E1,E2,E3,E4,E5,E6,E7},13) ->
    {_,VU} = E2,
    {_,VL} = E6,
    if
        VL > VU ->
            {E0,E1,E6,E3,E4,E5,E2,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(3, 7);
swap({E0,E1,E2,E3,E4,E5,E6,E7},14) ->
    {_,VU} = E3,
    {_,VL} = E7,
    if
        VL > VU ->
            {E0,E1,E2,E7,E4,E5,E6,E3};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(3, 6);
swap({E0,E1,E2,E3,E4,E5,E6,E7},15) ->
    {_,VU} = E3,
    {_,VL} = E6,
    if
        VL > VU ->
            {E0,E1,E2,E6,E4,E5,E3,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(2, 4);
swap({E0,E1,E2,E3,E4,E5,E6,E7},16) ->
    {_,VU} = E2,
    {_,VL} = E4,
    if
        VL > VU ->
            {E0,E1,E4,E3,E2,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(3, 5);
swap({E0,E1,E2,E3,E4,E5,E6,E7},17) ->
    {_,VU} = E3,
    {_,VL} = E5,
    if
        VL > VU ->
            {E0,E1,E2,E5,E4,E3,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end;

%SWAP(3, 4);
swap({E0,E1,E2,E3,E4,E5,E6,E7},18) ->
    {_,VU} = E3,
    {_,VL} = E4,
    if
        VL > VU ->
            {E0,E1,E2,E4,E3,E5,E6,E7};
        true ->
            {E0,E1,E2,E3,E4,E5,E6,E7}
    end.

testBasic_test()->
    Test_Tuple = {{0,0},{1,1},{2,2},{3,3},{4,4},{5,5},{6,6},{7,7}},
    Test_Tuple1 = list_to_tuple(lists:reverse(tuple_to_list(Test_Tuple))),
    
    F_Test_Tuple = {{0,0.123789456},{1,1.123},{2,2.99999},{3,3.32135489},{4,4.18186},{5,5.16816},{6,6.8},{7,7.1}},
    F_Test_Tuple1 = list_to_tuple(lists:reverse(tuple_to_list(F_Test_Tuple))),
    
    %?debugFmt("~p ~n ~p",[Test_Tuple,Test_Tuple1]),
    [
     ?assertEqual(Test_Tuple1,sort(Test_Tuple)),
     ?assertEqual(F_Test_Tuple1,sort(F_Test_Tuple))
     
     ].