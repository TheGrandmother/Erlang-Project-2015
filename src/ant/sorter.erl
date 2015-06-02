%% @author grandmother
%% @doc @todo Add description to sorter.


-module(sorter).

%% ====================================================================
%% API functions
%% ====================================================================
-export([sort/1,permute/1]).
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


%unsigned uniform(unsigned i,unsigned m); /* Returns a random integer i <= uniform(i,m) <= m */
% 
%void permute(unsigned permutation[], unsigned n)
%{
%    unsigned i;
%    for (i = 0; i < n; i++) {
%        unsigned j = uniform(i, n - 1);
%        unsigned swap = permutation[i];
%        permutation[i] = permutation[j];
%        permutation[j] = swap;
%    }
%}

permute(Tuple) ->
	permute(Tuple,1).

permute(Tuple,9)->
	Tuple;
permute(Tuple,N)->
	Swap = uniform(N,8),
	permute(swapper(Tuple,N,Swap),N+1).

uniform(1,M)->
	random:uniform(M);
uniform(N,M) when N /= M->
	N + random:uniform(M-N+1) -1 ;
uniform(N,_) ->
	N.
swapper(Tuple,A,B)->
	New_B = element(A,Tuple),
	New_A = element(B,Tuple),
	New1 = setelement(A,Tuple,New_A),
	New2 = setelement(B,New1,New_B),
	New2.
	
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

permute_test() ->
	Start = {1,2,3,4,5,6,7,8},
	Permute1 = permute(Start),
	Permute2 = permute(Start),
	[
	 ?assertNotEqual(Start,Permute1),
	 ?assertNotEqual(Start,Permute2),
	 ?assertNotEqual(Permute2,Permute1)
	 ].

hardcoreSort_test()->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	First = makeHistogram(1,10000,#{}),
	Last = makeHistogram(8,10000,#{}),
	?debugFmt("~nFIRSTELEMENT=~n~p",[First]),
	?debugFmt("~nLastElement=~n~p",[Last]),
	ok.

makeHistogram(_,0,Histogram) ->
	Histogram;
makeHistogram(Index,N,Histogram) ->
	Thing = element(1,element(Index,sort(makeRandomTuple(8, [])))),
	Old_Val = maps:get(Thing,Histogram,0),
	New_Val = Old_Val +1,
	New_Histogram = maps:put(Thing,New_Val,Histogram),
	makeHistogram(Index,N-1,New_Histogram).

hardcorePermute_test()->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	First = makeHistogram1(1,10000,#{}),
	Last = makeHistogram1(8,10000,#{}),
	?debugFmt("~nFIRSTELEMENT=~n~p",[First]),
	?debugFmt("~nLastElement=~n~p",[Last]),
	ok.
	
makeHistogram1(_,0,Histogram) ->
	Histogram;
makeHistogram1(Index,N,Histogram) ->
	Thing = element(Index,permute({1,2,3,4,5,6,7,8})),
	Old_Val = maps:get(Thing,Histogram,0),
	New_Val = Old_Val +1,
	New_Histogram = maps:put(Thing,New_Val,Histogram),
	makeHistogram1(Index,N-1,New_Histogram).

hardcoreUnioform_test()->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	F1 = makeHistogram2(1,10000,#{}),
	F2 = makeHistogram2(2,10000,#{}),
	F3 = makeHistogram2(3,10000,#{}),
	F4 = makeHistogram2(4,10000,#{}),
	F5 = makeHistogram2(5,10000,#{}),
	F6 = makeHistogram2(6,10000,#{}),
	F7 = makeHistogram2(7,10000,#{}),
	F8 = makeHistogram2(8,10000,#{}),
	
	?debugFmt("~n1=~n~p",[F1]),
	?debugFmt("~n2=~n~p",[F2]),
	?debugFmt("~n3=~n~p",[F3]),
	?debugFmt("~n4=~n~p",[F4]),
	?debugFmt("~n5=~n~p",[F5]),
	?debugFmt("~n6=~n~p",[F6]),
	?debugFmt("~n7=~n~p",[F7]),
	?debugFmt("~n8=~n~p",[F8]),

	
	ok.
	
makeHistogram2(_,0,Histogram) ->
	Histogram;
makeHistogram2(Index,N,Histogram) ->
	Thing = uniform(Index,8),
	Old_Val = maps:get(Thing,Histogram,0),
	New_Val = Old_Val +1,
	New_Histogram = maps:put(Thing,New_Val,Histogram),
	makeHistogram2(Index,N-1,New_Histogram).
	

	
makeRandomTuple(0,Thing) ->
	list_to_tuple(Thing);

makeRandomTuple(N,Thing) ->
	makeRandomTuple(N-1,[{N,random:uniform()}]++Thing).










	
	