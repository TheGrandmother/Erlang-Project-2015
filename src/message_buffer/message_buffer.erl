%% @author grandmother
%% @doc @todo Add description to message_buffer.


-module(message_buffer).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([receiver/2]).

-type message_buffer() :: {Queue_Length::integer(),Message_Buffer::list()}.

-spec receiver(Special::reference(),Buffer::message_buffer()) -> {_Message,New_Buffer::message_buffer()}.

receiver(none,{L,[]}) ->
	receive
		_A ->
			{_A,{L,[]}}
	end;
receiver(none,{_,[Message]}) ->
	{Message,{0,[]}};

receiver(none,{L,[Message | Tl]}) ->
	{Message,{L-1,Tl}};

receiver(Reference,Buffer={L,Queue}) -> 
	receive
		A={_,_,Request_Reference,_} when Reference == Request_Reference->
			{A,Buffer};
		_Any ->
			{none,{L+1,lists:append(Queue,[_Any])}}
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================

pinger() ->
	receive
		{Pid,Reference,_A} ->
			io:format("Pinger got its cool message~n"),
			timer:sleep(500),
			Pid ! {self(),make_ref(),Reference,"I Love To Reply!"};
		_B ->
			io:format("Pinger recieved pointless message ~p ~n",[_B]),
			exit(failure)
	end,
	pinger().

troller(Receiver,N) when N == 20 ->
	Receiver ! {die},
	exit(sucsess);
troller(Receiver,N) ->
	timer:sleep(100),
	Receiver ! {self(), N},
	troller(Receiver, N+1). 

tester() ->
	MyPid = self(),
	PingerPid = spawn(fun() -> pinger() end),
	TrollPid = spawn(fun() -> troller(MyPid,0) end),
	timer:sleep(500),
	Reference = make_ref(),
	PingerPid ! {self(),Reference,ok},
	tester(Reference,{0,[]},[],PingerPid,TrollPid).

tester(Reference,MB = {L,Buffer},List,PingerPid,TrollPid) ->
	{Message,New_Buffer} = receiver(Reference,MB),
	case Message of
		none ->
			tester(Reference,New_Buffer,List,PingerPid,TrollPid);
		{die} ->
			io:format("Recieved last message...time to die ~n"),
			exit(PingerPid,sucsess),
			List;
		_A={Pid,N}->
			io:format("Cool kid troll message ~p ~n",[_A]),
			tester(none,New_Buffer,lists:append(List,[N]),PingerPid,TrollPid);
		_ ->
			io:format("received dope message~n"),
			tester(none,New_Buffer,List,PingerPid,TrollPid)
	
	end.
	
%% ====================================================================
%% Tests
%% ====================================================================

filter_test_() ->
    ?_assertEqual([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19], tester()).

		

