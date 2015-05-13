%% @author grandmother
%% @doc @todo Add description to message_buffer.


-module(message_buffer).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([receiver/3,receiver/1]).

-define(DEFAULT_TIMEOUT,2000).

-type message_buffer() :: {Queue_Length::integer(),Message_Buffer::list()}.

%% @doc see receiver/2
%% 
%%
-spec receiver(Buffer::message_buffer()) -> {_Message,New_Buffer::message_buffer()}.
receiver({L,[]}) ->
    receive
        _A ->
            {_A,{L,[]}}   
    end;
receiver({_,[Message]}) ->
    {Message,{0,[]}};

receiver({L,[Message | Tl]}) ->
    {Message,{L-1,Tl}}.


%% @doc wrapper for the recevie keyword which enables a wait untill
%% a specific message arrives without messing up the order of the message queue.<p>
%% 
%% If `Special' is `none' the message wich has been waiting the longest will be returned<br>
%% if `Special' is a reference the function will block untill that message has arrived. But other
%% messages received by the process meanwhile will be placed on a buffer and can then retreived with this function
%% in such a fashion that the order in which these messages arrived is not altered.
%%
-spec receiver(Special::reference() | [reference()],Recipient::pid() | [pid()],Buffer::message_buffer()) -> {_Message,New_Buffer::message_buffer()}.
receiver(Refs, Recipients, Buffer={L,Queue}) when is_list(Refs)==true -> 
    receive
        A={_,_,Request_Reference,_} ->
            case lists:member(Request_Reference,Refs) of
                true ->
                    {A,Buffer,lists:delete(Request_Reference,Refs)};
                false ->
                    receiver(Refs,Recipients,{L+1,lists:append(Queue,[A])})
            end;
		{Pid,Return_Reference,{Request_Type,_}}  ->
			case lists:member(Pid,Recipients) of
				true ->
					Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
					receiver(Refs,Recipients,Buffer);
				false ->
					receiver(Refs,Recipients,Buffer)
			end;
        {Pid,Return_Reference,Request_Type}  ->
            case lists:member(Pid,Recipients) of
                true ->
                    Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
                    receiver(Refs,Recipients,Buffer);
                false ->
                    receiver(Refs,Recipients,Buffer)
            end;
        _Any ->
            receiver(Refs,Recipients,{L+1,lists:append(Queue,[_Any])})
    after ?DEFAULT_TIMEOUT ->
        ?debugFmt("~nReceiver(in process ~p) timed out whils awating ~p.~nDumping everything there is to dump~nMessage buffer: ~n~p~nMessage Queue:~n ~p~n",[self(),Refs,Buffer,element(2,erlang:process_info(self(), messages))]),
        timer:sleep(100),
        exit(failure)
    end;


receiver(Reference,Recipient,Buffer={L,Queue}) ->
	receive
		A={_,_,Request_Reference,_} when Reference == Request_Reference->
			{A,Buffer};
		{Pid,Return_Reference,{Request_Type,_}} when Pid == Recipient ->
			Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
			receiver(Reference, Recipient, Buffer);
        
        {Pid,Return_Reference,Request_Type} when Pid == Recipient ->
            Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
            receiver(Reference, Recipient, Buffer);
		_Any ->
			receiver(Reference,Recipient,{L+1,lists:append(Queue,[_Any])})
    after ?DEFAULT_TIMEOUT ->
        ?debugFmt("~nReceiver(in process ~p) timed out whils awating ~p.~nDumping everything there is to dump~n Message buffer: ~n~p~nMessage Queue:~n ~p~n",[self(),Reference,Buffer,element(2,erlang:process_info(self(), messages))]),
        timer:sleep(100),
        exit(failure)
    end.





%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

pinger() ->
	receive
		{Pid,Reference,_A} ->
			%io:format("Pinger got its cool message~n"),
			timer:sleep(250),
			Pid ! {self(),make_ref(),Reference,"I Love To Reply!"};
		_B ->
			%io:format("Pinger recieved pointless message ~p ~n",[_B]),
			exit(failure)
	end,
	pinger().

troller(Receiver,N) when N == 20 ->
	Receiver ! {die},
	exit(sucsess);
troller(Receiver,N) ->
	timer:sleep(50),
	Receiver ! {self(), N},
	troller(Receiver, N+1). 

tester() ->
	MyPid = self(),
	PingerPid = spawn(fun() -> pinger() end),
	TrollPid = spawn(fun() -> troller(MyPid,0) end),
	timer:sleep(300),
	Reference = make_ref(),
	PingerPid ! {self(),Reference,ok},
	tester(Reference,{0,[]},[],PingerPid,TrollPid).

tester(Reference,Message_Buffer,List,Pinger_Pid,TrollPid) ->
	{Message,New_Buffer} = receiver(Reference,Pinger_Pid,Message_Buffer),
	case Message of
		{die} ->
			%io:format("Recieved last message...time to die ~n"),
			exit(Pinger_Pid,sucsess),
			List;
		_A={_,N}->
			%io:format("Cool kid troll message ~p ~n",[_A]),
			tester(none,New_Buffer,lists:append(List,[N]),Pinger_Pid,TrollPid);
		_ ->
			%io:format("received dope message~n"),
			tester(none,New_Buffer,List,Pinger_Pid,TrollPid)
	
	end.
	

creeps(Receiver,Reference) ->
    timer:sleep(100+random:uniform(200)),
    Receiver ! {self(),make_ref(),Reference,self()}.
    

testerOfLists() ->
    MyPid = self(),
    TrollerPid = spawn(fun() -> troller(MyPid,0) end),
    Buffer = {0,[]},
    {Pids,Refs} = buildCoolList([],[],10),
    testerOfLists(Pids,Refs,Buffer),
    exit(TrollerPid,sucsess).

testerOfLists([_],[],_) ->
    exit(failure);
testerOfLists([],[],_) ->
    true;

testerOfLists(Pids,Refs,Buffer) ->
    {Message,New_Buffer,New_Refs} = receiver(Refs,Pids,Buffer),
    case Message of
        
        {_,_,Reference,Pid} ->
            ?_assert(lists:member(Reference,Refs)),
            ?_assert(lists:member(Pid,Pids)),
            testerOfLists(lists:delete(Pid,Pids),New_Refs,New_Buffer);
        _ ->
            ?_assert(false)
    end.

    

buildCoolList(Pids,Refs,0)->
    {Pids,Refs};

buildCoolList([],[], N) ->
    Reference = make_ref(),
    MyPid = self(),
    CoolPid= spawn(fun() -> creeps(MyPid,Reference) end),
    buildCoolList([CoolPid],[Reference],N-1);
buildCoolList(Pids,Refs, N) ->
    Reference = make_ref(),
    MyPid = self(),
    CoolPid= spawn(fun() -> creeps(MyPid,Reference) end),
    buildCoolList([CoolPid]++Pids,[Reference]++Refs,N-1).

oddball(Sender) ->
    receive
        {Pid,Reference,spam_me} ->
            Pid ! {self(),make_ref(),Reference,{reply,spam_me,sucsess}};
        _ ->
            ?debugMsg("Tried to unblock in the wrong state...."),
            ?assert(false)
    
            
    after 20 -> 
        Dumb_Ref = make_ref(),
        Sender ! {self(),Dumb_Ref,block},
        %?debugMsg("odball is blocking"),
        receive 
            {_,_,Dumb_Ref,{reply,block,sucsess}} ->
                ?debugMsg("block succeded !?"),
                ?assert(false);
            {_,_,Dumb_Ref,{reply,block,fail}} ->
                timer:sleep(10),
                %?debugMsg("odball received fail message"),
                ok
        end
        %?debugMsg("odball is no longer blocking")
               
    end,
    oddball(Sender).
          

testDeadlockResolving() ->
    My_Pid = self(),
    Oddball = spawn(fun() -> oddball(My_Pid) end),
    Reference = make_ref(),
    timer:sleep(50),
    Oddball ! {self(), Reference,spam_me},
    {Message,_} = receiver(Reference, Oddball, {0,[]}),
    case Message of
        {_,_,_,{reply,spam_me,sucsess}} -> 
            ok;
        _A -> 
          ?debugFmt("Receiver returned the wrong message ~p",[_A]),
          ?assert(fail)
    end,
    true.

        

testListThing_test()->
    [?_assert(testerOfLists())].

basic_test() ->
    [?_assertEqual([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19], tester())].

deadlock_test() ->
    [?assert(testDeadlockResolving())].

		

