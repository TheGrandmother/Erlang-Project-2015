%% @author grandmother
%% @doc @todo Add description to message_buffer.


-module(message_buffer).

-include_lib("eunit/include/eunit.hrl").

-define(RESOLVING,true).

%% ====================================================================
%% API functions
%% ====================================================================
-export([receiver/4,receiver/3,receiver/1,hasMessages/1]).

-define(DEFAULT_TIMEOUT,750).   

-type message_buffer() :: {Queue_Length::integer(),Message_Buffer::list()}.


-spec hasMessages(Buffer::types:buffer()) -> true | false.
hasMessages({_,[]})->
    {_,Length} = erlang:process_info(self(), message_queue_len),
    case Length of
        0 ->
            false;
        _ ->
            true
    end;
hasMessages(_)->
    true.


%% @doc see receiver/2
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
receiver(Refs, Recipients, {_,Queue})->
    %We must first check the buffer for conflicting deadlocks!!!!!
    receiver(Refs, Recipients, cleanBuffer(lists:flatten([Recipients]), Queue, []),none,ok).
receiver(Refs, Recipients, {_,Queue},Tag)->
	%We must first check the buffer for conflicting deadlocks!!!!!
	receiver(Refs, Recipients, cleanBuffer(lists:flatten([Recipients]), Queue, []),Tag,ok).

receiver(Refs, Recipients, Buffer={L,Queue},Tag,ok) when is_list(Refs)==true -> 
    receive
		Request_Messsage={Sender,Reference,Payload} ->
			case lists:member(Sender,Recipients) of
				true ->
					%?debugMsg("DEADLOCK ENCOUNTERED!"),
					case Payload of 
						{Type,_}->
							Sender ! {self(), make_ref(), Reference,{reply,Type,fail}};
						Type ->
							Sender ! {self(), make_ref(), Reference,{reply,Type,fail}}
					end,
					receiver(Refs, Recipients, Buffer,Tag,ok);
				false ->
					receiver(Refs,Recipients,{L+1,lists:append(Queue,[Request_Messsage])},Tag,ok)
			end;
            
		Reply_Message={_,_,Request_Reference,_} ->
			case lists:member(Request_Reference,Refs) of
				true ->
					{Reply_Message,Buffer,lists:delete(Request_Reference,Refs)};
				false ->
					receiver(Refs,Recipients,{L+1,lists:append(Queue,[Reply_Message])},Tag,ok)
			end;


        _Any ->
            receiver(Refs,Recipients,{L+1,lists:append(Queue,[_Any])},Tag,ok)
    after ?DEFAULT_TIMEOUT ->
        case ?RESOLVING of
            false ->
                ?debugFmt("~nReceiver(in process ~p) timed out whilst in state ~p and awaiting ~p from ~p .~nMessage buffer: ~n~p~nMessage Queue:~n ~p~n",
        				  [self(),Tag,Refs,Recipients,Buffer,element(2,erlang:process_info(self(), messages))]),
                timer:sleep(100),
                exit(failure);
            true ->
                case L of
                    0 ->
                        receiver(Refs, Recipients, Buffer,Tag,ok);
                    _ ->
                        ?debugFmt("PID ~p suspecting deadlock in state ~p! Failing all incoming reuqests.Queue length =  ~p",[self(),Tag,L]),
                        New_Queue = lists:filter(fun(X) -> (not ifRequestSendFail(X)) end,  Queue),
                        receiver(Refs, Recipients, {length(New_Queue),New_Queue},Tag,ok)
                end
        end
    end;


receiver(Reference,Recipient,Buffer={L,Queue},Tag,ok) ->
	receive
		A={_,_,Request_Reference,_} when Reference == Request_Reference->
			{A,Buffer};
		{Pid,Return_Reference,{Request_Type,_}} when Pid == Recipient ->
			%?debugMsg("DEADLOCK ENCOUNTERED!"),
			Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
			receiver(Reference, Recipient, Buffer,Tag,ok);
        
        {Pid,Return_Reference,Request_Type} when Pid == Recipient ->
			%?debugMsg("DEADLOCK ENCOUNTERED!"),
            Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
            receiver(Reference, Recipient, Buffer,Tag,ok);
		_Any ->
			receiver(Reference,Recipient,{L+1,lists:append(Queue,[_Any])},Tag,ok)
    after ?DEFAULT_TIMEOUT ->
        case ?RESOLVING of
            false ->
                ?debugFmt("~nReceiver(in process ~p) timed out whilst in state ~p and awaiting ~p from ~p.~n Message buffer: ~n~p~nMessage Queue:~n ~p~n",  
                  [self(),Tag,Reference,Recipient,Buffer,element(2,erlang:process_info(self(), messages))]),
                    timer:sleep(100),
                    exit(failure);
            true ->
                case L of
                    0 ->
                        receiver(Reference, Recipient, Buffer,Tag,ok);
                    _ ->
                        ?debugFmt("PID ~p suspecting deadlock in state ~p! Failing all incoming reuqests.Queue length =  ~p",[self(),Tag,L]),
                        New_Queue = lists:filter(fun(X) -> (not ifRequestSendFail(X)) end,  Queue),
                        receiver(Reference, Recipient, {length(New_Queue),New_Queue},Tag,ok)
                end
        end
    
    end.

cleanBuffer(_,[],[])->
	{0,[]};
cleanBuffer(_,[],New_Buffer)->
	{length(New_Buffer),lists:reverse(lists:flatten(New_Buffer))};
cleanBuffer(Pids, [Hd|Tl],Buff)->
	Recipient = element(1,Hd),
	case lists:member(Recipient,Pids) of
		true ->
			case ifRequestSendFail(Hd) of
				true ->
					%?debugFmt("Found deadlock in buffer and deleating msg ~p",[Hd]),
					cleanBuffer(Pids, Tl,Buff);
				false ->
					cleanBuffer(Pids, Tl,[Hd]++Buff)
			end;
		false ->
			cleanBuffer(Pids, Tl,[Hd]++Buff)
	end.


ifRequestSendFail(Msg = {Pid,Reference,{Type,_}}) ->
    ?debugFmt("Pid ~p premptivley rejects the request ~p",[self(),Msg]),
	Pid ! {self(),make_ref(),Reference,{reply,Type,fail}},
	true;
ifRequestSendFail(Msg = {Pid,Reference,Type}) ->
    ?debugFmt("Pid ~p premptivley rejects the request ~p",[self(),Msg]),
	Pid ! {self(),make_ref(),Reference,{reply,Type,fail}},
	true;
ifRequestSendFail(_) ->
	false.



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

		
%        A={_,_,Request_Reference,_} ->
%            case lists:member(Request_Reference,Refs) of
%                true ->
%                    {A,Buffer,lists:delete(Request_Reference,Refs)};
%                false ->
%                    receiver(Refs,Recipients,{L+1,lists:append(Queue,[A])})
%            end;
%		{Pid,Return_Reference,{Request_Type,_}}  ->
%			case lists:member(Pid,Recipients) of
%				true ->
%					?debugMsg("DEADLOCK ENCOUNTERED!"),
%					Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
%					receiver(Refs,Recipients,Buffer);
%				false ->
%					receiver(Refs,Recipients,Buffer)
%			end;
%        {Pid,Return_Reference,Request_Type}  ->
%            case lists:member(Pid,Recipients) of
%                true ->
%					?debugMsg("DEADLOCK ENCOUNTERED!"),
%                    Pid ! {self(), make_ref(), Return_Reference,{reply,Request_Type,fail}},
%                    receiver(Refs,Recipients,Buffer);
%                false ->
%                    receiver(Refs,Recipients,Buffer)
%            end;
