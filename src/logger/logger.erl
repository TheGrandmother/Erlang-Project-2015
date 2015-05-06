%% @author grandmother
%% @doc @todo Add description to logger.


-module(logger).
-define(LOG,true).
-define(LOG_MESSAGES,true).
-define(LOG_EVENTS,true).

%% ====================================================================
%% API functions
%% ====================================================================
-export([dumbTest/2,makeLog/2,initLogger/0]).

-type log()::{File::file:device(),Type::string(),Pid::pid()} | none.

-spec makeLog(Type::string(),Pid::pid()) -> log.

initLogger() ->
	case ?LOG of
		true ->
			initLogger(ok);
		_ ->
			ok
	end.

initLogger(ok) ->
	DirRet = file:list_dir("log"),
	case DirRet of
		{error,enoent} ->
			MkdirRet = file:make_dir("log"),
			case MkdirRet of
				ok ->
					ok;
				{error,Reason} ->
					exit(Reason)
			end;
		{ok,[]} ->
			ok;
		{ok, Files} ->
			io:format("Found old log files, Deleting them like a boss ~n"),
			deleteFiles(Files)
	end.

makeLog(Type,Pid) ->
    case ?LOG of
        true ->
            makeLog(Type,Pid,ok);
        _ ->
            none
    end.

makeLog(Type,Pid,ok)->
    R= io_lib:format("~s-~p.txt",[Type,Pid]),
    Name = lists:flatten(R),
    {Status, Device} = file:open(Name,[write,read]),
    if 
        Status == ok ->
            ok;
        true ->
            io:format("Logfile could not be created due to ~p ~n",[Status]),
            exit(fail)
    end,
    io:format(Device,"Logfile Created for ~s with pid ~w at ~w ~n",[Type,Pid,calendar:local_time()]),
    {Device,Type,Pid}.

-spec logMessage(Log::log(),_Message) -> ok.
logMessage(none,_) ->
    ok;

logMessage({Device,Type,Pid},Message) ->
    case ?LOG_MESSAGES of
        true ->
            logMessage({Device,Type,Pid},Message,ok);
        _ ->
            ok
    end.

logMessage({Device,Type,Pid},Message,ok) ->
    Sender = element(1,Message),
    io:format(Device, "~s :: Message ~n    ",[makeTimeStamp()]),
    case is_pid(Sender)of
        true ->
            io:format(Device, "Received message from ~p:~n        ~p~n",[Sender,Message]);
        false ->
            io:format(Device, "Tried to log this wich is not a nice message:~n        ~p~n",[Message])
    end.

logEvent(none,_) ->
	ok;
logEvent({Device,Type,Pid},Event) ->
	case ?LOG_EVENTS of
		true ->
			logEvent({Device,Type,Pid},Event,ok);
		_ ->
			ok
	end.

logEvent({Device,Type,Pid},Event,ok) ->
    io:format(Device, "~s :: Message ~n    ",[makeTimeStamp()]),
    io:format(Device, "Event: ~p~n",[Event]).



-spec dumbTest(String::string(),_Args) -> ok.
dumbTest(String,_A) -> 
    {Status, Thingy} = file:open("test.txt", [write,read]),
    io:format("Thingy: ~w ~n",[Thingy]),
    io:format(Thingy, "Look at me im writing a fajl :)",[]),
    io:format(Thingy, "And now i write data ~w ~n",[[1,2,3]]).

%% ====================================================================
%% Internal functions
%% ====================================================================

deleteFiles([]) ->
	ok;
deleteFiles([File]) ->
	io:format("~p~n",[file:delete(["log/" | File])]);
deleteFiles([File | Tl])->
	io:format("~p~n",[file:delete(["log/" | File])]),
	deleteFiles(Tl).

makeTimeStamp() ->
    {_,{H,M,S}} = calendar:local_time(),
    makeCoolString("~s:~s:~s",[H,M,S]).

makeCoolString(Format,Arguments) ->
    R= io_lib:format(Format,Arguments),
    lists:flatten(R).




