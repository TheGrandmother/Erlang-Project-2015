%% @author grandmother
%% @hidden
%% @doc @todo Add description to ant.


-module(ant).

%% ====================================================================
%% API functions
%% ====================================================================
-export([spawnAnt/1]).

-type ant()::{Pos::pid(),Dir::(up|down|left|right)}.

-spec spawnAnt(Cell::pid()) -> pid().
spawnAnt(Cell) ->
    spawn(fun() -> antInit(Cell) end).

-spec antInit(Cell::pid()) -> ok.
antInit(Cell) ->
    Cell ! {self(), place_ant, self()},
    receive
        {_, failed} ->
            io:format("Ant placement failed~n"),
            exit(fail);
        _ ->
            ok
    end,
    antMain({Cell,up}).

-spec antMain(Ant::ant()) -> ok.
antMain(Ant={Cell,Direction}) ->
    
    case process_info(self(), message_queue_len) of
        {_,0} -> 
            %take little nap ^^
            timer:sleep(200),
            takeAction(Ant);
        _ ->
            io:format("Ant has messages in mailbox ~n"),
            receive
                _ ->
                    io:format("Ant recieved cool message"),
                    antMain(Ant)           
            end
    end.

takeAction(Ant={Cell,Direction})->
    %make a move
    %io:format("Ant is trying to make a baller move ~n"),
    Cell ! {self(),state_querry},
    {State,_} = waitForStateQuery(),
    case State of
        white -> 
            New_Direction = turnRight(Direction),
            Cell ! {self(),set_state,black},
            waitForSetReply(Cell),
            %io:format("Changed color to black but movement is not yet implemented.~n"),
            Cell ! {self(), move_ant, New_Direction, self()},
            %io:format("Ant waiting for move reply~n"),
            New_Cell = receive
                {_,allowed,New_Cell0} ->
                    New_Cell0;
                {_,failed} ->
                    %io:format("Ant was not allowed to move.~n"),
                    exit(fail),
                    Cell
            end,
            %io:format("Ant moved to ~w ~n",[New_Cell]),
            antMain({New_Cell,New_Direction});
            
        black -> 
            New_Direction = turnLeft(Direction),
            Cell ! {self(),set_state,white},
            waitForSetReply(Cell),
            %io:format("Changed color to white but movement is not yet implemented.~n"),
            Cell ! {self(), move_ant, New_Direction, self()},
            %io:format("Ant waiting for move reply~n"),
            New_Cell = receive
                {_,allowed,New_Cell1} ->
                    New_Cell1;
                {_,failed} ->
                    %io:format("Ant was not allowed to move.~n"),
                    exit(fail),
                    Cell
            end,
            %io:format("Ant moved to ~w ~n",[New_Cell]),
            antMain({New_Cell,New_Direction})
    end.


turnLeft(up) -> left;
turnLeft(left) -> down;
turnLeft(down) -> right;
turnLeft(right) -> up.

turnRight(up) -> right;
turnRight(left) -> up;
turnRight(down) -> left;
turnRight(right) -> down.

%This emulates a form of non blocking receive. It will wait untill a state reply comes
%But it will allow other messages to arrive inbetween.
-spec waitForStateQuery() -> _.
waitForStateQuery() -> 
    %io:format("Ant waiting for state_querry confirmation~n"),
    receive
        
        {Sender,state_querry_reply,State} ->
            State;
        _A ->
            io:format("Recieved non state reply whilst waiting for state.~n"),
            self() ! _A,
            waitForStateQuery()
    end.

waitForSetReply(From) ->
    %io:format("Ant waiting for state_set confirmation~n"),
    receive
        {From,allowed} ->
            ok;
        _A ->
            io:format("Recieved non state reply whilst waiting for state set reply.~n"),
            self() ! _A,
            waitForSetReply(From)
    end.
        


%% ====================================================================
%% Internal functions
%% ====================================================================


