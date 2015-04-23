
-module(test). 



-export([cellStarter/0]).

-type color()::black|white.
-type cell()::{Color::color(),Ant::pid()|none,{
                         UL::pid() | none ,UM::pid() | none ,UR::pid() | none ,
                         ML::pid() | none ,MM::pid() | none ,MR::pid() | none ,
                         LL::pid() | none ,LM::pid() | none ,LR::pid() | none
                        },Next::pid() | none,{X::integer(),Y::integer()}}.

-type ant()::{Pos::pid(),Dir::{up|down|left|right}}.

-spec cellStarter() -> ok.
cellStarter() ->
    receive
        {Sender, hood_addresses, {{Ul,Um,Ur,Ml,Mm,Mr,Ll,Lm,Lr},Next,{X,Y}}} ->
            Cell = {white,false,{Ul,Um,Ur,Ml,Mm,Mr,Ll,Lm,Lr},Next,{X,Y}},
            io:format("Cell at ~w recieved linkup.~n",[{X,Y}]),
            cellMain(Cell);
        _A ->
            io:foramt("Recieved stupid message: ~w ~n",[_A])
    end.


%
% This is an example of what the main cell loop might look like.
%
-spec cellMain(Cell::cell()) -> ok.
cellMain(Cell={State,Ant,Hood,Next,Cordinate}) ->
    
    receive
        
        Place_Ant={Sender, place_ant,Ant_Pid} ->
            case Ant of
                none -> 
                    Sender ! {self(), failed},
                    cellMain(Cell);
                _ ->
                    Sender ! {self(), allowed},
                    cellMain({State,Ant_Pid,Hood,Next,Cordinate})
            end;
        
        Querry_Hood={Sender,querry_hood} ->
            spawn(fun() -> querryHood(Sender, Hood) end),
            cellMain(Cell);

        State_Querry = {Sender,state_querry} ->
            Sender ! {self(), state_querry_reply, State},
            cellMain(Cell);
        
        Set_State = {Sender, set_state,New_State} ->
            Sender ! {self(), allowed},
            cellMain(Cell);
        
        _A ->
            io:format("recieved silly message ~w ~n ",[_A])
    end.


-spec querryHood(Reciever::pid(),_Tuple) -> ok.
querryHood(Reciever, _Tuple)->
    querryHood_aux(Reciever, tuple_to_list(_Tuple),[]).

%Sends messages to all cells in the neighbourhood querrying states
%and forwrds a tupel of states to the process wich made the inital
%requet for a hood querry.
%A funtion like this could be made completley concurrent.
-spec querryHood_aux(Reciever::pid(),_Hood,_Ack) -> ok.
querryHood_aux(Reciever, [],_A) ->
    Reciever ! {self(), querry_hood_reply, erlang:list_to_tupel(lists:reverse(_A))};

querryHood_aux(Reciever, [none | Tl],_A) ->
    querryHood_aux(Reciever, Tl,[none|_A]);

querryHood_aux(Reciever, [H | Tl],_A) ->
    H ! {self(), state_querry},
    receive
        {Pid,state_querry_reply,State} -> %Add saftey check so that Pid matches H
            querryHood_aux(Reciever, Tl,[State|_A]);
        _ ->
            ok
    end.

      
































