%% @hidden
-module(cell). 



-import(prototype,[get2D/2]).
-export([cellStarter/0]).

-type color()::black|white.
-type cell()::{Color::color(),Ant::pid()|none,{
                         UL::pid() | none ,UM::pid() | none ,UR::pid() | none ,
                         ML::pid() | none ,MM::pid() | none ,MR::pid() | none ,
                         LL::pid() | none ,LM::pid() | none ,LR::pid() | none
                        },Next::pid() | none,{X::integer(),Y::integer()}}.



-spec cellStarter() -> ok.
cellStarter() ->
    receive
        {Sender, hood_addresses, {{Ul,Um,Ur,Ml,Mm,Mr,Ll,Lm,Lr},Next,{X,Y}}} ->
            Cell = {white,none,{Ul,Um,Ur,Ml,Mm,Mr,Ll,Lm,Lr},Next,{X,Y}},
            io:format("Cell at ~w recieved linkup.~n",[{X,Y}]),
            cellMain(Cell);
        _A ->
            io:format("Cell Starter Recieved stupid message: ~w. Forwarding to self ~n",[_A]),
            self() ! _A
    end.


%
% This is an example of what the main cell loop might look like.
%
-spec cellMain(Cell::cell()) -> ok.
cellMain(Cell={State,Ant,Hood,Next,Cordinate}) ->
    %io:format("Cell ~w awaiting message ~n",[self()]),
    receive
        
        Place_Ant={Sender, place_ant,Ant_Pid} ->
            case Ant of
                none -> 
                    Sender ! {self(), allowed},
                    cellMain({State,Ant_Pid,Hood,Next,Cordinate});
                _ ->
                    Sender ! {self(), failed},
                    cellMain(Cell)
            end;
        
		Move_Ant={Sender, move_ant, Direction, Ant_Pid} ->
            %io:format("Cell ~w processing move request ~n",[self()]),
			case Ant of
				none ->
					%io:format("Can't move nonexistent ant~n"),
					Sender ! {self(), failed},
					cellMain(Cell);
				_ ->
					if 
						(Ant /= Sender) or (Ant /= Ant_Pid) ->
					%		io:format("Don't touch my ant ~n"),
							Sender ! {self(), failed},
							cellMain(Cell);
						true ->
                           
                    %        io:format("Cell stareted reloving movy stuff ~n"),
                            Destination = getNeighbourFromDirection(Direction,Hood ),
                            case Destination of
                                 
                                none ->
                     %               io:format("ant wants to move to unreachable cell~n"),
                                    Sender ! {self(), failed},
                                    cellMain(Cell);
                                _ ->
        							Destination ! {self(), place_ant, Ant_Pid},
                      %              io:format("Cell waiting for place ant confirmation from ~w ~n",[Destination]),
        							receive
        								{_, failed} ->
        				%					io:format("Move failed ~n"),
        									Sender ! {self(), failed},
                         %                   io:format("Cell finised processing move request ~n"),
        									cellMain(Cell);
        								_ ->
        									Sender ! {self(), allowed,Destination},
                            %                io:format("Cell finised processing move request ~n"),
                            				cellMain({State,none,Hood,Next,Cordinate})
        							end
                            end
					end
			end;

        Querry_Hood={Sender,querry_hood} ->
            spawn(fun() -> querryHood(Sender, Hood) end),
            cellMain(Cell);

        State_Querry = {Sender,state_querry} ->
            		Sender ! {self(), state_querry_reply, {State, Ant}},  %I changed this to let the state be a tuple instead.
            		cellMain(Cell);
					
        
        Set_State = {Sender, set_state,New_State} ->
            Sender ! {self(), allowed}, 
            cellMain({New_State, Ant, Hood, Next, Cordinate});
        
        Get_Next = {Sender,get_next} ->
            Sender ! {self(),next_reply,Next},
            cellMain(Cell);
        
        _A ->
            io:format("Cell ~w,(~w) recieved silly message ~w ~n ",[self(),Cordinate,_A])
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
    Reciever ! {self(), querry_hood_reply, erlang:list_to_tuple(lists:reverse(_A))};

querryHood_aux(Reciever, [none | Tl],_A) ->
    querryHood_aux(Reciever, Tl,[none|_A]);

querryHood_aux(Reciever, [H | Tl],_A) ->
    H ! {self(), state_querry},
    receive
        {Pid,state_querry_reply,State} -> %Add saftey check so that Pid matches H
            querryHood_aux(Reciever, Tl,[{Pid,State}|_A]);
        _ ->
            ok
    end.


getNeighbourFromDirection(up,{_,Um,_,_,_,_,_,_,_}) ->
    Um;
getNeighbourFromDirection(right,{_,_,_,_,_,Mr,_,_,_}) ->
    Mr;
getNeighbourFromDirection(down,{_,_,_,_,_,_,_,Lm,_}) ->
    Lm;
getNeighbourFromDirection(left,{_,_,_,Ml,_,_,_,_,_}) ->
    Ml.


