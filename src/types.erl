%% @author Henrik Sommerland
%% @doc This is the headerfile containg the specifactions of global types.

-module(types).

-export_type([one_way_message/0, request_message/0, reply_message/0,
              one_way_type/0, request_type/0, reply_type/0,
              message_buffer/0, cell/0, neighbourhood/0,
              direction/0, cell_attributes/0, feremone_type/0,
              feremone_name/0, ant/0, ant_state/0, ant_attributes/0,
              log/0]).



-type one_way_message() :: {Sender::pid(), Payload::one_way_type()}.
%% Type specifying one way messages.
%% These messages are "fire and forget". There is no need to wait for replys.



-type request_message() :: {Sender::pid(),Reference::reference(),Payload::request_type()}.
%% This is the request type messages
%% Each of these messages require that you wait for a specific response

-type reply_message() :: {Sender::pid(),Reference::reference(),Request_Reference::reference(),Payload::reply_type()}.
%% This is the reply type messages
%% These are replies to specific request messages. The Request_reference must match the
%% reference in the request message to wich this is a reply.


-type one_way_type() ::   
                        {gui_update,{{X::integer(),Y::integer()},#{}}}
						| {gui_init,{X::integer(),Y::integer()}}
                        | {draw,Gui_Pid::pid()} %Message to be sent to a cell to tell it to relay its sttate to the gui module.
                        | start_ant
                        | stop_ant
						| {found_food,Steps_Taken::integer()}
						| {returned_with_food,Steps_Taken::integer()}
                        | dump.
%%  This is the different types of one way messages


-type request_type() ::   query_hood 
                        | query_state 
                        | {move_ant,{Direction::direction(),State::ant_state()}} 
						| {place_ant, {Ant::pid(),State::ant_state()}}
						| {set_cell_attribute, Attributes::#{}}
						| {set_ant_attribute, Attributes::#{}}
                        | {linkup,Hood::neighbourhood(),Next::pid()}
                        | take_food 
						| {deposit_feremone,Type::feremone_name()}
                        | ping.
%%  These are the different types of request messages
                        


-type reply_type() ::     {reply,query_hood,fail | {
                                             Attributes::#{}, Attributes::#{}, Attributes::#{},
                                             Attributes::#{}, Attributes::#{}, Attributes::#{},
                                             Attributes::#{}, Attributes::#{}, Attributes::#{}}}
                        | {reply,query_state,fail | [cell_attributes()] | [ant_attributes()]}
                        | {reply,move_ant,fail | {sucsess, pid()}}
						| {reply,place_ant,sucsess | fail}
						| {reply,set_cell_attribute, sucsess | fail}
						| {reply,set_ant_attribute, sucsess | fail}
                        | {reply,linkup, sucess | fail}
                        | {reply,take_food,sucsess | fail}
						| {reply,deposit_feremone,sucsess | fail}
                        | pong.
%%  These are the different types of reply messages.
                        

-type log()::{File::file:device(),Type::string(),Pid::pid()} | none.
%%   Type for the logger construct


-type message_buffer() :: {Queue_Length::integer(),Message_Buffer::list()}.
%%   This message buffer object is used by the message buffering mechanism


-type cell() :: {Pid::pid(),Position::{X::integer(),Y::integer()},Hood::neighbourhood(),Next_Cell::pid(), Attributes::#{},Metadata::message_buffer(),Log::log()}.
%%   This is the big type for the cell. Hopefully its self explanatory



-type neighbourhood() :: {   
                Upper_Left::pid(),  Upper_Middle::pid(),  Upper_right::pid(),
                Middle_Left::pid(), Middle_Middle::pid(), Middle_right::pid(),
                Lower_Left::pid(),  Lower_Middle::pid(),  Lower_right::pid()
                }.
%%   simple type for the neighbourhood


-type direction() ::    northwest   |   north   |   northeast |
                        west        |   center  |   east      |
                        southwest   |   south   |   southeast.
%%   Simple type specifying the different directions there are.


-type cell_attributes() ::    {type, plain | nest | block}
                            | {ant, Pid::pid()} 
                            | {feremones, none | list(feremone_type())} 
                            | {food, Ammount::integer()}
							| {gui_module, pid()}.
%%   These are the different key value pairs that a cells attributes map can have


-type feremone_type() :: {Name::feremone_name(),Strength::float(),Dissipation_Rate::float}.
%%   This is a definition of the types of freremone on a cell.

-type feremone_name() :: base_feremone | food_feremone.
%%   Just a collection of feremone names.


-type ant() :: {Pid::pid(),Cell::pid(),State::ant_state, Attributes::#{}, Metadata::message_buffer(),Log::log()}.
%%  A sweet type for the ant.


-type ant_state() ::     searching_for_food
                        |idling
                        |returning_with_food.
%%  The different "states" than an ant can be in.


-type ant_attributes() ::    {time_to_live, integer()}
                            |{food, Amount::integer()}.
%%  These are the different key value pairs that a cells attributes map can have.                    


