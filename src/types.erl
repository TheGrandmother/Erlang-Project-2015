%% @author grandmother
%% @doc This is the headerfile containg the specifactions of global types.

-module(types).

-export_type([one_way_message/0, request_message/0, reply_message/0,
              one_way_type/0, request_type/0, reply_type/0,
              message_buffer/0, cell/0, neighbourhood/0,
              direction/0, cell_attributes/0, feremone_type/0,
              feremone_name/0, ant/0, ant_state/0, ant_attributes/0,
              log/0]).


%% @doc Type specifying one way messages.
%% These messages are "fire and forget". Tehre is no need to wait for replys.
-type one_way_message() :: {Sender::pid(), Payload::one_way_type()}.

%% @doc This is the request type messages
%% Each of these messages require that you wait for a specific response
-type request_message() :: {Sender::pid(),Reference::reference(),Payload::request_type()}.

%% @doc This is the reply type messages
%% These are replies to specific request messages. The Request_reference must match the
%% reference in the request message to wich this is a reply.
-type reply_message() :: {Sender::pid(),Reference::reference(),Request_Reference::reference(),Payload::reply_type()}.

%% @doc This is the different types of one way messages
-type one_way_type() ::   
                        | {gui_update,{X::integer(),Y::integer()},[#{}]}
						| {gui_init,{X::integer(),Y::integer()}}
                        | dump.

%% @doc These are the different types of request messages
-type request_type() ::   query_hood 
                        | query_state 
                        | {move_request,Direction::direction()} 
						| {place_ant, Ant::pid()}
						| {set_cell_attribute, Attributes::#{}}
						| {set_ant_attribute, Attributes::#{}}
                        | {linkup,Hood::neighbourhood(),Next::pid()}
                        | take_food 
						| {deposit_feremone,Type::feremone_name()}
                        | ping.
                        

%% @doc These are the different types of reply messages.
-type reply_type() ::     {query_hood_reply,fail | {
                                             Attributes::#{}, Attributes::#{}, Attributes::#{},
                                             Attributes::#{}, Attributes::#{}, Attributes::#{},
                                             Attributes::#{}, Attributes::#{}, Attributes::#{}}}
                        | {query_state_reply,fail | [cell_attributes()] | [ant_attributes()]}
                        | {move_reply,fail | {sucsess, pid()}}
						| {place_ant_reply,sucsess | fail}
						| {set_cell_attribute_reply, sucsess | fail}
						| {set_ant_attribute_reply, sucsess | fail}
                        | {linkup_reply, sucess | fail}
                        | {take_food_reply,sucsess | fail}
						| {deposit_feremone_reply,sucsess | fail}
                        | pong.
                        
%% @doc Type for the logger construct
-type log()::{File::file:device(),Type::string(),Pid::pid()} | none.

%% @doc This message buffer object is used by the message buffering mechanism
-type message_buffer() :: {Queue_Length::integer(),Message_Buffer::list()}.

%% @doc This is the big type for the cell. Hopefully its self explanatory
-type cell() :: {Pid::pid(),Position::{X::integer(),Y::integer()},Hood::neighbourhood(),Next_Cell::pid(), Attributes::#{},Metadata::message_buffer(),Log::log()}.

%% @doc simple type for the neighbourhood
-type neighbourhood() :: {   
                Upper_Left::pid(),  Upper_Middle::pid(),  Upper_right::pid(),
                Middle_Left::pid(), Middle_Middle::pid(), Middle_right::pid(),
                Lower_Left::pid(),  Lower_Middle::pid(),  Lower_right::pid()
                }.

%% @doc Simple type specifying the different directions there are.
-type direction() ::    northwest   |   north   |   northeast |
                        west        |   center  |   east      |
                        southwest   |   south   |   southeast.

%% @doc These are the different key value pairs that a cells attributes map can have
-type cell_attributes() ::    {type, plain | nest | block}
                            | {ant, Pid::pid()} 
                            | {feremones, none | list(feremone_type())} 
                            | {food, Ammount::integer()}.

%% @doc This is a definition of the types of freremone on a cell.
-type feremone_type() :: {Name::feremone_name(),Strength::float(),Dissipation_Rate::float}.

%% @doc Just a collection of feremone names.
-type feremone_name() :: base_feremone | food_feremone.

%%@doc A sweet type for the ant.
-type ant() :: {Pid::pid(),Cell::pid(),State::ant_state, Attributes::#{}, Metadata::message_buffer(),Log::log()}.

%%@doc The different "states" than an ant can be in.
-type ant_state() ::     searching_for_food
                        |returning_with_food.
%%@doc These are the different key value pairs that a cells attributes map can have.
-type ant_attributes() ::    {time_to_live, integer()}
                            |{carrying_food, Amount::integer()}.
                    


