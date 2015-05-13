#
#	One day this project will see a glorious makefile....
#   	but today is not that day
#
SOURCES=$(wildcard src/*/*.erl)
OBJECTS:=$(wildcard ebin/*.beam)

BIN=ebin
SRC=src

FLAGS=+debug_info


#
# You shoulkd kinda copy paste this to make e make target for your module
# Look at the cool thing commented out bellow.
#
message_buffer_binary = $(BIN)/message_buffer.beam
message_buffer_source = $(wildcard $(SRC)/message_buffer/*.erl)

message_buffer: $(message_buffer_binary)
	
$(message_buffer_binary) : $(message_buffer_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_message_buffer: $(message_buffer_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(message_buffer_binary)"], [verbose])' -s init stop


logger_binary = $(BIN)/logger.beam
logger_source = $(wildcard $(SRC)/logger/*.erl)

logger: $(logger_binary)
	
$(logger_binary) : $(logger_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_logger: $(logger_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(logger_binary)"], [verbose])' -s init stop


utils_binary = $(BIN)/utils.beam
utils_source = $(wildcard $(SRC)/utils/*.erl)

utils: $(utils_binary)
	
$(utils_binary) : $(utils_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_utils: $(utils_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(utils_binary)"], [verbose])' -s init stop


grid_init_binary = $(BIN)/grid_init.beam
grid_init_source = $(wildcard $(SRC)/grid_init/*.erl)

grid_init: $(grid_init_binary) $(message_buffer_binary) $(logger_binary) $(utils_binary)
	
$(grid_init_binary) : $(grid_init_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_grid_init: $(grid_init_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(grid_init_binary)"], [verbose])' -s init stop
	
	

cell_binary = $(BIN)/cell.beam
cell_source = $(wildcard $(SRC)/cell/*.erl) $(message_buffer_binary) $(logger_binary) $(utils_binary)

cell: $(cell_binary)
	
$(cell_binary) : $(cell_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_cell: $(cell_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(cell_binary)"], [verbose])' -s init stop

ant_binary = $(BIN)/ant.beam
ant_source = $(wildcard $(SRC)/ant/*.erl)

ant: $(ant_binary) 
	
$(ant_binary) : $(ant_source) $(grid_init_binary) $(message_buffer_binary) $(cell_binary) $(logger_binary) src/ant/sorter.erl
	erlc -o $(BIN)/ $^

test_ant: $(ant_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(ant_binary)"], [verbose])' -s init stop


	

#
# COPY THIS AND REPLACE <name> with module name and you have a crazy cool build target :)
#
#<name>_binary = $(BIN)/<name>.beam
#<name>_source = $(wildcard $(SRC)/<name>/*.erl)
#
#<name>: $(<name>_binary)
#	
#$(<name>_binary) : $(<name>_source)	
#	erlc $(FLAGS) -o $(BIN)/ $^
#
#test_<name>: $(<name>_binary)
#	erl -noshell -pa ebin -eval 'eunit:test(["$(<name>_binary)"], [verbose])' -s init stop

clean:
	rm ebin/*.beam

#
# This is testy stuff
#

comma:= ,
empty:=
space:= $(empty) $(empty)
THINGS = $(foreach lol,$(OBJECTS),"$(lol)")
OBJECTS_LIST = $(subst $(space),$(comma),$(THINGS))
test_all: $(OBJECTS) $(message_buffer_binary) $(cell_binary) $(logger_binary) $(grid_init_binary)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [verbose])' -s init stop

test_all_quiet: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [])' -s init stop
