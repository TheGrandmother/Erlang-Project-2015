#
#	One day this project will see a glorious makefile....
#   	but today is not that day
#
SOURCES=$(wildcard src/*/*.erl)
OBJECTS:=$(wildcard ebin/*.beam)
BIN=ebin
SRC=src

FLAGS=+debug_info


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

grid_init: $(grid_init_binary) 
	
$(grid_init_binary) : $(grid_init_source) $(utils_binary) $(ant_binary) $(cell_binary) $(message_buffer_binary) $(logger_binary) 
	erlc $(FLAGS) -o $(BIN)/ $^

test_grid_init: $(grid_init_binary) $(message_buffer_binary) $(logger_binary) $(utils_binary) $(cell_binary) $(ant_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(grid_init_binary)"], [verbose])' -s init stop
	
	

cell_binary = $(BIN)/cell.beam
cell_test = $(BIN)/cell_tests.beam
cell_source = $(wildcard $(SRC)/cell/*.erl) $(message_buffer_binary) $(logger_binary) $(utils_binary)

cell: $(cell_binary)
	
$(cell_binary) : $(cell_source)	
	erlc $(FLAGS) -o $(BIN)/ $^



test_cell: $(cell_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(cell_test)"], [verbose])' -s init stop



gui_binary = $(BIN)/gui.beam
gui_source = $(wildcard $(SRC)/GUI/*.erl)  $(grid_init_binary)

gui: $(gui_binary)

$(gui_binary) : $(gui_source)	
	erlc $(FLAGS) -o $(BIN)/ $^

test_gui: $(gui_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(gui_binary)"], [verbose])' -s init stop



ant_binary = $(BIN)/ant.beam
ant_source = $(wildcard $(SRC)/ant/*.erl)

ant: $(ant_binary) 
	
$(ant_binary) : $(ant_source) $(grid_init_binary) $(message_buffer_binary) $(cell_binary) $(logger_binary) src/ant/sorter.erl
	erlc $(FLAGS) -o $(BIN)/ $^

test_ant: $(ant_binary)
	erl -noshell -pa ebin -eval 'eunit:test({timeout, 1000, ["ebin/ant_tests.beam","ebin/sorter.beam"]}, [verbose])' -s init stop

all: ant cell gui

run_ascii: all
	erl -pa ebin -run asciiGui initThingy
	
run: all
	@echo ""
	@echo "Gui cannot be run from the terminal like due to python modules not being loaded correctly"
	@echo "To start the Gui move to the ebin/ folder and run gui:initGui() from the erl shell"
	@echo "You can run the system with the ASCII gui by running 'make run_ascii'"


comma:= ,
empty:=
space:= $(empty) $(empty)
THINGS1 = $(foreach lol,$(SOURCES),"$(lol)")
SOURCE_LIST = $(subst $(space),$(comma),$(THINGS1))

doc: .FORCE
	erl -noshell -run edoc_run files '[$(SOURCE_LIST)]' '[{dir, "doc"}]'

.FORCE:

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



