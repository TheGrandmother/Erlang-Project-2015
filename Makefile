# Makefile used to compile the project. 

# If you use another build tool you should remove this file. 

SOURCES=$(wildcard src/*/*.erl)
OBJECTS:=$(wildcard ebin/*.beam)

BIN=ebin
SRC=src

module = none

module_binary = $(BIN)/$(module).beam
module_source = $(wildcard $(SRC)/$(module)/*.erl)

build: $(module_binary)
	
$(module_binary) : $(module_source)	
	erlc -o $(BIN)/ $^

test: $(module_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(module_binary)"], [verbose])' -s init stop


#
# This is testy stuff
#

comma:= ,
empty:=
space:= $(empty) $(empty)
THINGS := $(foreach lol,$(OBJECTS),"$(lol)")
OBJECTS_LIST:= $(subst $(space),$(comma),$(THINGS))
test_all: $(OBJECTS)
	#echo $(BALLS)
	#@echo $(OBJECT_LIST)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [verbose])' -s init stop

test_all_quiet: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [])' -s init stop