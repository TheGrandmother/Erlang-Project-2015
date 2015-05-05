#
#	One day this project will see a glorious makefile....
#   	but today is not that day
#
SOURCES=$(wildcard src/*/*.erl)
OBJECTS:=$(wildcard ebin/*.beam)

BIN=ebin
SRC=src



#
# You shoulkd kinda copy paste this to make e make target for your module
# Look at the cool thing commented out bellow.
#
message_buffer_binary = $(BIN)/message_buffer.beam
message_buffer_source = $(wildcard $(SRC)/message_buffer/*.erl)

message_buffer: $(message_buffer_binary)
	
$(message_buffer_binary) : $(message_buffer_source)	
	erlc -o $(BIN)/ $^

test_message_buffer: $(message_buffer_binary)
	erl -noshell -pa ebin -eval 'eunit:test(["$(message_buffer_binary)"], [verbose])' -s init stop




#
#<naem>_binary = $(BIN)/$(<name>).beam
#<name>_source = $(wildcard $(SRC)/$(<name>)/*.erl)
#
#<name>: $(<name>_binary)
#	
#$(<name>_binary) : $(<name>_source)	
#	erlc -o $(BIN)/ $^
#
#test: $(<name>_binary)
#	erl -noshell -pa ebin -eval 'eunit:test(["$(<name>_binary)"], [verbose])' -s init stop


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