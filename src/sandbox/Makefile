cell.beam: cell.erl
	erl -compile cell.erl
	
ant.beam: ant.erl
	erl -compile ant.erl
	
prototype.beam: cell.beam prototype.erl ant.beam
	erl -compile prototype.erl
	
	
test: prototype.beam
	erl -noshell -run prototype testShit
	
clean:
	-rm *.beam
	