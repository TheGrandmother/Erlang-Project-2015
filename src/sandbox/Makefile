test.beam: test.erl
	erl -compile test.erl
	
silly.beam: test.beam silly.erl
	erl -compile silly.erl
	
	
test: silly.beam test.beam
	erl -noshell -run silly testShit
	