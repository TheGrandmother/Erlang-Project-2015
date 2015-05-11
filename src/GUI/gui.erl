-module(gui).



main(AddList, SizeOfGrid)->
	receive
		{Pid,{gui_update,{X,Y},Attributes}} -> 
			Attributes;
		{Pid, {gui_init, {X, Y}}} ->
			Init_SizeOfGrid = X * Y, main(AddList, Init_SizeOfGrid).