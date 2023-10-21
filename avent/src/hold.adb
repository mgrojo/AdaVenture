

	exestr: string := Ada.command_line.command_name;

	lastchar : constant character := exestr( exestr'last );
	linux: constant boolean := (lastchar='u'); -- _gnu
	osx:   constant boolean := (lastchar='x'); -- _osx
	mswin: constant boolean := (lastchar='e'); -- .exe

