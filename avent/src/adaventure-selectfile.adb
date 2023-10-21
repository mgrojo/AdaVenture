
separate( adaventure )

--
-- Copyright (C) 2023  <fastrgv@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You may read the full text of the GNU General Public License
-- at <http://www.gnu.org/licenses/>.
--







function selectfile return unbounded_string is

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_IO;
use ada.directories;
use text_io;

----------------------------------------------------------------------------

	-- this indicates executable was compiled on Windows
	-- (NOT OSX and NOT Linux)
	mswin: constant boolean := (gnat.os_lib.directory_separator='\');

----------------------------------------------------------------------------




	search : search_type;
	directory_entry : directory_entry_type;


	--type linearray is array(1..3) of string(1..60);
	--line: linearray;


	ch: character;

	erase,changed, userselect,
	userexit, help, Ok, winner, playedonce : boolean := false;


	infilname : unbounded_string;


	epsilon : constant float := 0.01;

	selLine: integer:=1;

	updir : string := "../../";








	package myint_io is new text_io.integer_io(integer);
	package myfloat_io is new text_io.float_io(float);




procedure myassert( condition : boolean;  flag: integer:=0 ) is
begin
  if condition=false then
  		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put_line( "@ " & integer'image(flag) );
		end if;
		new_line;
  		raise program_error;
  end if;
end myassert;




------------ addendum begin --------------------------------



	totgame: integer := 0;

	fmax : integer; --current #files
	maxfmax : constant integer := 90;
	gamefiles, shortname : array(1..maxfmax) of unbounded_string;



procedure sortGames is
	ubstr: unbounded_string;
	use ada.characters.handling;
begin
	-- it seems file search does not return a sorted list...
	-- this proc sorts shortName(),gamefiles(),mxlev() arrays

	--20oct23: reversed ordering: changed "<" to ">" below

	-- begin bubble sort on 1st char
	for i in reverse 1..fmax loop
		for j in reverse 1..i-1 loop

			--case-aware UBstring sort:
			--if shortName(i) < shortName(j) then

			--case-unaware first letter sort
			--if to_lower(element(shortName(i),1)) 
			--	< to_lower(element(shortName(j),1)) then

			--case-unaware string sort:
			if   to_lower(to_string(shortName(i))) 
				> to_lower(to_string(shortName(j))) then -- is >, was <

				--swap i/j
				ubstr := shortName(i);
				shortName(i) := shortName(j);
				shortName(j) := ubstr;

				ubstr := gamefiles(i);
				gamefiles(i) := gamefiles(j);
				gamefiles(j) := ubstr;

				--nsav := mxlev(i);
				--mxlev(i):=mxlev(j);
				--mxlev(j):=nsav;

			end if;
		end loop; --j
	end loop; --i
	-- end bubble sort

end sortGames;


procedure loadGames is
begin
------- begin dynamic read of ./savedGames/ directory -------------------------

	-- find *.sok files under ./games/
	put("Loading Game Files...");
	--put_line("Here are the sok files found under ./games/ :");
	totgame:=0;
	start_search( search, "./savedGames/", "*.txt" );
	while more_entries( search ) loop
		get_next_entry( search, directory_entry );
		totgame:=totgame+1;

		--myassert( totgame <= maxfmax ,1350 );
		gamefiles(totgame)  := 
			to_unbounded_string( full_name( directory_entry ) );
		shortName(totgame):= 
			to_unbounded_string( simple_name(directory_entry) );

	end loop; -- while more_entries
	fmax:=totgame;
	put_line("totgame="&integer'image(totgame));
	new_line;


------- end dynamic read of ./games/ directory -------------------------

	sortGames;

	--put_line("Here are the sorted sok files found under ./games/ :");
	--for i in 1..fmax loop
	--	put(shortName(i)&", #lev=");
	--	put( integer'image( mxlev(i) ) );
	--	new_line;
	--end loop;
	--new_line;

end loadGames;







------------ addendum end --------------------------------













procedure Draw is
	info: terminal_info;
	Ok: boolean;
	--ch: character;
begin

	info.init_for_stdout(auto);


	if mswin then
		--SysUtils.bShell("cls", Ok); -- erase-terminal
		--tput00_h.cursorHome;
		cls_h.cursorHome;
	else
		SysUtils.bShell("clear", Ok); -- erase-terminal
		--SysUtils.bShell("tput cup 0 0", Ok); -- erase-terminal
	end if;


if osx then
	put_line("On OSX, window focus is quirky...");
	put_line("Click on this window to give it focus...");
	new_line;
end if;

	put_line("Select Game to resume by date/time...");
	put_line("Use arrow keys to move Green selector Up or Down");
	put_line(" <enter> or <space> to select;  q = quit");
	new_line;




-- colors available:
-- black,red,green,yellow,blue,magenta,cyan,grey
	Info.Set_Color (background=>black);


	for row in 1..fmax loop

		if row=selLine then
			Info.Set_Color (foreground=>green);
		else
			Info.Set_Color (foreground=>grey);
		end if;

		put( shortname(row) );
		new_line;
	end loop;

   Info.Set_Color (Standard_Output, Style => Reset_All);

	if fmax < totgame then
		put("...list is trucated due to window size...");
		new_line;
	end if;

end Draw;










procedure handle_key_down( ch: character ) is
begin


-- note that arrow keys typically produce chars
-- preceded by 1 or 2 non-printable chars.
--
-- on Linux:		<home>='H'	<end>='F'
--   A		
-- D B C
--
-- or on MSWin:	<home>='G'	<end>='O'
--   H
-- K P M




if character'pos(ch)=13 then -- <return/enter> on MsWin
	userselect:=true;

elsif character'pos(ch)=10 then -- <return/enter>
	userselect:=true;

elsif ch=' ' then
	userselect:=true; -- 23sep21

elsif 
	ada.characters.handling.is_letter(ch) 
	or (ch='?')
then

	changed:=true;
	case ch is

		when 'x' | 'q' =>	userexit:=true;

		when 'H' | 'A' =>	--Up
			selLine:=selLine-1;
			if selLine<1 then selLine:=fmax; end if;

		when 'P' | 'B' =>	--Dn
			selLine:=selLine+1;
			if selLine>fmax then selLine:=1; end if;

		when 's' => --select this one
			userselect:=true;

		when others => changed:=false;

	end case;

end if;
end handle_key_down;












	rtime: interfaces.c.int;


	winrows, wincols: integer;
	info0: terminal_info;

begin --nexus

	info0.init_for_stdout(auto);
	wincols:=get_width(info0);
	winrows:=get_lines(info0);

	--FTTB, this is width test
	if wincols<50 then
		put("Please widen this window to 50 chars."); new_line;
		raise program_error;
	end if;



	if mswin then
		rtime:=realtime.hiPriority;
		-- note:  this seems necessary because some, 
		-- but not all, windows commandline terminals 
		-- seem to randomly freeze at normal priority.
	else
		rtime:=1;
	end if;





	--get list of saved game files:
	loadgames; --note that fmax == totgame

	if fmax+9 > winrows then --we have a problem
		fmax := winrows-9; --show as many as possible; most recent 1st
	end if;


	if mswin then
		SysUtils.bShell("cls", Ok); -- erase-terminal
	else
		SysUtils.bShell("clear", Ok); -- erase-terminal
	end if;



	changed:=true;
	Draw;
	while not userexit loop
		get_immediate(ch);
		handle_key_down( ch );

		if userselect then

			userexit:=true;
			infilname := gamefiles(selLine);

		end if;

		Draw;
	end loop;



	if mswin then
		SysUtils.bShell("cls", Ok); -- erase-terminal
	else
		SysUtils.bShell("clear", Ok); -- erase-terminal
	end if;

put("File Selected: |");
put(infilname);
put("|"); new_line;

	return infilname;

end selectfile;

