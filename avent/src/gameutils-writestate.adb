separate(gameutils)


procedure writeState( aborting: boolean := false ) is

use ada.strings.unbounded;
use ada.calendar;
use ada.calendar.time_zones;
use ada.calendar.formatting;

	sfil: text_io.file_type;

	--usfname: unbounded_string;


	function i2st( i: integer ) return string is
	begin
		if i>=0 and i<=9 then
			return ada.strings.fixed.trim( integer'image(i), ada.strings.left);
		else
			return "";
		end if;
	end i2st;




	function dt_string return string is

		--TZ is the timezone adjustment.
		--If it is set at compile time,
		--it represents California Time.
		--However, UTC_Time_Offset is a function
		--so I am not sure about this.
		TZ: Time_Offset := UTC_Time_Offset;
		Now: Time := Clock;

		--1st numeral is 1..4 indicates "chapter":
		retstr : string := -- / should be no problem on MsWin!
			"savedGames/" 
			&Image(Now,True,TZ) 
			&"_"
			& i2st(Chapter) 
			& ".txt";

		--20oct23 changed format so chapter is last. This allows
		--a name-sort to show most recent saves first.

	begin
		--caution: retstr returns a string with embedded blanks/colons..
		-- so cannot be used for filenames as-is.

		for i in 1..retstr'length loop

			if retstr(i)=' ' then
				retstr(i):='_';
			end if;

			if retstr(i)=':' then --happens under MsWin!
				retstr(i):='_';
			end if;

		end loop;

		return retstr;

	end dt_string;

	-- use chapter+date+time to create file name...
	fname: constant string := dt_string;

begin

--put_line(" WriteState: Ok 0");

if aborting then
	text_io.create(
		file=>sfil,
		name=>statefilex, -- data/gamestatex.txt
		mode=>text_io.out_file);

	new_line;
	put(" Aborting...game state saved to: "&statefilex);
	new_line;

else

--put_line(" WriteState: Ok 1");
--put("...writing to file: ");
--put_line( fname );

	text_io.create(
		file=>sfil,
		name=>  fname,
		mode=>text_io.out_file);

--put_line(" WriteState: Ok 2");

	new_line;
	put(" User game saved to: "&fname);
	new_line;

end if;

--put_line(" WriteState: Ok 3");

	if drawchalice then put(sfil,"T"); else put(sfil,"F"); end if;
	if chalicegone  then put(sfil,"T"); else put(sfil,"F"); end if;
	if gateheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if bkeyheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if gkeyheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if wkeyheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if swordheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if chaliceheld then put(sfil,"T"); else put(sfil,"F"); end if;
	if bdragondead then put(sfil,"T"); else put(sfil,"F"); end if;
	if rdragondead then put(sfil,"T"); else put(sfil,"F"); end if;
	if minotaurdead then put(sfil,"T"); else put(sfil,"F"); end if;
	if labopen then put(sfil,"T"); else put(sfil,"F"); end if;
	if mazeopen then put(sfil,"T"); else put(sfil,"F"); end if;
	if lionopen then put(sfil,"T"); else put(sfil,"F"); end if;
	if gateopen then put(sfil,"T"); else put(sfil,"F"); end if;
	if interior then put(sfil,"T"); else put(sfil,"F"); end if;

	if bat1sent then put(sfil,"T"); else put(sfil,"F"); end if;
	if bat56sent then put(sfil,"T"); else put(sfil,"F"); end if;

	if bat7sent then put(sfil,"T"); else put(sfil,"F"); end if;
	if bat9sent then put(sfil,"T"); else put(sfil,"F"); end if;

	if play9 then put(sfil,"T"); else put(sfil,"F"); end if;

	new_line(sfil);

--put_line(" WriteState: Ok 4");


	put_line(sfil, integer'image(chapter));
	put_line(sfil, integer'image(scene));
	put_line(sfil, integer'image(schalice));
	put_line(sfil, integer'image(sbkey));
	put_line(sfil, integer'image(sgkey));
	put_line(sfil, integer'image(swkey));
	put_line(sfil, integer'image(ssword)); --line 8 scene of sword
	put_line(sfil, integer'image(kgate)); --line 9
	put_line(sfil, integer'image(sgate)); --line 10 scene of gate
	put_line(sfil, integer'image(rgate));
	put_line(sfil, integer'image(cgate));

	put_line(sfil, integer'image(bsdra));
	put_line(sfil, integer'image(rsdra));

	--put_line(sfil, integer'image(gk1));
	--put_line(sfil, integer'image(gzmk));

-----------------------------------------------------
	put_line(sfil, float'image(xchalice));
	put_line(sfil, float'image(ychalice));
	put_line(sfil, float'image(zchalice));

	put_line(sfil, float'image(xme));
	put_line(sfil, float'image(yme));
	put_line(sfil, float'image(zme));

	put_line(sfil, float'image(xgate));
	put_line(sfil, float'image(ygate));
	put_line(sfil, float'image(zgate));

	put_line(sfil, float'image(xbkey));
	put_line(sfil, float'image(ybkey));
	put_line(sfil, float'image(zbkey));

	put_line(sfil, float'image(xgkey));
	put_line(sfil, float'image(ygkey));
	put_line(sfil, float'image(zgkey));

	put_line(sfil, float'image(xwkey));
	put_line(sfil, float'image(ywkey));
	put_line(sfil, float'image(zwkey));




	put_line(sfil, float'image(rxdra));
	put_line(sfil, float'image(rydra));
	put_line(sfil, float'image(rzdra));

	put_line(sfil, float'image(bxdra));
	put_line(sfil, float'image(bydra));
	put_line(sfil, float'image(bzdra));

	put_line(sfil, float'image(mxdra));
	put_line(sfil, float'image(mydra));
	put_line(sfil, float'image(mzdra));


	put_line(sfil, float'image(xsword));
	put_line(sfil, float'image(ysword));
	put_line(sfil, float'image(zsword));




--put_line(" WriteState: Ok 5");



	text_io.close(sfil);

	--printstate;

--put_line(" WriteState: Ok 6");

end writeState;


