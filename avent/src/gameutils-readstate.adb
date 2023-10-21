separate(gameutils)


procedure readState( statefile: in string ) is
	linestr: string(1..30);
	last,last2: natural;
	bstr: string(1..21);
	fstate: float;
	istate: integer;
	sfil: text_io.file_type;
begin
	text_io.open(
		file=>sfil,
		name=>statefile,
		mode=>text_io.in_file);

	get_line(sfil, bstr, last); --boolean array
	skip_line(sfil);


	if bstr(1)='T'  then drawchalice:=true; else drawchalice:=false; end if;
	if bstr(2)='T'  then chalicegone:=true; else chalicegone:=false; end if;
	if bstr(3)='T'  then gateheld:=true; else gateheld:=false; end if;
	if bstr(4)='T'  then bkeyheld:=true; else bkeyheld:=false; end if;
	if bstr(5)='T'  then gkeyheld:=true; else gkeyheld:=false; end if;
	if bstr(6)='T'  then wkeyheld:=true; else wkeyheld:=false; end if;
	if bstr(7)='T'  then swordheld:=true; else swordheld:=false; end if;
	if bstr(8)='T'  then chaliceheld:=true; else chaliceheld:=false; end if;
	if bstr(9)='T'  then bdragondead:=true; else bdragondead:=false; end if;
	if bstr(10)='T' then rdragondead:=true; else rdragondead:=false; end if;
	if bstr(11)='T' then minotaurdead:=true; else minotaurdead:=false; end if;
	if bstr(12)='T' then labopen:=true; else labopen:=false; end if;
	if bstr(13)='T' then mazeopen:=true; else mazeopen:=false; end if;
	if bstr(14)='T' then lionopen:=true; else lionopen:=false; end if;
	if bstr(15)='T' then gateopen:=true; else gateopen:=false; end if;
	if bstr(16)='T' then interior:=true; else interior:=false; end if;

	if bstr(17)='T' then bat1sent:=true; else bat1sent:=false; end if;
	if bstr(18)='T' then bat56sent:=true; else bat56sent:=false; end if;

	if bstr(19)='T' then bat7sent:=true; else bat7sent:=false; end if;
	if bstr(20)='T' then bat9sent:=true; else bat9sent:=false; end if;

	if bstr(21)='T' then play9:=true; else play9:=false; end if;

--put("ok @ 4218");
--new_line;

	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	chapter:=istate; --line 2

	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	scene:=istate; -- line 3

	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	schalice:=istate; --line 4


	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	sbkey:=istate; --line 5


	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	sgkey:=istate; --line6


	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	swkey:=istate; --line 7


	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	ssword:=istate; --line 8


	get_line(sfil, linestr, last); --integer
	myint_io.get(linestr(1..last),istate,last2);
	kgate:=istate; --line 9


	get_line(sfil, linestr, last); --integer (gate scene)
	myint_io.get(linestr(1..last),istate,last2);
	sgate:=istate;


	get_line(sfil, linestr, last); --integer (gate row)
	myint_io.get(linestr(1..last),istate,last2);
	rgate:=istate;


	get_line(sfil, linestr, last); --integer (gate col)
	myint_io.get(linestr(1..last),istate,last2);
	cgate:=istate;


	get_line(sfil, linestr, last); --integer (gate col)
	myint_io.get(linestr(1..last),istate,last2);
	bsdra:=istate;


	get_line(sfil, linestr, last); --integer (gate col)
	myint_io.get(linestr(1..last),istate,last2);
	rsdra:=istate;



----------------------------------------------------------
	--chalice

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xchalice:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ychalice:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zchalice:=fstate;

	--me

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xme:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	yme:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zme:=fstate;

	--gate

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xgate:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ygate:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zgate:=fstate;

	--Bkey

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xbkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ybkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zbkey:=fstate;


	--Gkey

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xgkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ygkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zgkey:=fstate;

	--Wkey

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xwkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ywkey:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zwkey:=fstate;



	--Rdra

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	rxdra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	rydra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	rzdra:=fstate;


	--Bdra

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	bxdra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	bydra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	bzdra:=fstate;



	--Mdra

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	mxdra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	mydra:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	mzdra:=fstate;







	--sword
	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	xsword:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	ysword:=fstate;

	get_line(sfil, linestr, last); --float
	myfloat_io.get(linestr(1..last), fstate,last2);
	zsword:=fstate;



-----------------------------------------------------------------
	text_io.close(sfil);

	--printstate;


end readState;


