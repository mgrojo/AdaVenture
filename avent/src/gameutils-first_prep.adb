separate(gameutils)

procedure first_prep(HiRes: boolean:=false) is -- main program setup
	linestr: string(1..9);
	last,last2: natural;
begin

	--oldstateexists:=false;
	--if ada.directories.Exists(statefile) then
	--	oldstateexists:=true;
	--end if;

	--First, Initialize all Sounds:

	snd4ada.initSnds;



	introsong := snd4ada.initLoop( --16nov19
		Interfaces.C.Strings.New_String("data/Old_Tower_InnCC0.wav"));

------- addendum 15dec17 begin ---------------------------------------

	evilBugs := snd4ada.initLoop( --16nov19
		Interfaces.C.Strings.New_String("data/evil6.wav"));
		--Interfaces.C.Strings.New_String("data/evilRoaches.wav"));

	wind1 := snd4ada.initLoop( --1dec18
		Interfaces.C.Strings.New_String("data/windb.wav"));


--------- remove cc-by-nc begin -----------------------------------

	atmos8 := snd4ada.initLoop( -- play always in M8
		Interfaces.C.Strings.New_String("data/rainForest.wav"));
		--Interfaces.C.Strings.New_String("data/ceph.wav"));

	dang8 := snd4ada.initLoop(  -- play in M8 when in danger
		Interfaces.C.Strings.New_String("data/ceph.wav"));
		--Interfaces.C.Strings.New_String("data/indusBeat2sec.wav"));

--------- remove cc-by-nc end -----------------------------------


	tmpl4 := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/spChoirCC3.wav"));
		--Interfaces.C.Strings.New_String("data/westfold.wav"));

------- addendum 15dec17 end ---------------------------------------

	water := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/water.wav")); --bearLake



	misr := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/msrlu.wav")); --9.3Mb

	turk := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/trksh.wav")); --5.7Mb



	angv := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/AngevinB.wav")); --5.4Mb

	ibn := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/Ibn-Al-Noor.wav")); --9.2Mb





	hiss := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/hiss-beat.wav"));



	smallwf := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/smallWF.wav"));

	toxic := snd4ada.initLoop(
		Interfaces.C.Strings.New_String("data/fs-ultralow.wav"));


------- now for transient sounds ==================================


	gameover := snd4ada.initSnd( -- 9feb24
		Interfaces.C.Strings.New_String("data/freesound_game-over.wav"));



	stone := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/concrete.wav"));

	down := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/putdown.wav"));

	up := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/pickup.wav"));

	die := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/dragondie.wav"));

	eat := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/eaten.wav"));

	roar := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/roar.wav"));

	won := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/won.wav"));

	medusascream := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/medusa.wav"));

	girlyscream := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/girly_scream.wav"));

	monsterscream := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/monster.wav"));

	womanscream := snd4ada.initSnd(
		Interfaces.C.Strings.New_String("data/woman_scream.wav"));


	if 
		dang8<0 or atmos8<0 or tmpl4<0 or
		water<0 or misr<0 or turk<0 or hiss<0 or
		stone<0 or down<0 or up<0 or
		die<0 or eat<0 or roar<0 or won<0 or 
		medusascream<0 or girlyscream<0 or 
		monsterscream<0 or womanscream<0
	then
		put_line("snd4ada.initSnds ERROR");
		raise program_error;
	end if;



------- begin glfw prep ---------------------------------------------------------


	InitGlfwFs("AdaVenture");
	zoomwheel.enable(mainWindow);



	glgenvertexarrays(1, vertexarrayid'address );
	glbindvertexarray(vertexarrayid);

	-- from the literature it seems I might not have to
	-- call this explicitly because the first texture
	-- unit is the active texture unit, by default.
	-- And I have no multi-texturing needs yet,
	-- like a tarnish on top of an existing texture.
	glactivetexture(gl_texture0); -- moved here 5nov14 (outside main loop)

	glgenbuffers(1, vertbuff'address);
	glgenbuffers(1, normbuff'address);
	glgenbuffers(1, rgbbuff'address);
	glgenbuffers(1, uvbuff'address);
	glgenbuffers(1, elembuff'address);




	glenable(gl_depth_test);
	gldepthfunc( gl_lequal );
	glenable( gl_cull_face );



	-- reduces aliasing:
	glEnable(GL_MULTISAMPLE); -- default setting anyway


	--minimal error test
	if gl_no_error /= glGetError then
		put_line(" GLerror in gameutils.first_prep");
	end if;


	level:=0;



-- insert here attempt to read "settings.txt" file
	if ada.directories.Exists(setfile) then 
		put("settings file found...");

		declare
			xfspd, xmslu, xkslu, xgslu, xjslu: float;
		begin

			text_io.open(tfile, in_file, setfile);

			text_io.get_line(tfile,linestr,last);
			myfloat_io.get(linestr(1..last),xfspd,last2);

			text_io.get_line(tfile,linestr,last);
			myfloat_io.get(linestr(1..last),xmslu,last2);

			text_io.get_line(tfile,linestr,last);
			myfloat_io.get(linestr(1..last),xkslu,last2);

			text_io.get_line(tfile,linestr,last);
			myfloat_io.get(linestr(1..last),xgslu,last2);

			text_io.get_line(tfile,linestr,last);
			myfloat_io.get(linestr(1..last),xjslu,last2);

			text_io.close(tfile);

			fspd:=xfspd;
			mslu:=xmslu;
			kslu:=xkslu;
			gslu:=xgslu;
			jslu:=xjslu;
			put_line("and used.");

		 exception
			when others =>
				put_line("...problem reading settings file...using defaults.");
				text_io.close(tfile);

		end; --declare

	else
		put_line("No settings file found; using defaults.");
	end if;



end first_prep;





