--
-- Copyright (C) 2024  <fastrgv@gmail.com>
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


with ada.numerics.discrete_random;

with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

-------------------------------------------------------------
with System;
with Interfaces.C;
use  type interfaces.c.unsigned;
with Interfaces.C.Pointers;
with interfaces.c.strings;


----------------------------------------------------------------

with glfw3; use glfw3;
with zoomwheel;

----------------------------------------------------------------

with matutils;
with ftex;
with utex;

with ada.unchecked_conversion;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Directories;

----------------------------------------------------------------


with shader;  use shader;

with cubemapobj;
with droomobj;
with xtreeobj;
with w3treeobj;
with bugobj;
with rectobj;
with rectxobj;
with avatarolay;
with pictobj;
with twictobj;
with cylobj;
with cyl2texobj;

with holesurfobj;


with text_io;
with pngloader;
with gametypes;
with gameutils;
with matutils;

with ada.calendar;

with snd4ada;



package body avent is


	use text_io;
	use pngloader;
	use gametypes;
	use gameutils;
	use matutils;
	use interfaces.c;
	use interfaces.c.strings;
	use glext;
	use glext.pointers;
	use glext.binding;
	use gl;
	use gl.binding;
	use gl.pointers;


	nerr: integer;






	hdang: float := 0.0;

	sz: float;

	keytime,
	winnertime, warningtime: gldouble:=0.0;
	warningpause: constant gldouble := 3.0;
	warningreset: constant gldouble := 60.0;

	keydwell: constant gldouble := 2.0; --seconds
	dxme : float := 0.0;
	dt: gldouble := 0.0;

	resuming,
	reenter, roarbegun, runAway: boolean:=false; --used for red dragon

	dbug: boolean := false;

	avent_main_error : exception;


	fontcol: constant vec4 := (1.0,1.0,1.0,1.0); --white


	exeName: constant string := ada.command_line.command_name;
	onMac : boolean := ( ada.strings.fixed.index(exename,"_osx",1)>1 );





-- Exterior rolling hills:
-- this new surface allows castle to extend below it...
-- (x,z) in [-10..10, -10..10] => 20X20, 
-- with square hole @ (-5,-5) of radius 5:
package terrain is new holesurfobj(10,10,land_alt,-5.0,-5.0,5.0,5.0);
grounds : terrain.holesurf;


procedure setup_maze9 is separate;
procedure draw_maze9 is separate;


procedure setup_lab8 is separate;
procedure draw_lab8 is separate;

procedure setup_maze7 is separate;
procedure draw_maze7 is separate;

procedure setup_maze5 is separate;
procedure setup_maze6 is separate;
procedure draw_maze5 is separate;
procedure draw_maze6 is separate;

procedure setup_exterior(chapter: integer) is separate;
procedure draw_exterior(chapter: integer) is separate;

procedure setup_castle is separate;
procedure draw_castle is separate;


procedure setup_temple is separate;
procedure draw_temple is separate;




procedure release_textures is separate;
procedure setup_textures is separate;



type itemtype is (pgate, gkee, bkee, wkee, srd, cup);

procedure dropitem( item: itemtype ) is separate;



-- decide if object is picked
procedure pickLeft( item: itemtype ) is separate;

procedure drawAvatar( mytime : gldouble ) is separate;

procedure pickOrDrop is separate;

procedure handle_mouse_click( nowTime : gldouble ) is separate;

procedure getKeyInputs( mainWin : access GLFWwindow ) is separate;









procedure aventure( --called from adaventure.adb
	inchapter: integer; 
	jump: integer:=0;
	resume: boolean:=false;
	HiRes: boolean:=false --caller sends TRUE, unless cmdLnPrm=0
	) is


	onetime : boolean := false;

	axs, ays : aliased float;

	tstart: gldouble;

-------------------------- main program begin ==========================
begin --adaventure

	resuming:=resume;

	chapter:=inchapter;


	new_line;
	new_line;
	put_line("Please be patient...AdaVenture is slow to load...");
	put_line("...starting Level " & integer'image(chapter) );
	new_line;
	new_line;


	first_prep(HiRes);  -- main program setup; hires=true unless cmdLnPrm=0
	-- NOW, we may begin tesing for GLerrors


	nerr:=dumpGLerrorQueue("AV.main_1");


	emptyGLerrorQueue; -- avoid misleading messages in setup_textures
	setup_textures; -- prep various textures

	nerr:=dumpGLerrorQueue("AV.main_2");


	initializeNewMazes;



	nko:=0;
	setup_exterior(chapter);


	setup_castle;

	if chapter=1 or chapter=3 then
		setup_maze5;
		setup_maze6;
		setup_temple;
	elsif chapter=2 or chapter=4 then
		setup_maze7;
		setup_lab8;
		setup_maze9;
	end if;
	-- note:  darkness set in draw_exterior...
	-- ch 1:  if chaliceheld then d:=1 (bright day) else d:=2
	-- ch 2:  if chaliceheld then d:=2 (dark night) else d:=1

	put("nko="&integer'image(nko));
	put_line(", maxnko="&integer'image(maxnko));



	xtreeobj.setrect(chalice);

	nuscene:=1; -- starting scene
	xme:= -5.0;
	zme:=-15.0;
	yme:=aheight+land_alt(xme,zme);

	if oldstateexists and resuming then -- NOT debugging

	--hopefully, what follows will overwrite any contrary 
	--initializations made above.

		--put_line("avent: Final readstate");
		readState("data/gamestate.txt");
		setState; -- prep objects to resume
		nuscene:=scene;

		--30oct19:
		if scene=8 then
			scene:=0; --forces entry logic (eg. music lab8)
			--successful on lab8,
		end if;

		if not interior then --better accuracy
			yme:=aheight+land_alt(xme,zme);
		end if;

		myassert(jump<3);

	end if; --resuming


----------------------------------------------------------------------
-- initialize black key, now that we know or have restored its coords;
-- same code whether M5, M6, or M7.

		ybkey:=-iymax+htobj+htobj; -- -3+.02+.02 = -2.96

		pictobj.setrect( 
			key2, 
			xbkey,ybkey,zbkey, --xc,yc,zc
			rkey, 0.0, 0.5*rkey, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

		pictobj.setrect( --ghostkey
			key0, 
			xbkey,-htobj-htobj,zbkey, --xc,yc,zc
			rkey, 0.0, 0.5*rkey, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);
----------------------------------------------------------



	if sbkey=6 then 
		put("Bkey in M6"); 
	elsif sbkey=5 then
		put("Bkey in M5");
	elsif sbkey=7 then
		put("Bkey in M7");
	end if; 
	new_line;


	xcam:=xme; ycam:=yme; zcam:=zme;



-- these following jump clauses ONLY for debug:
	-- chapter 1---------------
	-- 3=>castle, 4=>temple, 
	--	5=>maze5, 6=>maze6, 
	-- chapter 2---------------
	-- 7=>maze7, 8=>lab8, 9=maze9
	--	any other number => normal start


if jump=3 then --debug just outside castle with chalice

	-- with chalice:
	xchalice:=-5.0;
	ychalice:=htobj+hcup;
	zchalice:=-5.0;
	schalice:=2;
	intro:=false;

	-- debug quick access key:
	xwkey:=-5.0; zwkey:=-13.0; swkey:=1;
	ywkey:=0.2+htobj+land_alt(xwkey,zwkey);
	pictobj.setrect( key1, xwkey,ywkey,zwkey,
	0.1, 0.0, 0.1, j1,j2,j3,j4,j5,j6);


elsif jump=9 then --maze9 (orange2)

	nuscene:=9;
	scene:=9;
	interior:=true;
	xme:= -9.6;
	zme:= -9.6; --7.0;
	yme:=-iymax+aheight;
	gateheld:=true;
	kgate:=0; --=>gateway inactive
	intro:=false;
	minotaurdead:=true;
	horiang:=+halfpi;

elsif jump=8 then --lab

	nuscene:=8;
	scene:=7;
	interior:=true;
	xme:= 2.0;
	zme:= 2.0;
	yme:=-iymax+aheight;
	swordheld:=true;
	ssword:=8;
	intro:=false;
	labopen:=true;

elsif jump=7 then --maze7:

	nuscene:=7;
	scene:=7;
	interior:=true;
	xme:= 0.0;
	zme:= 0.0;
	yme:=-iymax+aheight;
	intro:=false;

	swordheld:=true;
	ssword:=7;
	-- want open door, holding cup
	--chaliceheld:=true;
	--schalice:=7;
	mazeopen:=true;

elsif jump=5 then --maze5:

	nuscene:=5;
	scene:=5;
	interior:=true;
	xme:= 0.0;
	zme:= 0.0;
	yme:=-iymax+aheight;
	swordheld:=true;
	ssword:=5;
	intro:=false;
	mazeopen:=true;

elsif jump=6 then --maze6

	nuscene:=6;
	scene:=5;
	interior:=true;
	xme:= 0.0;
	zme:= 0.0;
	yme:=-iymax+aheight;
	--swordheld:=true;
	bkeyheld:=true;
	ssword:=6;
	intro:=false;

elsif jump=4 then --greek temple:

	nuscene:=4;
	scene:=5;
	interior:=true;
	xme:= 0.0;
	zme:= 0.0;
	yme:=-iymax+aheight;
	swordheld:=true;
	ssword:=4;
	intro:=false;

end if;



-- scene 1: exterior 		(ch1,2)
-- scene 2: castle			(ch1,2)
-- scene 3: NONE...deleted
-- scene 4: greek temple	(ch1)
-- scene 5: maze5				(ch1)
-- scene 6: maze6				(ch1)
-- scene 7: maze7				(ch2)
-- scene 8: labyrinth		(ch2)
-- scene 9: darkMaze			(ch2)



-------------------------- begin insert 29apr20 --------------------------

	-- hopefully, X11 windows have been "set" by now:


	glfwGetWindowSize(mainWindow, winwidth'access, winheight'access);
	glfwGetFramebufferSize(mainwindow, fwid'access, fhit'access);
	glfwGetWindowContentScale(mainWindow, axs'access,ays'access);

	glViewport(0,0,fwid,fhit);

	if axs>1.5 or ays>1.5 then
		hidpi:=true;
		put_line("HiDpi");
	else
		hidpi:=false;
		put_line("NOT HiDpi");
	end if;


	emptyGLerrorQueue; --avoid misleading messages in utex

	utex.inittext2d(
		"data/sansw.png", integer(winwidth),integer(winheight));
	put_line( "Window: wid-X-hit :" 
		& interfaces.c.int'image(winwidth)&" X "
		& interfaces.c.int'image(winheight) );


	put_line( "Drawable: Fwid-X-Fhit : "
		&interfaces.c.int'image(Fwid)&" X "
		& interfaces.c.int'image(Fhit) );

-------------------------- end insert 29apr20 --------------------------

	currentTime := glfwGetTime;
	updateMVPs( currentTime, float(winwidth), float(winheight) );




	-- prepare font-related shaders & uniforms -------------
	ftex.InitFont ( winwidth, winheight, "data/Canterbury.ttf" );






	nerr:=dumpGLerrorQueue("AV.main_3");
	-- begin mainloop with a clean slate


	if intro then
		snd4ada.playLoop(introsong); --19jan23 addendum
	end if;



	-- main event loop begin: ------------------------------------------
   while not userexit loop

		direction:=0;

		if nuscene /= scene then

			if nuscene=1 then interior:=false;
			else interior:=true; end if;


			if nuscene=1 then
				if scene=2 and seen1already 
				and not bdragonsent and not bdragondead then
					bdragonfly:=true;
					dragonstart:=glfwGetTime;

					bdragonsent:=true;
				end if;
				seen1already:=true; 
			end if;


			if nuscene=5 then -- chapter 1
				if scene=1 
				and not rdragonsent and not rdragondead then
					rdragonfly:=true;
					runAway:=false; --deadly encounter
					dragonstart:=glfwGetTime;

					rdragonsent:=true;
				end if;
			end if;


			-- NewStoryLine chapter 2/4:
			-- send red dragon on first exit of Lab8 
			-- (old) if Minotaur is dead
			-- (new) if Chalice has been found
			-- this prevents simply sneaking out 
			-- [with no sword] with chalice...
			if 
				(nuscene=7) and (scene=8) and 
				(schalice<9) -- i.e. chalice has been found and moved
				and not rdragondead -- limit to single deadly encounter
			then --return from Lab
				--send Red Dragon and hope user has sword!
				rdragonfly:=true;
				runAway:=false; --2nd encounter is deadly
				dragonstart:=glfwGetTime;

			elsif nuscene=7 and chaliceheld and not bat7sent then 
				-- send bat to grab chalice
				sendBat; -- just once per scene
				bat7sent:=true; --interior: bat grabs chalice

			elsif nuscene=7 and scene=1 and not chaliceheld then --1st maze entry
				--send Red Dragon and hope user has sword!
				rdragonfly:=true;
				runAway:=true; --runs away @ first encounter
				dragonstart:=glfwGetTime;

			elsif nuscene=1 and scene=7 and chaliceheld and not bat9sent then
				sendBat; -- just once per scene
				bat9sent:=true; --exterior: bat grabs chalice

			end if;


			-- call is always Ok, even if no music is playing...
			-- But, do not interrupt if Korla is playing.
			if not (success and heralded) and not intro then
				snd4ada.stopLoops; --21jul18
			end if;



			scene:=nuscene; --<<<<<<<<<<<<<<<<<<<<<<<<<<<
			if chaliceheld then schalice:=scene; end if;
			if swordheld then ssword:=scene; end if;
			if wkeyheld then swkey:=scene; end if;
			if gkeyheld then sgkey:=scene; end if;
			if bkeyheld then sbkey:=scene; end if;
			if gateheld then sgate:=scene; end if;




			if scene=4 or jump=4 then --temple(choir)
				if not play4 then
					snd4ada.playLoop(tmpl4);
					play4:=true;
				end if;
			elsif scene=8 or jump=8 then --labyrinth(cephalopod)
				if 
					play9            -- return entry...fireball danger
					or
					not minotaurdead -- first entry...minotaur danger
				then
					snd4ada.playLoop(atmos8); --always
				end if;
			elsif scene=2 then --castle pool
				snd4ada.playLoop(water);

			elsif scene=1 and not (success and heralded) then

				snd4ada.playLoop(wind1); --1dec18

			end if;

			--19dec16 addendum to correct direction after entry
			ahoriang:=horiang;
			choriang:=horiang;
			updateCamera(true); 
			-- initialize=true => reset (xcam,zcam) whether moving or not

		end if; -- nuscene /= scene =================================

		-- dangerous bug sounds:  toggle if appropriate
		if bugloop then

			if 
				--(scene=6 and (zme>-4.5 or abs(xme)>8.0 ) ) update 10apr21
				(scene=6 and (zme>-5.0 or abs(xme)>6.0 ) ) 
					or 
				(scene=7 and (zme<5.0 or xme>0.0))
					or 
				(scene=5)
			then
				snd4ada.stopLoop(evilbugs);
				bugloop:=false;
				warn_bug:=false;
			end if;

		else -- NOT bugloop

			if
				--(scene=6 and (zme<-4.5 and abs(xme)<8.0)) update 10apr21
				(scene=6 and (zme<-5.0 and abs(xme)<6.0)) 
					or 
				(scene=7 and (zme>5.0 and xme<0.0))
			then
				snd4ada.playLoop(evilbugs);
				bugloop:=true;
				bugTimeStart := glfwGetTime;
			end if;

		end if;


		if reenter then
			ahoriang:=horiang;
			choriang:=horiang;
			updateCamera(true); 
			-- initialize=true => reset (xcam,zcam) whether moving or not
			reenter:=false;
		end if;


		if ( scene=5 or scene=6 ) and bkeyseen and not bat56sent then
			sendBat; -- just once per scene
			bat56sent:=true;
		end if;

		if scene=1 and wkeyseen and not bat1sent then
			sendBat; -- just once per scene
			bat1sent:=true;
		end if;








	-- main event loop middle: -----------------------------------------------

------- begin response to user inputs ////////////////////////////////////

if not bugstarted then -- allow inputs, & updates.


		currentTime := glfwGetTime;

		buget:=currentTime-bugTimeStart;
		if bugloop and then bugEt>bugThreshold then
			imdead_bug:=true;

		elsif bugloop and then bugEt>bugThreshold/3.0 then
			warn_bug:=true;

		end if;


		if ((currenttime-warningtime)>warningreset) then
			warning1:=false;
			-- if you avoid the snake for 1 minute
			-- then you will, again, get a warning only
		end if;


		GlfwPollEvents;

		if osx and not onetime then

			glfwRestoreWindow(mainWindow); 
			--3oct23  necessary for OSX
			--(if not iconified, this has no effect)

			onetime:=true;

		end if;

		getKeyInputs(mainWindow);
		handle_mouse_move(currenttime);
		handle_mouse_click(currenttime);


		if gamepad then

			--first, AXES:
			axes := glfwGetJoystickAxes(glfw_joystick_1, akount'access); 
			--gp(0..3), js(0..2)

			buttons := glfwGetJoystickButtons(glfw_joystick_1, bkount'access);
			--gp(0..15), js(0..7)
			-- see glfw3.ads::233

			-- right stik
			axis_rx2 := axes(glfw_gamepad_axis_right_x);
			axis_ry3 := axes(glfw_gamepad_axis_right_y); --NOT on joystik
			handle_gc_look(axis_rx2,axis_ry3,gslu); --lookdir

			-- left stik
			axis_lx0 := axes(glfw_gamepad_axis_left_x);
			axis_ly1 := axes(glfw_gamepad_axis_left_y);
			handle_gc_move(currentTime,axis_lx0,axis_ly1); --forw/back

			-- second, buttons:
			btn4 := buttons(glfw_gamepad_button_left_bumper); 
			btn5 := buttons(glfw_gamepad_button_right_bumper); 
			if 
				(btn4=glfw_press or btn5=glfw_press)
				and (currenttime-pltime)>pickdwell
			then
				pickOrDrop;
				pltime:=currentTime;
				--put_line("Pick/Drop"); --perfect
			end if;

		elsif joystik then

			--first, AXES:
			axes := glfwGetJoystickAxes(glfw_joystick_1, akount'access); 
			-- js(0..2)

			buttons := glfwGetJoystickButtons(glfw_joystick_1, bkount'access);
			-- js(0..7)
			-- see glfw3.ads::233

			-- stik
			axis_lx0 := axes(glfw_gamepad_axis_left_x);
			axis_ly1 := axes(glfw_gamepad_axis_left_y);
			handle_gc_look(axis_lx0,axis_ly1,jslu); --lookdir

			-- second, buttons:
			btn0 := buttons(0); --bak
			btn1 := buttons(1); --for (thumbCenter)
			btn2 := buttons(2); --pik (thumbL)
			btn3 := buttons(3); --pik (thumbR)

			if (btn0=glfw_press) then
				moveBackward(currentTime);
			elsif (btn1=glfw_press) then
				moveForward(currentTime);
			end if;

			if 
				(btn2=glfw_press or btn3=glfw_press)
				and (currenttime-pltime)>pickdwell
			then
				pickOrDrop;
				pltime:=currentTime;
			end if;


		end if;



----////////////// end response to user inputs //////////////////////////


		currentTime:=glfwGetTime;

		updateCamera;
		updateMVPs( currentTime, float(winwidth), float(winheight), true );

else --bugstarted...(do not allow inputs, updates)

		currentTime:=glfwGetTime;

end if; -- not bugstarted




--------- begin drawing =================================================
		glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);










	--tests for scene transitions;  if changing...reset
	--position (xme,yme,zme) to a reasonable entrance value

	-- current status per scene:
	--  1 = exterior
	--  2 = castle
	--  3 = defunct...removed

	--  4 = temple (ch1)
	--  5 = entry from exterior maze (chapter1)
	--  6 = exit to temple maze (chapter1)

	--  7 = entry from exterior, exit to labyrinth maze (ch2)
	--  8 = labyrinth (ch2)
	--  9 = maze9 (other side of lab)





		--exterior
		if scene=1 then

			noso:=0;
			eawe:=0;

			-- economize if insides of castle are not visible
			if zme<-10.0 then -- inside visible
				oyme:=yme;
				yme:=yme+0.1; --need slight boost so floor looks correct

				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_castle; --interior


				-- restore exterior coords
				yme:=oyme;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
			end if;

			draw_exterior(chapter);


			-- threshold castle/exterior:
			-- (ixdoor,izdoor)=(-5.0,-10.0)
			-- ( xdoor, zdoor)=(-5.0,-10.0)
			if 
				atThreshold(currenttime, "2e", 1) --2e
			then
				nuscene:=2;
				xme:=ixdoor; 
				zme:=izdoor+ddoor+0.01;
				yme:=aheight;

			-- threshold exterior1/maze5or7
			elsif 
				atThreshold(currenttime, "1x", 1) --1x
				and mazeopen
				and not mazewait
			then
				if chapter=1 or chapter=3 then	
					nuscene:=5;
				else 
					nuscene:=7; 
				end if;
				xme:=ixmaze;
				zme:=izmaze+dmaze+0.01;
				yme:=-iymax+aheight;
			end if;



		--castle interior
		elsif scene=2 then

			noso:=0;
			eawe:=0;

			--in case we can see outside:
			draw_exterior(chapter); --here, yme is close enough

			draw_castle; --interior

			drawspider(currenttime);


		-- threshold castle/exterior:
		-- (ixdoor,izdoor)=(-5.0,-10.0)
		-- ( xdoor, zdoor)=(-5.0,-10.0)
			if
				atThreshold(currenttime, "2x", 2) --2x
			then
				nuscene:=1;
				xme:=xdoor;
				zme:=zdoor-ddoor-0.01;
				yme:=aheight+land_alt(xme,zme);
			end if;




		--greek temple
		elsif scene=4 then --assuming see-thru to scene#6

			noso:=0;
			eawe:=0;

			--in case we can see maze outside temple:
			zme:=zme+20.0;
			zcam:=zcam+20.0;
			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_maze6;
			zme:=zme-20.0;	 --restore to present value
			zcam:=zcam-20.0;

			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_temple;

			if
				atThreshold(currenttime, "4x", 4) --4x
			then
				nuscene:=6;
				xme:=xtmpl; -- 0
				zme:=ztmpl-dtmpl-0.01; --  ~= +10
				yme:=-iymax+aheight;
			end if;




		--maze part1
		elsif scene=5 then --connected with itself, scene #6, #1=exterior

		-- See scene5.txt, scene6.txt for definitions of
		-- all the inter-connections:
		-- { a,b,c,d,e,f,g,y,z,v,w,x, a1,a2,b1,b2, 
		--   c6,d6,e6,f6,g6,v6,w6,x6,y6,z6 }



			eawe:=0;
			if xme<0.0 and zme>=3.0 then --5yz

				if zme>=6.0 then eawe:=6; --6Y
				else eawe:=7; end if; --6Z

				xme:=xme+20.0;
				xcam:=xcam+20.0;
				zme:=zme-13.0;
				zcam:=zcam-13.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze6;
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore
				zme:=zme+13.0;
				zcam:=zcam+13.0; --restore

				if zme>=6.0 then eawe:=6; --5Y
				else eawe:=7; end if; --5Z

			elsif xme<0.0 and zme<=3.0 then --5vwx

				if zme>=1.0 then eawe:=3; --6V
				elsif zme>=-1.0 then eawe:=4; --6W
				else eawe:=5; end if; --6X

				xme:=xme+20.0;
				xcam:=xcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze6;
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore

				if zme>=1.0 then eawe:=8; --5V
				elsif zme>=-1.0 then eawe:=9; --5W
				else eawe:=10; end if; --5X

			elsif xme>0.0 then --5cdefg

				if    zme>=6.0 then eawe:=8; --6C
				elsif zme>=3.0 then eawe:=9; --6D
				elsif zme>=1.0 then eawe:=10; --6E
				elsif zme>=-1.0 then eawe:=11; --6F
				else eawe:=12; end if;--G

				xme:=xme-20.0;
				xcam:=xcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze6;
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore

				if    zme>=6.0 then eawe:=1; --C
				elsif zme>=3.0 then eawe:=2; --D
				elsif zme>=1.0 then eawe:=3; --E
				elsif zme>=-1.0 then eawe:=4; --F
				else eawe:=5; end if;--G

			end if;


			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_maze5;


-- now, check for imminent transitions...

			--(+10,+9) -> #6 @(-10,+9)
			if atThreshold(currenttime,"5c",5) then --5c
					xme:=xme-20.0;
					nuscene:=6;
					xme:=xme+step;

			
			--(+10,+4) -> #6 @(-10,+4)
			elsif atThreshold(currenttime,"5d",5) then --5d
					xme:=xme-20.0;
					nuscene:=6;
					xme:=xme+step;

			
			--(+10,+2) -> #6 @(-10,+2)
			elsif atThreshold(currenttime,"5e",5) then --5e
					xme:=xme-20.0;
					nuscene:=6;
					xme:=xme+step;

			
			--(+10,+0) -> #6 @(-10,+0)
			elsif atThreshold(currenttime,"5f",5) then --5f
					xme:=xme-20.0;
					nuscene:=6;
					xme:=xme+step;

			
			--(+10,-2) -> #6 @(-10,-2)
			elsif atThreshold(currenttime,"5g",5) then --5g
					xme:=xme-20.0;
					nuscene:=6;
					xme:=xme+step;

			
			--(-10,+2) -> #6 @(+10,+2)
			elsif atThreshold(currenttime,"5v",5) then --5v
					xme:=xme+20.0;
					nuscene:=6;
					xme:=xme-step;

			
			--(-10,+0) -> #6 @(+10,+0)
			elsif atThreshold(currenttime,"5w",5) then --5w
					xme:=xme+20.0;
					nuscene:=6;
					xme:=xme-step;

			
			--(-10,-2) -> #6 @(+10,-2)
			elsif atThreshold(currenttime,"5x",5) then --5x
					xme:=xme+20.0;
					nuscene:=6;
					xme:=xme-step;

			
			--(-10,+9) -> #6 @(+10,-4)
			elsif atThreshold(currenttime,"5y",5) then --5y
					xme:=xme+20.0;
					zme:=zme-13.0;
					nuscene:=6;
					xme:=xme-step;

			
			--(-10,+4) -> #6 @(+10,-9)
			elsif atThreshold(currenttime,"5z",5) then --5z
					xme:=xme+20.0;
					zme:=zme-13.0;
					nuscene:=6;
					xme:=xme-step;



			elsif
				atThreshold(currenttime, "5ex", 5)
			then
				nuscene:=1;
				xme:=xmaze; -- new single doorway
				zme:=zmaze-dmaze-0.01;
				yme:=aheight+land_alt(xme,zme);
			end if;







		--maze part2
		elsif scene=6 then --connected with itself & scene #5, #4=temple

			-- in case temple4 is visible:
			zme:=zme-20.0; --temple coords
			zcam:=zcam-20.0; --temple coords
			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_temple; --scene #4
			zme:=zme+20.0; --restore to present value
			zcam:=zcam+20.0; --restore to present value




			eawe:=0;
			if xme>0.0 and zme<=-3.0 then --6yz

				if zme>=-6.0 then eawe:=6; --5Y
				else eawe:=7; end if; --5Z

				xme:=xme-20.0;
				xcam:=xcam-20.0;
				zme:=zme+13.0;
				zcam:=zcam+13.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze5;
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore
				zme:=zme-13.0;
				zcam:=zcam-13.0; --restore

				if zme>=-6.0 then eawe:=6; --6Y
				else eawe:=7; end if; --6Z

			elsif xme>0.0 and zme<=3.0 then --6vwx

				if zme>=1.0 then eawe:=8; --5V
				elsif zme>=-1.0 then eawe:=9; --5W
				else eawe:=10; end if; --5X

				xme:=xme-20.0;
				xcam:=xcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze5;
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore

				if zme>=1.0 then eawe:=3; --6V
				elsif zme>=-1.0 then eawe:=4; --6W
				else eawe:=5; end if; --6X

			elsif xme<0.0 and zme>=-3.0 then --6cdefg

				if    zme>=6.0 then eawe:=1; --5C
				elsif zme>=3.0 then eawe:=2; --5D
				elsif zme>=1.0 then eawe:=3; --5E
				elsif zme>=-1.0 then eawe:=4; --5F
				else eawe:=5; end if;--5G

				xme:=xme+20.0;
				xcam:=xcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze5;
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore

				if    zme>=6.0 then eawe:=8; --6C
				elsif zme>=3.0 then eawe:=9; --6D
				elsif zme>=1.0 then eawe:=10; --6E
				elsif zme>=-1.0 then eawe:=11; --6F
				else eawe:=12; end if;--6G

			elsif xme<0.0 and zme<=-3.0 then --6ab right

				if zme>-7.0 then eawe:=1; --Aleft
				else eawe:=2; end if; --Bleft

				xme:=xme+20.0;
				xcam:=xcam+20.0;
				zme:=zme+13.0;
				zcam:=zcam+13.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze6;
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore
				zme:=zme-13.0;
				zcam:=zcam-13.0; --restore

				if zme>-7.0 then eawe:=13; --Aright
				else eawe:=14; end if; --Bright


			elsif xme>0.0 and zme>=3.0 then --6ab left

				if zme>7.0 then eawe:=13; --Aright
				else eawe:=14; end if; --Bright

				xme:=xme-20.0;
				xcam:=xcam-20.0;
				zme:=zme-13.0;
				zcam:=zcam-13.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze6;
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore
				zme:=zme+13.0;
				zcam:=zcam+13.0; --restore

				if zme>7.0 then eawe:=1; --Aleft
				else eawe:=2; end if; --Bleft

			end if;



			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_maze6; --main view


		

-- now, check for imminent transitions:


			if atThreshold(currenttime,"6z",6) then --6z

			--(+10,-9) -> #5 @(-10,+4)
				xme:=xme-20.0;
				zme:=zme+13.0;
				nuscene:=5;
				xme:=xme+step;

			elsif atThreshold(currenttime,"6y",6) then --6y

			--(+10,-4) -> #5 @(-10,+9)
				xme:=xme-20.0;
				zme:=zme+13.0;
				nuscene:=5;
				xme:=xme+step;

			elsif atThreshold(currenttime,"6x",6) then --6x

			--(+10,-2) -> #5 @(-10,-2)
				xme:=xme-20.0;
				nuscene:=5;
				xme:=xme-step;

			elsif atThreshold(currenttime,"6w",6) then --6w

			--(+10,+0) -> #5 @(-10,+0)
				xme:=xme-20.0;
				nuscene:=5;
				xme:=xme-step;

			elsif atThreshold(currenttime,"6v",6) then --6v

			--(+10,+2) -> #5 @(-10,+2)
				xme:=xme-20.0;
				nuscene:=5;
				xme:=xme-step;

			elsif atThreshold(currenttime,"6g",6) then --6g

			--(-10,-2) -> #5 @(+10,-2)
				xme:=xme+20.0;
				nuscene:=5;
				xme:=xme+step;

			elsif atThreshold(currenttime,"6f",6) then --6f

			--(-10,0)  -> #5 @(+10,+0)
				xme:=xme+20.0;
				nuscene:=5;
				xme:=xme+margin;

			elsif atThreshold(currenttime,"6e",6) then --6e

			--(-10,+2) -> #5 @(+10,+2)
				xme:=xme+20.0;
				nuscene:=5;
				xme:=xme+step;

			elsif atThreshold(currenttime,"6d",6) then --6d

			--(-10,+4) -> #5 @(+10,+4)
				xme:=xme+20.0;
				nuscene:=5;
				xme:=xme-step;

			elsif atThreshold(currenttime,"6c",6) then --6c

			--(-10,+9) -> #5 @(+10,+9)
				xme:=xme+20.0;
				nuscene:=5;
				xme:=xme-step;

			elsif atThreshold(currenttime,"6br",6) then --6br

			--(-10,-9) -> (+10,+4)
				xme:=xme+20.0;
				zme:=zme+13.0;
				nuscene:=6;
				xme:=xme-step;
				reenter:=true;

			elsif atThreshold(currenttime,"6bl",6) then --6bl

			--(+10,+4) -> (-10,-9)
				xme:=xme-20.0;
				zme:=zme-13.0;
				nuscene:=6;
				xme:=xme+step;
				reenter:=true;

			elsif atThreshold(currenttime,"6ar",6) then --6ar

			--(-10,-4)
				xme:=xme+20.0;
				zme:=zme+13.0;
				nuscene:=6;
				xme:=xme-step;
				reenter:=true;

			elsif atThreshold(currenttime,"6al",6) then --6al

			--(+10,+9)
				xme:=xme-20.0;
				zme:=zme-13.0;
				nuscene:=6;
				xme:=xme+step;
				reenter:=true;



			elsif 
				atThreshold(currenttime, "6ex", 6) --6ex(4e)
				and lionopen and not lionwait
			then
				nuscene:=4;
				xme:=ixtmpl; -- 0
				zme:=iztmpl+dtmpl+0.01; --  ~= -10
				yme:=-iymax+aheight;


			end if;





		elsif scene=7 then --connected with itself, scene #1, #8=labyrinth

			-- note that drawing exterior would be problematic...
			-- (ground level changes)


			eawe:=0;
			if zme<-3.0 and xme<0.0 then --7abcLeft

				if zme<-8.0 then eawe:=7; --7aR
				elsif zme<-5.5 then eawe:=8; --7bR
				else eawe:=9; end if; --7cR

				zme:=zme+7.0;
				zcam:=zcam+7.0;
				xme:=xme+20.0;
				xcam:=xcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze7;
				zme:=zme-7.0;
				zcam:=zcam-7.0; --restore
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore

				if zme<-8.0 then eawe:=1; --7aL
				elsif zme<-5.5 then eawe:=2; --7bL
				else eawe:=3; end if; --7cL


			elsif zme>-3.0 and xme<0.0 then --7defLeft (-Z)

				if zme<-1.0 then eawe:=10; --7dR
				elsif zme<+1.0 then eawe:=11; --7eR (+Z)
				else eawe:=12; end if; --7fR

				zme:=zme-7.0;
				zcam:=zcam-7.0;
				xme:=xme+20.0;
				xcam:=xcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze7;
				zme:=zme+7.0;
				zcam:=zcam+7.0; --restore
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore

				if zme<-1.0 then eawe:=4; --7dL (-Z)
				elsif zme<+1.0 then eawe:=5; --7eL
				else eawe:=6; end if; --7fL


			elsif zme>-3.0 and xme>0.0 then --7abcRight (+Z)

				if zme<-1.0 then eawe:=1; --7aL
				elsif zme<+1.0 then eawe:=2; --7bL
				elsif zme<+5.0 then eawe:=3; --7cL
				else eawe:=2; --8hL
				end if;

				if zme<5.0 then
					zme:=zme-7.0;
					zcam:=zcam-7.0;
					xme:=xme-20.0;
					xcam:=xcam-20.0;
					updateMVPs( currentTime, float(winwidth), float(winheight) );
					draw_maze7;
					zme:=zme+7.0;
					zcam:=zcam+7.0; --restore
					xme:=xme+20.0;
					xcam:=xcam+20.0; --restore

				else

					xme:=xme-20.0;
					xcam:=xcam-20.0;
					updateMVPs( currentTime, float(winwidth), float(winheight) );
					draw_lab8;
					xme:=xme+20.0;
					xcam:=xcam+20.0; --restore
				end if;

				if zme<-1.0 then eawe:=7; --7aR
				elsif zme<+1.0 then eawe:=8; --7bR
				elsif zme<+5.0 then eawe:=9; --7cR
				else eawe:=13; end if; --7hR


			elsif zme<-3.0 and xme>0.0 then --7defRight

				if zme<-8.0 then eawe:=4; --7dL
				elsif zme<-5.5 then eawe:=5; --7eL
				else eawe:=6; end if; --7fL

				zme:=zme+7.0;
				zcam:=zcam+7.0;
				xme:=xme-20.0;
				xcam:=xcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze7;
				zme:=zme-7.0;
				zcam:=zcam-7.0; --restore
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore

				if zme<-8.0 then eawe:=10; --7dR
				elsif zme<-5.5 then eawe:=11; --7eR
				else eawe:=12; end if; --7fR


			end if;


			--restore
			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_maze7;

-- 1st, check for imminent transitions within scene7:

			if atThreshold(currenttime,"7ar",7) then --7ar
				xme:=-10.0+step;
				zme:=-9.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7al",7) then --7al
				xme:=+10.0-step;
				zme:=-2.0;
				nuscene:=7;
				reenter:=true;


			elsif atThreshold(currenttime,"7br",7) then --7br
				xme:=-10.0+step;
				zme:=-7.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7bl",7) then --7bl
				xme:=+10.0-step;
				zme:=0.0;
				nuscene:=7;
				reenter:=true;



			elsif atThreshold(currenttime,"7cr",7) then --7cr
				xme:=-10.0+step;
				zme:=-4.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7cl",7) then --7cl
				xme:=+10.0-step;
				zme:=+3.0;
				nuscene:=7;
				reenter:=true;





			elsif atThreshold(currenttime,"7dr",7) then --7dr
				xme:=-10.0+step;
				zme:=-2.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7dl",7) then --7dl
				xme:=+10.0-step;
				zme:=-9.0;
				nuscene:=7;
				reenter:=true;




			elsif atThreshold(currenttime,"7er",7) then --7er
				xme:=-10.0+step;
				zme:=-0.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7el",7) then --7el
				xme:=+10.0-step;
				zme:=-7.0;
				nuscene:=7;
				reenter:=true;




			elsif atThreshold(currenttime,"7fr",7) then --7fr
				xme:=-10.0+step;
				zme:=+3.0;
				nuscene:=7;
				reenter:=true;

			elsif atThreshold(currenttime,"7fl",7) then --7fl
				xme:=+10.0-step;
				zme:=-4.0;
				nuscene:=7;
				reenter:=true;



-- check for scene8 transition:
			elsif atThreshold(currenttime,"7h",7) then --7h
				xme:=-10.0+step;
				nuscene:=8;



-- check for scene1 transition:
			elsif
				atThreshold(currenttime, "7ex", 7) --7ex(1e)
			then
				nuscene:=1;
				xme:=xmaze; -- new single doorway
				zme:=zmaze-dmaze-0.01;
				yme:=aheight+land_alt(xme,zme);
			end if;




		elsif scene=8 then --connected to scene7, scene9

			noso:=0;
			eawe:=0;

			if xme<0.0 and zme>6.0 then --draw 7
				eawe:=13; --opening # within M7
				xme:=xme+20.0; xcam:=xcam+20.0;

				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze7;

				xme:=xme-20.0; xcam:=xcam-20.0;
				eawe:=2; --opening # within M8

			elsif xme>0.0 and zme>4.0 then --draw9

				eawe:=13; --opening # within M9
				xme:=xme-20.0; xcam:=xcam-20.0;

				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;

				xme:=xme+20.0; xcam:=xcam+20.0;
				eawe:=1; --opening # within M8


			end if;

			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_lab8;

-- check for scene9 transition:
			if atThreshold(currenttime,"8g",8) then --8g
				xme:=xme-20.0;
				xme:=xme+step;
				nuscene:=9;

-- check for scene7 transition:
			elsif atThreshold(currenttime,"8h",8) then --8h
				xme:=+10.0-step;
				nuscene:=7;




			end if;

------------- addendum 29oct16 end ----------------------------------------

		elsif scene=9 then

			noso:=0; eawe:=0;
			if xme<0.0 then --9deBot

				if zme<-5.0 then noso:=9; --9dT
				elsif zme<0.0 then noso:=10; --9eT
				--else noso:=1; end if; --lab8exit2M9 FAIL
				else eawe:=1; end if; --lab8exit2M9 (may not matter which)

				xme:=xme+20.0;
				xcam:=xcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				if zme>0.0 then
				draw_lab8;
				else
				draw_maze9;
				end if;
				xme:=xme-20.0;
				xcam:=xcam-20.0; --restore

				if zme<-5.0 then noso:=11; --9dB
				elsif zme<0.0 then noso:=12; --9eB
				else noso:=13; end if; --exit2lab8


			elsif xme>0.0 then --9deTop

				if zme<-5.0 then noso:=11; --9dB
				else noso:=12; end if; --9eB

				xme:=xme-20.0;
				xcam:=xcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;
				xme:=xme+20.0;
				xcam:=xcam+20.0; --restore

				if zme<-5.0 then noso:=9; --9dT
				else noso:=10; end if; --9eT

			end if;


			eawe:=0;
			if xme>0.0 and zme<0.0 then --9fbaLeft

				if xme>6.0 then eawe:=8; --9fR
				elsif xme>3.5 then eawe:=6; --9bR
				else eawe:=5; end if; --9aR

				xme:=xme-10.0;
				xcam:=xcam-10.0;
				zme:=zme+20.0;
				zcam:=zcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;
				xme:=xme+10.0;
				xcam:=xcam+10.0; --restore
				zme:=zme-20.0;
				zcam:=zcam-20.0; --restore

				if xme>6.0 then eawe:=4; --9fL
				elsif xme>3.5 then eawe:=2; --9bL
				else eawe:=1; end if; --9aL


			elsif xme<0.0 and zme<0.0 then --9cLeft

				eawe:=7; --9cR

				xme:=xme+10.0;
				xcam:=xcam+10.0;
				zme:=zme+20.0;
				zcam:=zcam+20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;
				xme:=xme-10.0;
				xcam:=xcam-10.0; --restore
				zme:=zme-20.0;
				zcam:=zcam-20.0; --restore

				eawe:=3; --9cL

			elsif xme>+4.0 and zme>0.0 then --9cRight

				eawe:=3; --9cL

				xme:=xme-10.0;
				xcam:=xcam-10.0;
				zme:=zme-20.0;
				zcam:=zcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;
				xme:=xme+10.0;
				xcam:=xcam+10.0; --restore
				zme:=zme+20.0;
				zcam:=zcam+20.0; --restore

				eawe:=7; --9cR

			elsif xme<+4.0 and zme>0.0 then --9fbaRight

				if xme>-4.0 then eawe:=4; --9fL
				elsif xme>-7.0 then eawe:=2; --9bL
				else eawe:=1; end if; --9aL

				xme:=xme+10.0;
				xcam:=xcam+10.0;
				zme:=zme-20.0;
				zcam:=zcam-20.0;
				updateMVPs( currentTime, float(winwidth), float(winheight) );
				draw_maze9;
				xme:=xme-10.0;
				xcam:=xcam-10.0; --restore
				zme:=zme+20.0;
				zcam:=zcam+20.0; --restore

				if xme>-4.0 then eawe:=8; --9fR
				elsif xme>-7.0 then eawe:=6; --9bR
				else eawe:=5; end if; --9aR

			end if;



			--restore to normal & draw
			updateMVPs( currentTime, float(winwidth), float(winheight) );
			draw_maze9;


-- check for scene8 transition:
			if atThreshold(currenttime,"9x",9) then --98=9x
				xme:=xme+20.0;
				xme:=xme-step;
				nuscene:=8;
				-- shall be doing an about-face!


			--(+1,-10) -> (-9,+10)
			elsif atThreshold(currenttime,"9al",9) then --9al

					xme:=xme-10.0;
					zme:=zme+20.0;
					nuscene:=9;
					zme:=zme-step;
					reenter:=true;

			--(-9,+10) -> (+1,-10)
			elsif atThreshold(currenttime,"9ar",9) then --9ar

					xme:=xme+10.0;
					zme:=zme-20.0;
					nuscene:=9;
					zme:=zme+step;
					reenter:=true;


			--(+5,-10) -> (-5,+10)
			elsif atThreshold(currenttime,"9bl",9) then --9bl

					xme:=xme-10.0;
					zme:=zme+20.0;
					nuscene:=9;
					zme:=zme-step;
					reenter:=true;

			--(-5,+10) -> (+5,-10)
			elsif atThreshold(currenttime,"9br",9) then --9br

					xme:=xme+10.0;
					zme:=zme-20.0;
					nuscene:=9;
					zme:=zme+step;
					reenter:=true;



			--(-4,-10) -> (+6,+10)
			elsif atThreshold(currenttime,"9cl",9) then --9cl

					xme:=xme+10.0;
					zme:=zme+20.0;
					nuscene:=9;
					zme:=zme-step;
					reenter:=true;

			--(+6,+10) -> (-4,-10)
			elsif atThreshold(currenttime,"9cr",9) then --9cr

					xme:=xme-10.0;
					zme:=zme-20.0;
					nuscene:=9;
					zme:=zme+step;
					reenter:=true;




			--(+8,-10) -> (-2,+10)
			elsif atThreshold(currenttime,"9fl",9) then --9fl

					xme:=xme-10.0;
					zme:=zme+20.0;
					nuscene:=9;
					zme:=zme-step;
					reenter:=true;

			--(-2,+10) -> (+8,-10)
			elsif atThreshold(currenttime,"9fr",9) then --9fr

					xme:=xme+10.0;
					zme:=zme-20.0;
					nuscene:=9;
					zme:=zme+step;
					reenter:=true;

-----------------------------------------------------



			--(+10,-8) -> (-10,-8)
			elsif atThreshold(currenttime,"9dt",9) then --9dt

					xme:=xme-20.0;
					nuscene:=9;
					xme:=xme+step;
					reenter:=true;

			--(-10,-8) -> (+10,-8)
			elsif atThreshold(currenttime,"9db",9) then --9db

					xme:=xme+20.0;
					nuscene:=9;
					xme:=xme-step;
					reenter:=true;




			--(+10,-1) -> (-10,-1)
			elsif atThreshold(currenttime,"9et",9) then --9et

					xme:=xme-20.0;
					nuscene:=9;
					xme:=xme+step;
					reenter:=true;

			--(-10,-1) -> (+10,-1)
			elsif atThreshold(currenttime,"9eb",9) then --9eb

					xme:=xme+20.0;
					nuscene:=9;
					xme:=xme-step;
					reenter:=true;



			end if;



		end if; -- scene=9




-------------------------------------------------------------------------
--bat handler logic moved to draw_exterior & draw_maze5



------------- begin draw dragons ----------------------------------------
-- Dragon-Handler logic:

		if bdragonfly and not bdragondead and (scene=1 or scene=2) then --draw dragon
			dt:=(currenttime-dragonstart)/dragonduration; -- 0..1
			if dt>=1.0 then
				bdragonfly:=false;
				bsdra:=scene;
				bxdra:=xme;
				bzdra:=zme;
				if interior then --scene=2
					bydra:=2.0*htobj;
				else
					bydra:=2.0*htobj+land_alt(bxdra,bzdra);
				end if;
				pictobj.setrect( 
					bdragon, 
					bxdra,bydra,bzdra,
					0.6, 0.0, 0.6, --xr,yr,zr
					j1,j2,j3,j4,j5,j6);

				snd4ada.playSnd(roar); --Roar (in either case) 11sep16
				delay 0.8; -- give time to play roar

				if swordheld then --dragon dies
					bdragondead:=true;
					snd4ada.playSnd(die); -- dragondie.wav
					delay 0.8;
				else --dragon eats you...
					snd4ada.playSnd(eat); -- eaten.wav
					delay 1.0;
					userexit:=true;
					imdead_dragon:=true;
				end if;
				
			else
				drawbdragon(dt);
			end if;
		end if; --bdragonfly





		-- draw RED dragon
		if rdragonfly and not rdragondead 
		and (scene=5 or scene=1 or scene=6 or scene=7) then
			dt:=(currenttime-dragonstart)/dragonduration; -- 0..1

			if dt>=2.0 and runAway then
				rdragonfly:=false;
				runAway:=false;
				roarbegun:=false;

			elsif dt>=1.0 and runAway then
				if swordheld then
					drawrdragon(2.0-dt); --retreat

					-- would prefer a diminshing retreat roar
					if not roarbegun then
						snd4ada.playSnd(roar); --Roar but retreat
						roarbegun:=true;
					end if;
					--delay 0.8; -- give time to play roar


				else --dragon eats you...
					rdragonfly:=false;

					snd4ada.playSnd(roar); --Roar
					delay 0.8; -- give time to play roar

					snd4ada.playSnd(eat); -- eaten.wav
					delay 1.0;
					userexit:=true;
					imdead_dragon:=true;
				end if;

			elsif dt>=1.0 then
				rdragonfly:=false;
				rsdra:=scene;
				rxdra:=xme;
				rzdra:=zme;
				if interior then
					rydra:=-iymax+2.0*htobj;
				else
					rydra:=land_alt(rxdra,rzdra)+2.0*htobj;
				end if;
				pictobj.setrect( 
					rdragon, 
					rxdra,rydra,rzdra,
					0.6, 0.0, 0.6, --xr,yr,zr
					j1,j2,j3,j4,j5,j6);

				snd4ada.playSnd(roar); --Roar (in either case) 11sep16
				delay 0.8; -- give time to play roar

				if swordheld then --dragon dies
					rdragondead:=true;
					snd4ada.playSnd(die); -- dragondie.wav
					delay 0.8;
				else --dragon eats you...
					snd4ada.playSnd(eat); -- eaten.wav
					delay 1.0;
					userexit:=true;
					imdead_dragon:=true;
				end if;
				
			else -- 0<dt<1
				drawrdragon(dt);
			end if;
		end if; --rdragonfly




		if scene=rsdra and rdragondead then

			if not interior then
				glUseProgram(pidterra02);
				glUniformMatrix4fv(mvpid02, 1, GL_FALSE, imvp(1,1)'address);

				if thirdperson then
				glUniform3f(eyeid02,glfloat(xcam),glfloat(ycam),glfloat(zcam));
				else
				glUniform3f(eyeid02,glfloat(xme),glfloat(yme),glfloat(zme));
				end if;

				glUniform1i(flevid02, 0); --extfoglev);
				glUniform1i(sampid02, 0);
				glUniform1i(darkid02, 0); --extdarkness);

				glbindtexture(gl_texture_2d, deadrdragon_texid);
				pictobj.draw(rdragon, vertbuff,uvbuff,elembuff);
			else
				glUseProgram( pidtex05 );
				gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );
				gluniform1i(sampid05,0);
				glUniform1i(darkid05,0); -- darkness);
				gluniform1i(flevid05, 0 ); 
				-- 1=>normal, 2=>heavy, 3=>extreme fog
				gluniform1i(fcolid05, 1 ); 
				-- 1=>gray, 2=>brown, 3=>purple fog

				glbindtexture(gl_texture_2d, deadrdragon_texid);
				pictobj.draw(rdragon, vertbuff,uvbuff,elembuff);
			end if;

		end if;

--Always draw dragons with no fog ========================================


		if scene=bsdra and bdragondead then

			if not interior then

				glUseProgram(pidterra02);

				if thirdperson then
				glUniform3f(eyeid02,glfloat(xcam),glfloat(ycam),glfloat(zcam));
				else
				glUniform3f(eyeid02,glfloat(xme),glfloat(yme),glfloat(zme));
				end if;


				glUniform1i(flevid02, 0); --extfoglev);
				glUniformMatrix4fv(mvpid02, 1, GL_FALSE, imvp(1,1)'address);
				glUniform1i(sampid02, 0);
				glUniform1i(darkid02,0); -- darkness);

				glbindtexture(gl_texture_2d, deadbdragon_texid);
				pictobj.draw(bdragon, vertbuff,uvbuff,elembuff);
			else

				glUseProgram( pidtex05 );
				gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );
				gluniform1i(sampid05,0);
				glUniform1i(darkid05,0); -- darkness);
				gluniform1i(flevid05, 0 ); 
				-- 1=>normal, 2=>heavy, 3=>extreme fog
				gluniform1i(fcolid05, 1 ); 
				-- 1=>gray, 2=>brown, 3=>purple fog

				glbindtexture(gl_texture_2d, deadbdragon_texid);
				pictobj.draw(bdragon, vertbuff,uvbuff,elembuff);
			end if;

		end if;


------------- end draw dragons ----------------------------------------




----------------------- avatar begin ------------------------------------

		if thirdPerson then 
			currentTime := glfwGetTime;
			-- moved here 3apr17 to assure fresh, accurate
			-- (xme,yme,zme) & (xcam,ycam,zcam) positions
			-- and perhaps avoid avatar jitter.
			drawAvatar(currenttime);
		end if;


------------------ bug warning ------------------------------------------

		if warn_bug  and not imdead_bug then
			--ftex.print2d("Get Away! Beetles are hungry!",0.1,0.75,1.5,fontcol);
			ftex.print2d("Get Away! Scarabs are hungry!",0.1,0.75,1.5,fontcol);
		end if;


---------------- toxicFog handler ----------------------------------

		if nearfog then
			if not toxicplaying then
				toxicplaying:=true;
				snd4ada.playLoop(toxic);
			end if;
		elsif toxicplaying then
			toxicplaying:=false;
			snd4ada.stopLoop(toxic);
		end if;

------------------ waterfall handler -------------------------------

		if nearwaterfall then
			if not wfplaying then
				wfplaying:=true;
				snd4ada.playLoop(smallwf);
			end if;
		elsif wfplaying then
			wfplaying:=false;
			snd4ada.stopLoop(smallwf);
		end if;


-----------------------snake handler logic begin-------------------------

		if snakehiss then
			if not hissbeat then
				hissbeat:=true;
				snd4ada.playLoop(hiss);
			end if;
		elsif hissbeat then
			hissbeat:=false;
			snd4ada.stopLoop(hiss);
		end if;



		if nearsnake then
			ftex.print2d("TOO CLOSE...",
				0.3,0.80, 0.8, fontcol );
			ftex.print2d("that mamba is deadly.",
				0.2,0.70, 0.8, fontcol );

			if warning1 and ((currenttime-warningtime)>warningpause) then

				-- here we should roll the dice...even if you are holding
				-- a sword, 1 out of 3 times you still die...

				if not swordheld then -- 20nov17 addendum
					userexit:=true;
					imdead_snake:=true;

				elsif snakeVSsword<2 then
					-- we COULD kill the snake here
					-- but it's more fun if we keep it...
					ftex.print2d("your SWORD keeps him at bay!",
						0.2,0.60, 0.7, fontcol );
					snakeVSsword:=snakeVSsword+1;
					warningtime := currenttime;

				else -- you die anyway
					userexit:=true;
					imdead_snake:=true;

				end if;

				--if imdead_snake then --15dec17
				--	snd4ada.playSnd(womanscream); -- bitten!!!
				--	delay 3.0;
				--end if; now do this down further

			else
				warning1:=true;
				warningtime := currenttime;
			end if;
		end if;


----------- screen-text begin -----------------------------------------------------------------

		if intro then
			sz:=1.0;
			if chapter=1 or chapter=3 then
				ftex.print2d("AdaVenture is a point & click quest set in", 0.02,0.90, sz,fontcol);
				ftex.print2d("ancient Persia.  The golden chalice of Xerxes",0.02,0.85, sz,fontcol);
				ftex.print2d("has been stolen by Leonidas the Spartan King.",0.02,0.80, sz,fontcol);
				ftex.print2d("Find & return the golden chalice",0.02,0.75, sz,fontcol);
				ftex.print2d("to its pedestal within the castle.",0.02,0.70, sz,fontcol);
				ftex.print2d("Use the sword to kill deadly beasts,",0.02,0.65, sz,fontcol);
				ftex.print2d("click to grab/drop objects as needed.",0.02,0.60, sz,fontcol);
				ftex.print2d("The [i] or [h] keys toggle this Intro/Help",0.02,0.55, sz,fontcol);
				ftex.print2d("The [m]-key toggles the Avatar",0.02,0.50, sz,fontcol);

				ftex.print2d("The [l]-key toggles LazyCamera: default.",0.02,0.45, sz,fontcol);
				ftex.print2d("The [n/f/z]-keys Zoom avatar Near/Far/default",0.02,0.40, sz,fontcol);
				ftex.print2d("Press the [esc]-key to Quit",0.02,0.25, sz,fontcol);

				ftex.print2d("Press the [v]-key to saVe game",0.02,0.20, sz,fontcol);


			else
				ftex.print2d("AdaVenture is a point & click quest set in",0.02,0.90, sz,fontcol);
				ftex.print2d("ancient Persia.  The golden chalice of Xerxes",0.02,0.85, sz,fontcol);
				ftex.print2d("has been stolen by Minos the Cretan King.",0.02,0.80, sz,fontcol);
				ftex.print2d("Find & return the golden chalice",0.02,0.75, sz,fontcol);
				ftex.print2d("to its pedestal within the castle.",0.02,0.70, sz,fontcol);
				ftex.print2d("Use the sword to kill deadly beasts,",0.02,0.65, sz,fontcol);
				ftex.print2d("click to grab/drop objects as needed.",0.02,0.60, sz,fontcol);
				ftex.print2d("The [i]-key toggles this Intro/Help",0.02,0.55, sz,fontcol);
				ftex.print2d("The [m]-key toggles the Avatar",0.02,0.50, sz,fontcol);

				ftex.print2d("The [l]-key toggles LazyCamera: default.",0.02,0.45, sz,fontcol);
				ftex.print2d("The [n/f/z]-keys Zoom avatar Near/Far/default",0.02,0.40, sz,fontcol);
				ftex.print2d("Press the [esc]-key to Quit",0.02,0.25, sz,fontcol);

				ftex.print2d("Press the [v]-key to saVe game",0.02,0.20, sz,fontcol);

			end if;
		end if; --intro

		if success and heralded then

			if (currentTime-winnertime<15.0) then --show for 15 seconds

				sz:=1.2;
				ftex.print2d("You have restored the Golden Chalice",0.02,0.90,      sz,fontcol);
				ftex.print2d("of Xerxes.  Praise be upon you.",0.02,0.80,           sz,fontcol);
				ftex.print2d("Now, we shall celebrate into the night...",0.02,0.70, sz,fontcol);
				ftex.print2d("...or until you hit the [esc]-key.",0.02,0.60,        sz,fontcol);

			elsif chapter=1  then

				sz:=0.8;
				ftex.print2d(
				"Korla Pandit playing Misrlu...a song so old " 
				&"that it was known even to the ancient Persians!",
				0.02,0.05, sz,fontcol);

			elsif chapter=2 then

				sz:=0.8;
				ftex.print2d(
				"Korla Pandit playing Turkish Dance",
				0.02,0.05,  sz,fontcol);

			elsif chapter=3 then

				sz:=0.8;
				ftex.print2d(
				"Kevin Macleod playing AngevinB",
				0.02,0.05, sz,fontcol);

			elsif chapter=4 then

				sz:=0.8;
				ftex.print2d(
				"Kevin Macleod playing Ibn-Al-Noor",
				0.02,0.05,  sz,fontcol);

			end if;

		end if;

		--user feedback messages to screen:
		if (currentTime-savetime<3.0) then --show for 3 seconds
			sz:=1.2;
			ftex.print2d("Game State was Saved.",0.02,0.90,      sz,fontcol);

		elsif (currentTime-lazytime)<3.0 then
			sz:=1.2;
			if lazyCam then
				ftex.print2d("Lazy Camera.",0.02,0.90,   sz,fontcol);
			else
				ftex.print2d("Tight Camera.",0.02,0.90,  sz,fontcol);
			end if;

		end if;


--------------------------------------------------------------------------
		if not bugstarted then
			glflush;
			glfwSwapBuffers( mainWindow );
		end if;
		--otherwise, postpone till AFTER drawing screenBugs
--------------------------------------------------------------------------


		if success and not heralded then
			winnertime:=currentTime;
			heralded:=true;
			delay 0.3; --allow putdown sound to complete

			snd4ada.playSnd(won); --adventure-win-sound
			delay 5.0; --allow win-sound to complete

			if chapter=1 then
				snd4ada.playLoop(misr); --Misrlu;
			elsif chapter=2 then
				snd4ada.playLoop(turk); --TurkishDance;
			elsif chapter=3 then
				snd4ada.playLoop(angv); --AngevinB;
			elsif chapter=4 then
				snd4ada.playLoop(ibn); --IbnAlNoor;
			end if;
		end if;


		if imdead_bug then

			if not bugstarted then
				snd4ada.playSnd(womanscream); -- nibbled to death!!!
				bugstarted:=true;
				tstart:=currenttime;
			end if;

			-- bugstarted => 
			-- freeze game but allow redraws of screenBugs.

			--ftex.print2d("Beetles Ate You!", 0.1, 0.75, 1.5,fontcol);
			ftex.print2d("Scarabs Ate You!", 0.1, 0.75, 1.5,fontcol);

			screenBugs( float(currentTime) );

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			--delay exit so users reads message & sees screenbugs:
			if currentTime-tstart>4.0 then userexit:=true; end if;


elsif imdead_toxicfog then --9feb24
			userexit:=true;
			snd4ada.playSnd(gameover); -- asphyxiation
			ftex.print2d("ToxicFog Killed You!", 0.3, 0.75, 1.5,fontcol);

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			delay 4.0; --time to read above message


		elsif imdead_fireball then
			userexit:=true;
			snd4ada.playSnd(medusascream); -- charred & crushed!!!
			--delay 3.0;
			ftex.print2d("You were burnt to a crisp!", 0.1, 0.75, 1.5,fontcol);

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			delay 4.0; --time to read above message

		elsif imdead_minotaur then
			userexit:=true;
			snd4ada.playSnd(monsterscream); -- gored & trampled!
			ftex.print2d("The Minotaur Ate You!", 0.3, 0.75, 1.5,fontcol);

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			delay 4.0; --time to read above message

		elsif imdead_dragon then
			userexit:=true;
			ftex.print2d("The Dragon Ate You!", 0.3, 0.75, 1.5,fontcol);

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			delay 3.0; --time to read above message

		elsif imdead_snake then
			userexit:=true;
			snd4ada.playSnd(girlyscream); -- bitten!!!
			--delay 3.0;
			ftex.print2d("The Mamba Bit You!", 0.1, 0.75, 1.5,fontcol);

			---------------------------------------------------------
			glflush;
			glfwSwapBuffers( mainWindow );
			---------------------------------------------------------

			delay 3.0; --time to read above message

		end if;



		--output errors and raise exception here only if dbug
		if dbug then
			if dumpGLerrorQueue("AV main loop end")>0 then
				raise avent_main_error;
			end if;
		end if;




---------------------------------------------------------------------------
   end loop; ---------------------- main event loop end -------------------
---------------------------------------------------------------------------

	ftex.CloseFont;

	utex.cleanuptext;

	snd4ada.termSnds; -- stops any loops;  then deallocates

	release_textures;


	glfwdestroywindow(mainWindow);
	glfwTerminate;


-- addendum 22jun23:
exception
	when others =>

		-- The value is that this might
		-- help to reproduce some obscure error, as well as
		-- to enable a user to resume after an error.
		-- Note: this is NOT invoked when user dies!
		writeState(true);

		ftex.CloseFont;
		utex.cleanuptext;
		snd4ada.termSnds;
		release_textures;

		glfwdestroywindow(mainWindow);
		glfwTerminate;

end aventure;

end avent;

