

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


-- AdaVenture ... adventure Ada style
-- 


-------------------------------------------------------------------------------


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

----------------------------------------------------------------

with matutils;
with utex;

with ada.unchecked_conversion;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

----------------------------------------------------------------


with shader;  use shader;

with gametypes;  use gametypes;
with gameutils; use gameutils;


with text_io;
with pngloader;
with matutils;

with ada.calendar;
with ada.directories;

with ada.strings.fixed;


with pictobj;


with avent;

----------------------------------------------------------------

with cls_h;
with Text_IO;
with SysUtils;  use SysUtils;
with ada.directories;
with ada.environment_variables;
with Ada.command_line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with ada.characters.handling;
with gnat.os_lib;
with realtime;
with interfaces.c;
use type interfaces.c.int;
with interfaces.c.strings;
with GNATCOLL.Terminal;  use GNATCOLL.Terminal;





procedure adaventure is

	package myint_io is new text_io.integer_io(integer);

	use Ada.Strings.Unbounded;
	use Ada.Strings.Unbounded.Text_IO;


	use text_io;
	use pngloader;
	use matutils;

	use interfaces.c;
	use interfaces.c.strings;
	use glext;
	use glext.pointers;
	use glext.binding;
	use gl;
	use gl.binding;
	use gl.pointers;



	resfilname: unbounded_string;

	pgmtexshadid, pgmtexid : gluint := 0;

	uniftex, matid : glint;



	type vec3 is array(1..3) of float;


	function selectfile return unbounded_string is separate;


function mysqrt( arg: float ) return float is
begin
	if arg<0.0001 then
		return 0.0;
	else
		return fmath.sqrt(arg);
	end if;
end mysqrt;










Wwid,Whit, Nwid,Nhit, Fwid, Fhit : aliased interfaces.c.int;



vertbuff, uvbuff, elembuff, vertexarrayid : gluint;















procedure pre_prep is -- main program setup
      FileId : text_io.File_Type;
begin



	Wwid:=800;
	Whit:=800;

	gameutils.InitGLFW(Wwid, Whit, "AdaVenture -- click to select");




	utex.inittext2d("data/sansw.png", integer(Wwid),integer(Whit));


	glfwGetFramebufferSize(mainwindow, fwid'access, fhit'access);
	glViewport(0,0,fwid,fhit);



	glgenvertexarrays(1, vertexarrayid'address );
	glbindvertexarray(vertexarrayid);

	-- from the literature it seems I might not have to
	-- call this explicitly because the first texture
	-- unit is the active texture unit, by default.
	-- And I have no multi-texturing needs yet
	-- (like a tarnish on top of an existing texture)
	glactivetexture(gl_texture0);

	glgenbuffers(1, vertbuff'address);
	glgenbuffers(1, uvbuff'address);
	glgenbuffers(1, elembuff'address);




	glenable(gl_depth_test);
	gldepthfunc( gl_lequal );
	glenable( gl_cull_face );


	glEnable(GL_MULTISAMPLE);
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
	glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);

	glClearColor(0.5, 0.5, 0.5, 1.0);

end pre_prep;














function max( a, b : float ) return float is
begin
	if a>b then return a;
	else return b; end if;
end max;

MVP, ModelMatrix, ViewMatrix, ProjectionMatrix
	 : mat44 := identity;

procedure updatePreMVP( wid, hit : glint ) is
	xlook, ylook, zlook, xlk,ylk,zlk, xrt,yrt,zrt, xup,yup,zup : float;
	xme,yme,zme : float;

	woh : constant float := float(wid)/float(hit);
	how : constant float := float(hit)/float(wid);

	fovdeg : constant float := 45.0;
	fovrad : constant float := fovdeg*deg2rad;

	aspect : constant float := max(1.0,how);

	-- distance from eye so FOV encompasses proper field:
	eyeradius : constant float := aspect / fmath.tan(fovrad/2.0);

	near : constant float := 0.1;
	far  : constant float := 100.0;


-- New setup looks toward +Z direction (@ origin)
-- with +Y=up, +X=left:
	focus : constant vec3 := (0.0, 0.0, 0.0);
	eyepos: constant vec3 := (0.0, 0.0, -eyeradius);
	look  : constant vec3 := 
		( focus(1)-eyepos(1), focus(2)-eyepos(2), focus(3)-eyepos(3) );
	vertAng : constant float := 0.0;
	horiAng : constant float := fmath.arctan( look(1), look(3) ); --0.0;


begin

	ModelMatrix:=identity;
	--scale width versus height so pic fills window:
	if woh>1.0 then
		ModelMatrix(1,1):=woh;
	else
		ModelMatrix(3,3):=how;
	end if;

	xme:=eyepos(1);
	yme:=eyepos(2);
	zme:=eyepos(3);

	-- look direction:
	xlook := fmath.cos(vertang)*fmath.sin(horiang);
	ylook := fmath.sin(vertang);
	zlook := fmath.cos(vertang)*fmath.cos(horiang);

	xlk := xme+xlook;
	ylk := yme+ylook;
	zlk := zme+zlook;

	-- Right unit-Direction
	xrt:= fmath.sin(horiang-halfpi);
	yrt:= 0.0;
	zrt:= fmath.cos(horiang-halfpi);

	-- calculate UP unit-Direction
	cross( xrt,yrt,zrt, xlook,ylook,zlook, xup,yup,zup );

	perspective(ProjectionMatrix, fovdeg, woh,  near, far);

	lookat(ViewMatrix, xme,yme,zme, xlk,ylk,zlk, xup,yup,zup );

	MVP:=ModelMatrix;
	matXmat(MVP,ViewMatrix);
	matXmat(MVP,ProjectionMatrix);

end updatePreMVP;





npuz : constant integer := 4;
texid    : array(0..npuz+1) of GLuint;
puzpiece : array(0..npuz+1) of pictobj.pictangle; -- 0=>enclosure



procedure release_stuff is -- prepare to close down
begin

	glext.binding.glDeleteBuffers(1, vertbuff'address);
	glext.binding.glDeleteBuffers(1, uvbuff'address);
	glext.binding.glDeleteBuffers(1, elembuff'address);

	glext.binding.glDeleteProgram( pgmtexshadid );

	glext.binding.glDeleteVertexArrays(1, vertexarrayid'address);

end release_stuff;


procedure setup_stuff is  -- prepare dungeon textures
	xx,yy,zz,dx,dy,dz,nx,ny,nz : float;
	j1,j2,j3,j4,j5,j6: float;
begin 

	pgmtexshadid := 
		loadshaders("./data/texobjFog.vs", "./data/texobjShine.fs");
	matid := glgetuniformlocation
		(pgmtexshadid, new_string("MVP"&ascii.nul));
	uniftex  := glgetuniformlocation
		(pgmtexshadid, new_string("myTextureSampler"&ascii.nul));

	--either syntax is Ok:
	texid(0):=loadPng(clamp,"data/pool.png"); --bkgd
	texid(1):=loadPng(clamp,"data/one1.png");  --level1,ch1
	texid(2):=loadPng(clamp,"data/two2.png");  --level2,ch2
	texid(3):=loadPng(clamp,"data/three3.png");--level3,ch3
	texid(4):=loadPng(clamp,"data/four4.png"); --level4,ch4
	texid(5):=loadPng(clamp,"data/redoor.png"); --resume

-- orientation:  +Z look, +X=left, +Y=up
-- See "eyepos"
	myassert( npuz = 4, 998 );
	zz := -0.001;
	dz :=  0.001;
	nz := zz; -- slightly closer to eye than origin
	for row in 0..1 loop

		xx := 0.25;
		yy := 0.25 + float(1-row)/2.0; -- (+y is up)
		dx := 0.4 * 0.9;
		dy := 0.3 * 0.9;
		nx := 2.0*xx-1.0; --  0.0,  0.0
		ny := 2.0*yy-1.0; -- +0.5, -0.5

		--either syntax works:
		--puzpiece(row+1).setRect(
		pictobj.setRect( puzpiece(row+3),
			nx,ny,nz, 
			dx,dy,dz, 
			j1,j2,j3,j4,j5,j6);

		xx := 0.75;
		yy := 0.25 + float(1-row)/2.0; -- (+y is up)
		dx := 0.4 * 0.9;
		dy := 0.3 * 0.9;
		nx := 2.0*xx-1.0; --  0.0,  0.0
		ny := 2.0*yy-1.0; -- +0.5, -0.5

		--either syntax works:
		--puzpiece(row+1).setRect(
		pictobj.setRect( puzpiece(row+1),
			nx,ny,nz, 
			dx,dy,dz, 
			j1,j2,j3,j4,j5,j6);

	end loop;

	pictobj.setRect( puzpiece(5),
		0.0, 0.0, nz,
		0.8*dx,  0.8*dy,  dz, j1,j2,j3,j4,j5,j6);

	zz := 0.001;
	nz := zz; -- slightly further away from eye

	--puzpiece(0).setRect(
	pictobj.setRect( puzpiece(0),
		0.0,0.0,nz, 
		1.0,1.0,dz, 
		j1,j2,j3,j4,j5,j6);

end setup_stuff;






	resume: boolean := false;



--mousestate : Uint32;
mousex, mousey : aliased gldouble;
--state, ileft, iright : integer;
userpicked, userexit, details : boolean := false;
currenttime, keytime : gldouble;
dwell : constant float := 0.2;

ppreviousTime : gldouble := glfwGetTime;

--chapter, 30oct19
pselBlock : integer := -1;


procedure handle_mouse_pick( 
	width, height : glint;
	xmouse, ymouse : gldouble ) is

 -- these coords have origin @ lower left of window:
 col : constant float := float(xmouse)/float(width);
 row : constant float := float(ymouse)/float(height);

 ir,ic : integer := -1;

begin


	if( row < 0.5 ) then -- ch 1/3
		ir:=0;
	else                 -- ch 2/4
		ir:=1;
	end if;


	if( col < 0.5 ) then --light fog (ch1/2)
		ic:=0;
	else                 --heavy fog (ch3/4)
		ic:=1;
	end if;

	pselBlock := 2*ic+ir; -- Zero-based

	myassert( pselBlock >= 0, 997 );
	myassert( pselBlock < 4, 996 );

		-- chapter := pselBlock+1;

	if abs(row-0.5)<0.10 and abs(col-0.5)<0.10 then
		resume:=true;
	end if;

	userexit:=true;
	userpicked:=true;

end handle_mouse_pick;











function odd( i: integer ) return boolean is
begin
	return ( i mod 2 = 1 );
end odd;

function odd( i: gl.glint ) return boolean is
begin
	return ( i mod 2 = 1 );
end odd;






procedure getKeyInputs is

	procedure flushkey( scancode: int ) is
	begin
		while glfwgetkey( mainWindow, scancode ) /= Glfw_Release loop
			GlfwPollEvents;
		end loop;
	end flushkey;

begin

	if glfwgetkey( mainWindow, glfw_key_escape ) = Glfw_Press then
		userexit:=true;

	elsif glfwgetkey( mainWindow, glfw_key_q ) = Glfw_Press then
		userexit:=true;

	end if;

end getKeyInputs;





	pdlay : constant gldouble := 0.20; --mousePickDelay interval

	jump: integer := 0;


	HiRes, Ok: boolean := true; --false;

	 msl,msr: glint;

	len: natural;
	ch: character;
	slash: constant character := gnat.os_lib.directory_separator;

---------------------- main program begin -----------------------
begin -- adaventure

   
   if Ada.Command_Line.Argument_Count = 1 then

	-- jump = 0 => HiRes=false; NEW as of 3dec19

	-- jump in {3..10}:
	-- jump: 3=>castle, 4=>temple, 
	--			5=>maze5, 6=>maze6, 7=>maze7, 8=>lab8, 9=maze9
	--			any other number => normal start
	--
	-- add   10=>maze9/chapter4 ???
	--
	-- note that non-normal starts are NOT playable...
	--			they only allow observing effects of changes.
	--

     declare
       lst: natural;
       jstr : string := Ada.Command_Line.Argument(1);--level
		 ijump: integer;
     begin
      myint_io.get(jstr,ijump,lst);
		--jump:=ijump;
		if ijump=0 then
			HiRes:=false;
		elsif ijump>=3 and ijump<=10 then
			jump:=ijump;

			if jump<=6 then
				pselBlock:=0; --chapter 1
			else
				pselBlock:=1; --chapter 2
			end if;
			userexit:=true;
			userpicked:=true;

		elsif ijump>=31 then
			jump:=3;
			if ijump=31 then
				pselBlock:=0;
			elsif ijump=32 then
				pselBlock:=1;
			elsif ijump=33 then
				pselBlock:=2;
			elsif ijump=34 then
				pselBlock:=3;
			elsif ijump=51 then
				jump:=5;
				pselBlock:=1;
			elsif ijump=53 then
				jump:=5;
				pselBlock:=3;
			end if;

			userexit:=true;
			userpicked:=true;

		end if;

		exception
			when others =>
				put_line("...a single integer was expected...");
				raise;

     end; --declare

   end if;





	pre_prep; -- init graphics/sound, defines fnum, flev

	setup_stuff;



	currentTime := glfwGetTime;
	ppreviousTime := currentTime;
	keytime := currentTime;


	updatePreMVP( Wwid, Whit );



	-- main event loop begin: -----------------------------------------------
   while not userexit loop

		currentTime := glfwGetTime;
		glfwPollEvents;


		getKeyInputs;

		exit when userexit;


		exit when glfwWindowShouldClose(mainWindow) /= 0; 
		--15nov21 addendum




		glfwgetcursorpos(mainWindow,mousex'access,mousey'access);
		msr := glfwGetMouseButton(mainWindow, glfw_mouse_button_2); -- ?3?
		msl := glfwGetMouseButton(mainWindow, glfw_mouse_button_1);
		-- msl,msr: glint;

		if
			msl=glfw_press or msr=glfw_press
		then 
			if (currentTime - ppreviousTime) > pdlay then
				handle_mouse_pick(Wwid,Whit, mousex, mousey);
				ppreviousTime := currentTime;
			end if;
		end if;



-- might be preferable to maintain aspect ratio...
-------- here we might handle user-resized window ----------------------


		glfwGetWindowSize( mainWindow, Nwid'access, Nhit'access );
		if( (Nwid /= winwidth) or (Nhit /= winheight) ) then
			winwidth:=Nwid;  winheight:=Nhit;

			glfwGetFramebufferSize(mainwindow, fwid'access, fhit'access);
			glViewport(0,0,Fwid,Fhit);

			--stex.resize( winwidth, winheight );

		end if;


		updatePreMVP( Wwid, Whit );

		--------- begin drawing =============================

		glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);



		-- use this to draw ordinary textured objects:
		glUseProgram(pgmTexShadID);
		glUniformMatrix4fv(MatID, 1, GL_FALSE, MVP(1,1)'address);
		glUniform1i(uniftex, 0);


		for k in 0..npuz loop
			glBindTexture( GL_TEXTURE_2D, texid(k) );
			pictobj.draw(puzpiece(k),vertbuff,uvbuff,elembuff);
		end loop;

		glBindTexture( GL_TEXTURE_2D, texid(npuz+1) );
		pictobj.draw(puzpiece(npuz+1),vertbuff,uvbuff,elembuff);




		if details then -- toggled with <x>-key

			-- intent is to show technical details here so that I can track
			-- down the antialiasing problem under OS-X in case a MacBundle
			-- is used rather than the command line version.

			utex.print2d(" Ndim: " &
				interfaces.c.int'image(Nwid)&" X "
				& interfaces.c.int'image(Nhit), 0.05, 0.55, 25 );

			utex.print2d(" hdpi: " &
				interfaces.c.int'image(Fwid)&" X "
				& interfaces.c.int'image(Fhit), 0.05, 0.45, 25 );



--------- begin OGL queries -----------------------------------------

			glGetIntegerv(GL_CONTEXT_PROFILE_MASK, gametypes.profile'address);
			if( gametypes.profile = GL_CONTEXT_CORE_PROFILE_BIT ) then
				utex.print2d("ogl-query:  Core Profile", 0.02, 0.6, 10);
			end if;

			-- Note that OSX currently requires the forward_compatible flag!
			glGetIntegerv(GL_CONTEXT_FLAGS, gametypes.flags'address);
			if( gametypes.flags = GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT ) then
				utex.print2d("ogl-query:  Forward-Compatible bit is set", 0.02, 0.5, 10);
			end if;

			glgetintegerv(gl_major_version, gametypes.major'address);
			glgetintegerv(gl_minor_version, gametypes.minor'address);
			utex.print2d( "ogl-query: OGL-major: "&glint'image(gametypes.major), 0.02, 0.4, 10);
			utex.print2d( "ogl-query: OGL-minor: "&glint'image(gametypes.minor), 0.02, 0.3, 10);

			glgetintegerv(gl_max_texture_units, gametypes.mtu'address);
			utex.print2d( "ogl-query: maxTexUnits: "&glint'image(gametypes.mtu), 0.02, 0.2, 10);

			glgetintegerv(gl_max_texture_image_units, gametypes.mtu'address);
			utex.print2d( "ogl-query: maxTexImgUnits: "&glint'image(gametypes.mtu), 0.02, 0.13, 10);

			glgetintegerv(gl_max_combined_texture_image_units, gametypes.mtu'address);
			utex.print2d( "ogl-query: maxCombTexImgUnits: "&glint'image(gametypes.mtu), 0.02, 0.06, 10);


--------- end OGL queries -----------------------------------------




		end if;



		glflush;
		glfwSwapBuffers( mainWindow );


   end loop; ------------------- main event loop end -------------------



	release_stuff;
	utex.cleanuptext;

	--glfwIconifyWindow(mainWindow); --3oct23 for OSX
	glfwdestroywindow(mainWindow);
	glfwTerminate;



	if resume then
		oldstateexists:=true;

		resfilname := selectfile; --unbounded_string

		--need to know chapter...embedded into file name:
		if    index(resfilname,"_1.txt",1)>0 then
			pselBlock:=0;
		elsif index(resfilname,"_2.txt",1)>0 then
			pselBlock:=1;
		elsif index(resfilname,"_3.txt",1)>0 then
			pselBlock:=2;
		elsif index(resfilname,"_4.txt",1)>0 then
			pselBlock:=3;
		else
			raise program_error;
		end if;

if mswin then
	len:=length(resfilname);
	for i in 1..len loop
		ch:=element(resfilname,i);
		if ch='/' then
			replace_element(resfilname,i,'\');
		end if;
	end loop;
end if;

put("AdaVenture...Copying game file: ");
put(resfilname);
--put(" to data/gamestate.txt, level=");
put(" to data"&slash&"gamestate.txt, level=");
put_line( integer'image(pselBlock+1));

		--gameutils.readState( to_string(resfilname) );
		--pselBlock:=chapter-1;

		if mswin then
		SysUtils.bShell("copy "&to_string(resfilname)&" data\gamestate.txt", Ok);
		else
		SysUtils.bShell("cp "&to_string(resfilname)&" data/gamestate.txt", Ok);
		end if;

	end if;

	if userpicked then

		chapter:=pselBlock+1;

		if jump=10 then --debug M9+Fog
			chapter:=4; --Xtreme Fog
			jump:=9; -- maze 9
		end if;
		put_line("Selected Chapter "&integer'image(chapter));
		put_line("...jump= "&integer'image(jump));
		avent.aventure( chapter, jump, resume, HiRes );

	end if;

end adaventure;

