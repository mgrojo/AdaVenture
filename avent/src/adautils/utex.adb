
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


-- This package renders fonts from a homemade dictionary.
-- Its main use now is to draw special glyphs, other than
-- regular text, to the screen in order to cue the user
-- as to which item the avatar is carrying.




with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

-------------------------------------------------------------

with System;
with Interfaces.C;
use  type interfaces.c.unsigned;
with Interfaces.C.Pointers;
with interfaces.c.strings;

with ada.text_io; use ada.text_io;

with gameutils;
----------------------------------------------------------------

with pngloader;
use  pngloader;




package body utex is


use gl;
use type interfaces.c.c_float;
use type interfaces.c.unsigned;

	-- additional bias to remove:
	yadj : constant float := -0.01;






t2dtexid, t2dvbufid, t2duvbufid : gluint;
t2dshadid : interfaces.c.unsigned;
samplerUniformID : glint;
centerUniformID : glint;

Wwid, Whit : glint;







function loadshaders return interfaces.c.unsigned is
	pid : interfaces.c.unsigned;

------------- begin vertex shader code --------------------------
	vscode :  string :=
"#version 330 core "& ascii.lf&
"layout(location = 0) in vec2 inPos; "& ascii.lf&
"layout(location = 1) in vec2 inUV; "& ascii.lf&
"uniform ivec2 screenCen; "& ascii.lf&
"out vec2 UV; "& ascii.lf&
"void main(){ "& ascii.lf&
"vec2 homogPos = inPos - screenCen;"& ascii.lf&
"homogPos /= screenCen;"& ascii.lf&
"gl_Position = vec4(homogPos,-1,1); "& ascii.lf&
"UV = inUV; }"& character'Val(0)
;

	avs : system.address := vscode'address;
------------- end vertex shader code --------------------------

------------- begin fragment shader code --------------------------
	fscode :  string :=
"#version 330 core "& ascii.lf&
"in vec2 UV; "& ascii.lf&
"out vec4 color; "& ascii.lf&
"uniform sampler2D texSampler; "& ascii.lf&
"void main(){ "& ascii.lf&
"  color = texture( texSampler, UV); "& ascii.lf&
"  if( color.a < 0.1 ) discard; }"& character'val(0) --12sep16 addition
;

	afs : system.address := fscode'address;
------------- end fragment shader code --------------------------

	vshaderid, fshaderid	 : aliased gluint;

use glext;
use glext.binding;
use gl.binding;
use  type interfaces.c.unsigned;
use  type interfaces.c.int;


	result : aliased glint := gl_false;
	infologlength : aliased glint := 0;
	errmsg : interfaces.c.strings.chars_ptr;
	amsg : interfaces.c.strings.char_array_access;
	bufsize : glsizei;
	tbufsize : interfaces.c.size_t;
	nulptr : gl.pointers.glsizei_pointer := null;

	error_found: boolean := false;
	shader_error : exception;


begin -- loadshaders

	vshaderid := glcreateshader(gl_vertex_shader);
	fshaderid := glcreateshader(gl_fragment_shader);

	glshadersource(vshaderid, 1, avs'address, system'to_address(0));
	glcompileshader(vshaderid);

	--// Check Vertex Shader
	glGetShaderiv(VShaderID, GL_COMPILE_STATUS, Result'address);
	glGetShaderiv(VShaderID, GL_INFO_LOG_LENGTH, InfoLogLength'address);
	if result=gl_false then
		new_line;
		put_line("Failure to compile VertexShader");
		if infologlength>0 then
			bufsize := glsizei(infologlength+1); -- need nul @ end
			tbufsize := interfaces.c.size_t(infologlength+1);

			amsg := new interfaces.c.char_array(1..tbufsize);

			errmsg := interfaces.c.strings.new_char_array(amsg.all);

			glgetshaderinfolog(vshaderid,bufsize,nulptr,errmsg);
			put_line( interfaces.c.strings.value(errmsg) );
			interfaces.c.strings.free(errmsg);
		end if;
		error_found:=true;
	end if;





	glshadersource(fshaderid, 1, afs'address, system'to_address(0) );
	glcompileshader(fshaderid);

	--// Check Fragment Shader
	glGetShaderiv(fShaderID, GL_COMPILE_STATUS, Result'address);
	glGetShaderiv(fShaderID, GL_INFO_LOG_LENGTH, InfoLogLength'address);
	if result=gl_false then
		new_line;
		put_line("Failure to compile FragmentShader");
		if infologlength>0 then
			bufsize := glsizei(infologlength+1); -- need nul @ end
			tbufsize := interfaces.c.size_t(infologlength+1);

			amsg := new interfaces.c.char_array(1..tbufsize);

			errmsg := interfaces.c.strings.new_char_array(amsg.all);

			glgetshaderinfolog(vshaderid,bufsize,nulptr,errmsg);
			put_line( interfaces.c.strings.value(errmsg) );
			interfaces.c.strings.free(errmsg);
		end if;
		error_found:=true;
	end if;





	pid := glcreateprogram;
	glattachshader(pid, vshaderid);
	glattachshader(pid, fshaderid);
	gllinkprogram(pid);

	--// Check Shader Program Link
	glGetProgramiv(pid, GL_link_STATUS, Result'address);
	glGetProgramiv(pid, GL_INFO_LOG_LENGTH, InfoLogLength'address);
	if result=gl_false then
		put_line("Failure to Link Shader Program");
		new_line;
		if infologlength>0 then
			bufsize := glsizei(infologlength+1); -- need nul @ end
			tbufsize := interfaces.c.size_t(infologlength+1);

			amsg := new interfaces.c.char_array(1..tbufsize);

			errmsg := interfaces.c.strings.new_char_array(amsg.all);

			glgetprograminfolog(pid,bufsize,nulptr,errmsg);
			put_line( interfaces.c.strings.value(errmsg) );
			interfaces.c.strings.free(errmsg);
		end if;
		error_found:=true;
	end if;




	gldeleteshader(vshaderid);
	gldeleteshader(fshaderid);

	if error_found then
		--raise shader_error;
		put_line(" GLerror in utex.LoadShaders");
	end if;


	return pid;

end loadshaders;






-- original baseline
procedure printex( text: string; x,y,size: integer) is

use interfaces.c;
use type interfaces.c.int;
use interfaces.c.strings;
use glext;
use gl.binding;
use glext.binding;

	txtlen : constant integer := text'length;
	nverts : constant int := int(6*txtlen);
	ncoords : constant int := int(12*txtlen);
	verts, uvs : array(0..ncoords-1) of glfloat;
	ch : character;
	ux,uy: glfloat;
	nerr,aski : integer;
	i : glint;

	-- this elliminates textual artifacts caused by neighboring letters:
	dx : constant glfloat := 0.06/16.0; -- 6% of width reduction is best

	utex_error : exception;

	blendWasEnabled : glboolean :=gl.binding.glIsEnabled(gl_blend);

begin


	for ii in 0..txtlen-1 loop
		i:=glint(ii)*12;


------------- 1st triangle -------------
		verts(i+0):=glfloat(x+ii*size);
		verts(i+1):=glfloat(y+size);

		verts(i+2):=glfloat(x+ii*size);
		verts(i+3):=glfloat(y);

		verts(i+4):=glfloat(x+ii*size+size);
		verts(i+5):=glfloat(y+size);

------------- 2nd triangle -------------
		verts(i+6):=glfloat(x+ii*size+size);
		verts(i+7):=glfloat(y);

		verts(i+8):=glfloat(x+ii*size+size);
		verts(i+9):=glfloat(y+size);

		verts(i+10):=glfloat(x+ii*size);
		verts(i+11):=glfloat(y);

-------- now for the uvs ----------------------------

		ch := text( integer(ii+1) );

		aski := character'pos(ch);

		ux := glfloat(aski mod 16)/16.0;
		uy := glfloat(aski / 16)/16.0;


---------- 1st tri ------------------
		uvs(i+0):= ux +dx;
		uvs(i+1):= uy;

		uvs(i+2):= ux +dx;
		uvs(i+3):= uy +1.0/16.0;

		uvs(i+4):= ux +1.0/16.0 -dx;
		uvs(i+5):= uy;

--------- 2nd tri ---------------

		uvs(i+6):= ux +1.0/16.0 -dx;
		uvs(i+7):= uy +1.0/16.0;

		uvs(i+8):= ux +1.0/16.0 -dx;
		uvs(i+9):= uy;

		uvs(i+10):= ux +dx;
		uvs(i+11):= uy +1.0/16.0;


	end loop; -- for i


--put_line("tex: nverts="&int'image(nverts)); --60
--put_line("tex: verts'length = "&int'image(verts'length)); --120




	--bind shader
	gluseprogram( t2dshadid );



	-- bind texture
	--gl.binding.glactivetexture(gl_texture0);
	gl.binding.glbindtexture(gl_texture_2d, t2dtexid);
	gluniform1i(samplerUniformID, 0);

	gluniform2i( centerUniformID, Wwid/2, Whit/2 );



	-- 1st attribute buffer : vertices
	glenablevertexattribarray(0);
	glbindbuffer(glext.gl_array_buffer, t2dvbufid);
	glbufferdata( glext.gl_array_buffer, 
		(float'size/8)*verts'length, verts(0)'address, glext.gl_static_draw);
	glvertexattribPointer(
		0,2,gl_float,gl_false,0, system.null_address);


	-- 2nd attribute buffer : UVs
	glenablevertexattribarray(1);
	glbindbuffer(glext.gl_array_buffer, t2duvbufid);
	glbufferdata( glext.gl_array_buffer, 
		(float'size/8)*uvs'length, uvs(0)'address, glext.gl_static_draw);
	glvertexattribPointer(
		1,2,gl_float,gl_false,0, system.null_address);


	gl.binding.glenable(gl_blend);
	gl.binding.glblendfunc(gl_src_alpha, gl_one_minus_src_alpha);

--minimal error test
if gl_no_error /= glGetError then
	--raise utex_error;
	--put_line(" GLerror in utex.printex");
	nerr:=gameutils.dumpGLerrorQueue("utex.printex");
--
-- 16#0#   =    0 = no_error
-- 16#500# = 1280 = invalid_enum
-- 16#501# = 1281 = invalid_value
-- 16#502# = 1282 = invalid_operation
-- 16#503# = 1283 = stack_overflow
-- 16#504# = 1284 = stack_underflow
-- 16#505# = 1285 = out_of_memory
--

end if;

	-- draw call
	gl.binding.gldrawarrays( gl_triangles, 0, nverts );


	if blendWasEnabled=gl_false then
		gl.binding.gldisable(gl_blend);
	end if;

	gldisablevertexattribarray(0);
	gldisablevertexattribarray(1);


end printex;





procedure inittext2d( fname: string;  swid, shit : integer ) is

use interfaces.c.strings;
use glext.binding;
use gl.binding;

	-- these strings must match exactly the corresponding
	-- names in the shader codes...
	-- "uniform sampler2D" variable name in FS
	-- "uniform ivec2" variable name in VS:
	mysamp :  chars_ptr := new_string("texSampler" & ascii.nul);
	mycen :  chars_ptr := new_string("screenCen" & ascii.nul);

	utex_init_error : exception;
begin

	Wwid := glint(swid);
	Whit := glint(shit);

	--put_line("inittext2d:  (Wwid,Whit) = ("&
	--	glint'image(Wwid)&","&glint'image(Whit)&" )");

	-- lettering file
	t2dtexid:=loadpng(mirror,fname,false);

	glgenbuffers(1, t2dvbufid'address);
	glgenbuffers(1, t2duvbufid'address);

	t2dshadid := loadshaders;

	samplerUniformID := glgetuniformlocation( t2dshadid, mysamp);

	centerUniformID := glgetuniformlocation( t2dshadid, mycen);

--minimal error test
if gl_no_error /= glGetError then
	--raise utex_init_error;
	put_line(" GLerror in utex.InitText2D");
end if;

	free(mysamp);
	free(mycen);

end inittext2d;







--procedure print2d( text: string;	xcen,ycen: glfloat;	size : glint ) is
procedure print2d( text: string;	xcen,ycen: float;	size: integer ) is

--use type interfaces.c.c_float;

	fwid : float := float(Wwid);
	fhit : float := float(Whit);

	fsize : constant float := float(size);
	hfrac : constant float := 0.5*fsize/fwid;
	vfrac : constant float := 0.5*fsize/fhit;

	xoff : constant float := -hfrac;
	yoff : constant float := -vfrac;

	xfac : constant float :=  1.0;
	yfac : constant float :=  1.0;

	xpos : integer := integer( fwid*(xfac*xcen+xoff) );
	ypos : integer := integer( fhit*(yfac*ycen+yoff+yadj) );

begin
	printex( text, xpos, ypos, size );
end print2d;



procedure cleanuptext is

use gl.binding;
use glext.binding;

begin
	gldeletebuffers(1, t2dvbufid'address);
	gldeletebuffers(1, t2duvbufid'address);
	gldeletetextures(1, t2dtexid'address);
	gldeleteprogram(t2dshadid);
end cleanuptext;





procedure print3d( 
	text: string; 
	ccx,ccy,ccz,ccw: float; 
	isize: integer; 
	distance: float -- median Z-distance
	) is

use interfaces.c;


	fsize : constant float := float(isize)/distance;
	nusize : constant integer := integer( fsize );

	fwid : float := float(Wwid);
	fhit : float := float(Whit);

	hfrac : constant float := 0.5*fsize/fwid;
	vfrac : constant float := 0.5*fsize/fhit;


	xoff : constant float := -hfrac;
	yoff : constant float := -vfrac;

	xfac : constant float := 1.0;
	yfac : constant float := 1.0;


	-- Normalize Device Coords in [-1..1]
	xndc : float := ccx/ccw;
	yndc : float := ccy/ccw;
	zndc : float := ccz/ccw;

	-- coords in [0..1]
	xcen : float := (1.0+xndc)/2.0;
	ycen : float := (1.0+yndc)/2.0;

	xpos : integer := integer( fwid*(xfac*xcen+xoff) );
	ypos : integer := integer( fhit*(yfac*ycen+yoff+yadj) );

	eps : constant float := 0.001;

begin

	-- this form needed for portal gun & connection beam
	printex(text, xpos, ypos, nusize);

	-- this form needed for foreground 3D chars
	--if( (-1.0<zndc) and (zndc<distance-eps) ) then
	--	printex(text, xpos, ypos, isize);
	--end if;


end print3d;







procedure printex1a( aski, x,y,size: integer) is

use interfaces.c;
use type interfaces.c.int;
use interfaces.c.strings;
use glext;
use gl.binding;
use glext.binding;

	txtlen : constant integer := 1; --text'length;
	nverts : constant int := int(6*txtlen);
	ncoords : constant int := int(12*txtlen);
	verts, uvs : array(0..ncoords-1) of glfloat;
	ux,uy: glfloat;
	nerr : integer;

	-- this elliminates textual artifacts caused by neighboring letters:
	dx : constant glfloat := 0.06/16.0; -- 6% of width reduction is best

	utex_error : exception;

	blendWasEnabled : glboolean :=gl.binding.glIsEnabled(gl_blend);

begin


------------- 1st triangle -------------
		verts(0):=glfloat(x);
		verts(1):=glfloat(y+size);

		verts(2):=glfloat(x);
		verts(3):=glfloat(y);

		verts(4):=glfloat(x+size);
		verts(5):=glfloat(y+size);

------------- 2nd triangle -------------
		verts(6):=glfloat(x+size);
		verts(7):=glfloat(y);

		verts(8):=glfloat(x+size);
		verts(9):=glfloat(y+size);

		verts(10):=glfloat(x);
		verts(11):=glfloat(y);

-------- now for the uvs ----------------------------

		--ch := text( integer(ii+1) );
		--aski := character'pos(ch);

		ux := glfloat(aski mod 16)/16.0;
		uy := glfloat(aski / 16)/16.0;


---------- 1st tri ------------------
		uvs(0):= ux +dx;
		uvs(1):= uy;

		uvs(2):= ux +dx;
		uvs(3):= uy +1.0/16.0;

		uvs(4):= ux +1.0/16.0 -dx;
		uvs(5):= uy;

--------- 2nd tri ---------------

		uvs(6):= ux +1.0/16.0 -dx;
		uvs(7):= uy +1.0/16.0;

		uvs(8):= ux +1.0/16.0 -dx;
		uvs(9):= uy;

		uvs(10):= ux +dx;
		uvs(11):= uy +1.0/16.0;





	--bind shader
	gluseprogram( t2dshadid );


	-- bind texture
	gl.binding.glbindtexture(gl_texture_2d, t2dtexid);
	gluniform1i(samplerUniformID, 0);
	gluniform2i( centerUniformID, Wwid/2, Whit/2 );


	-- 1st attribute buffer : vertices
	glenablevertexattribarray(0);
	glbindbuffer(glext.gl_array_buffer, t2dvbufid);
	glbufferdata( glext.gl_array_buffer, 
		(float'size/8)*verts'length, verts(0)'address, glext.gl_static_draw);
	glvertexattribPointer(
		0,2,gl_float,gl_false,0, system.null_address);


	-- 2nd attribute buffer : UVs
	glenablevertexattribarray(1);
	glbindbuffer(glext.gl_array_buffer, t2duvbufid);
	glbufferdata( glext.gl_array_buffer, 
		(float'size/8)*uvs'length, uvs(0)'address, glext.gl_static_draw);
	glvertexattribPointer(
		1,2,gl_float,gl_false,0, system.null_address);


	gl.binding.glenable(gl_blend);
	gl.binding.glblendfunc(gl_src_alpha, gl_one_minus_src_alpha);

--minimal error test
if gl_no_error /= glGetError then
	nerr:=gameutils.dumpGLerrorQueue("utex.printex1a");
end if;

	-- draw call
	gl.binding.gldrawarrays( gl_triangles, 0, nverts );

	if blendWasEnabled=gl_false then
		gl.binding.gldisable(gl_blend);
	end if;


	gldisablevertexattribarray(0);
	gldisablevertexattribarray(1);


end printex1a;







procedure print1a( 
	ascii: integer;
	ccx,ccy,ccz,ccw: float; 
	isize: integer; 
	distance: float -- median Z-distance
	) is

use interfaces.c;

	fsize : constant float := float(isize)/distance;
	nusize : constant integer := integer( fsize );

	fwid : float := float(Wwid);
	fhit : float := float(Whit);

	hfrac : constant float := 0.5*fsize/fwid;
	vfrac : constant float := 0.5*fsize/fhit;

	xoff : constant float := -hfrac;
	yoff : constant float := -vfrac;

	xfac : constant float := 1.0;
	yfac : constant float := 1.0;

	-- Normalize Device Coords in [-1..1]
	xndc : float := ccx/ccw;
	yndc : float := ccy/ccw;
	zndc : float := ccz/ccw;

	-- coords in [0..1]
	xcen : float := (1.0+xndc)/2.0;
	ycen : float := (1.0+yndc)/2.0;

	xpos : integer := integer( fwid*(xfac*xcen+xoff) );
	ypos : integer := integer( fhit*(yfac*ycen+yoff+yadj) );

	eps : constant float := 0.001;

begin

	printex1a(ascii, xpos, ypos, nusize);

end print1a;





procedure print2d( ascii: integer;	xcen,ycen: float;	size: integer ) is

--use type interfaces.c.c_float;

	fwid : float := float(Wwid);
	fhit : float := float(Whit);

	fsize : constant float := float(size);
	hfrac : constant float := 0.5*fsize/fwid;
	vfrac : constant float := 0.5*fsize/fhit;

	xoff : constant float := -hfrac;
	yoff : constant float := -vfrac;

	xfac : constant float :=  1.0;
	yfac : constant float :=  1.0;

	xpos : integer := integer( fwid*(xfac*xcen+xoff) );
	ypos : integer := integer( fhit*(yfac*ycen+yoff+yadj) );

begin
	printex1a( ascii, xpos, ypos, size );
end print2d;







end utex;
