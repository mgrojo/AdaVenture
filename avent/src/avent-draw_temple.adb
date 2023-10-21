
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



separate (avent)

procedure draw_temple is

	xeye,yeye,zeye: float;
begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;


		--for normal textured objects:
		glUseProgram( pidtex05 );

gluniform1i(eawe05,0);
gluniform1i(noso05,0);


		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );

		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );

		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness4);

		if chapter=1 then
		gluniform1i(flevid05, foglev4 ); 
		gluniform1i(fcolid05, fogclr4 ); 
		else
		gluniform1i(flevid05, 3 ); 
		gluniform1i(fcolid05, 4 ); 
		end if;

		glbindtexture(gl_texture_2d, adobe_texid);
		droomobj.draw(tdo,vertbuff,uvbuff,elembuff); --textured room


		glbindtexture(gl_texture_2d, greekey_texid);
		rectobj.draw(zpwall, vertbuff,uvbuff,elembuff);
		rectobj.draw(zmwall, vertbuff,uvbuff,elembuff);
		rectobj.draw(xpwall, vertbuff,uvbuff,elembuff);
		rectobj.draw(xmwall, vertbuff,uvbuff,elembuff);

		glbindtexture(gl_texture_2d, mural_texid);
		pictobj.draw(zpmural, vertbuff,uvbuff,elembuff);


		--floor (only needed if different than walls)
		glbindtexture(gl_texture_2d, gmarble_texid);
		rectobj.draw(tfloor, vertbuff,uvbuff,elembuff);

		--ceiling (only needed if different than walls)
		glbindtexture(gl_texture_2d, gmarble_texid);
		rectobj.draw(rceil, vertbuff,uvbuff,elembuff);




		--pillars
		glbindtexture(gl_texture_2d, gmarble_texid);
		cylobj.draw(pillar3, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar4, vertbuff,uvbuff,elembuff);
		pictobj.draw(beam34, vertbuff,uvbuff,elembuff);

		cylobj.draw(pillar5, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar6, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar7, vertbuff,uvbuff,elembuff);

		cylobj.draw(pillar8, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar9, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar10, vertbuff,uvbuff,elembuff);



		-- roseQuartz altar:
		glbindtexture(gl_texture_2d, rosequartz_texid);
		pictobj.draw(rsafe, vertbuff,uvbuff,elembuff);



		if not gkeyheld and sgkey=4  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=4 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
		end if;

		if not wkeyheld and swkey=4 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=4 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
		end if;

		if not gateheld and sgate=4  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;


-----------------------------------------------------------------


	if drawchalice and schalice=4 and not chaliceheld then
	-- use uniforms to set position
		glUseProgram(pidcup06);

		gluniform1i(lFlagid06, 1 ); --no light effects
		glUniform1f(hangid06, 0.0);
		gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
		gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );




		glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
		glUniform1i(sampid06, 0);
		glUniform1i(darkid06, 0);
		glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
		glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

		gluniform1i(flevid06, 0); --foglev4 ); 
		gluniform1i(fcolid06, 0); --fogclr4 ); 

		glbindtexture(gl_texture_2d, chalice_texid);
		xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);

	end if;



	if scene=4 then
		showWhatIsHeld;
	end if;


end draw_temple;


