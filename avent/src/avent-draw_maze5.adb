
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

procedure draw_maze5 is

	hwdoor : constant float := 0.8;
	fhdoor : constant float := 1.8;
	dt: gldouble;

	xeye,yeye,zeye: float;
begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;


		snake5rad := 7.0;

		snake5angl := 0.02*onepi*float(currenttime);

		-- given my coords, this defines a clockwise rotation:
		x5snake:= snake5rad*fmath.cos(snake5angl);
		z5snake:= snake5rad*fmath.sin(snake5angl);


		-- green mamba
		glUseProgram( pidsnake12 );

		gluniform3f(eyeid12, glfloat(xeye),glfloat(yeye),glfloat(zeye) );


		gluniformmatrix4fv(mvpid12, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid12,0);
		glUniform3f(cenID12, 0.0, glfloat(-iymax+0.02 ), 0.0 );
		glUniform1f(radid12, glfloat(snake5rad) ); -- circle radius
		glUniform1f(anglid12, glfloat(snake5angl) ); -- angle around orbit
		glUniform1f(wvelid12, 1.0 ); --wiggle freq
		glUniform1f(wampid12, 4.0 ); --wiggle amplitude

		glUniform1i(darkid12, darkness5);
		if chapter=1 then
		gluniform1i(flevid12, foglev5 ); 
		gluniform1i(fcolid12, fogclr5 ); 
		else
		gluniform1i(flevid12, 3 ); 
		gluniform1i(fcolid12, 4 ); 
		end if;

		glbindtexture(gl_texture_2d, snake_texid);
		longtube.draw( snake, vertbuff,uvbuff,elembuff );





		--for normal textured objects:
		glUseProgram( pidtex05 );

xdc:=xdc5;
zdc:=zdc5;
--gluniform1i(ndc05,ndc5);
gluniform1i(eawe05,eawe);
gluniform1i(noso05,0);
gluniform1fv(xdc05,ndc5,xdc(1)'address);
gluniform1fv(zdc05,ndc5,zdc(1)'address);
--put_line("|| EAWE: "&glint'image(eawe));


		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness5);
		if chapter=1 then
		gluniform1i(flevid05, foglev5 ); 
		gluniform1i(fcolid05, fogclr5 ); 
		else
		gluniform1i(flevid05, 3 ); 
		gluniform1i(fcolid05, 4 ); 
		end if;




		glbindtexture(gl_texture_2d, mazeouter_texid);
		droomobj.draw(mdo5,vertbuff,uvbuff,elembuff); --textured room

		--floor
		glbindtexture(gl_texture_2d, grass_texid);
		rectobj.draw(mfloor, vertbuff,uvbuff,elembuff);



		--sign
		glbindtexture(gl_texture_2d, exit_texid); --exit sign
		pictobj.draw(ex5, vertbuff,uvbuff,elembuff);


		--maze entry door
		glbindtexture(gl_texture_2d, doort_texid);
		pictobj.draw(imazedoor, vertbuff,uvbuff,elembuff);

		-- transition doors
		pictobj.draw(doorc, vertbuff,uvbuff,elembuff);
		pictobj.draw(doord, vertbuff,uvbuff,elembuff);
		pictobj.draw(doore, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorf, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorg, vertbuff,uvbuff,elembuff);
		pictobj.draw(doory, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorz, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorv, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorw, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorx, vertbuff,uvbuff,elembuff);





		--maze walls
		glbindtexture(gl_texture_2d, hedge_texid);
		for row in -mrows..mrows loop
		for col in -mcols..mcols loop
		if 
			iswall(5,row,col) and
			(kgate=0 or sgate/=5 or rgate/=row or cgate/=col) 
		then
				pictobj.draw(
					mzwall(5,row,col), 
						vertbuff,uvbuff,elembuff);
		end if;

		end loop;
		end loop;

		if kgate>0 and sgate=5 then
			glbindtexture(gl_texture_2d, frame_texid); --passthru
			pictobj.draw(
				mzwall(5,rgate,cgate), 
					vertbuff,uvbuff,elembuff);
		end if;





		if not gkeyheld and sgkey=5  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=5 and not bathasbkey then

			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key0, vertbuff,uvbuff,elembuff); --ghostKey 9apr21

		end if;

		if not wkeyheld and swkey=5 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=5 and not bathassword then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
			pictobj.draw(sword0, vertbuff,uvbuff,elembuff);--ghostsword
		end if;

		if not gateheld and sgate=5  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;


---------------------------------------------------------------

		if drawchalice and schalice=5 and not chaliceheld then
		-- use uniforms to set position
			glUseProgram(pidcup06);

			gluniform1i(lFlagid06, 1 ); --no light effects
			glUniform1f(hangid06, 0.0);
			gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
			gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



			glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
			glUniform1i(sampid06, 0);
			glUniform1i(darkid06, 0); --darkness5);
			glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
			glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );


			gluniform1i(flevid06, 0); --foglev5 ); 
			gluniform1i(fcolid06, 0); --fogclr5 ); 

			glbindtexture(gl_texture_2d, chalice_texid);
			xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
		end if;


	if scene=5 then
		showWhatIsHeld;
	end if;


		-- fancy fragshader draws cloud ceiling
		--glUseProgram( pidsky11 );
		--gluniformmatrix4fv( mvpid11, 1, gl_false, imvp(1,1)'address );
		--gluniform1f(timeid11, glfloat(currentTime) );
		--gluniform2f(resid11, glfloat(winwidth), glfloat(winheight) );
		--rectxobj.draw(rox,vertbuff,elembuff);


---------------- begin bat insert ------------------

		--note: sendBat was called when bkeyseen is true

		if batfly and scene=5 and sbkey=5 then --draw flying bat as per gun-beam

			dt:=-0.5+1.0*(currenttime-batstart)/(0.5*batduration); -- -0.5..+0.5

			if dt>0.0 and not batested5 then
				batested5:=true;

				if not bathasbkey and not bkeyheld then 
					-- bat succeeded in grabbing key from the ground!
					bathasbkey:=true;
					snd4ada.playSnd(up);

				elsif not bathassword and not swordheld and ssword=5 and bkeyheld then 
					--sword was dropped to pickup key, so bat grabs sword
					bathassword:=true; -- bat grabbed the sword from ground
					snd4ada.playSnd(up);
				end if;

			end if;

			if dt>=0.5 then
				batfly:=false;
				if not bkeyheld and bathasbkey then
					--bat drops key onto ground
					xbkey:=xbat;
					zbkey:=zbat;
					ybkey:=-iymax+htobj+htobj;

					pictobj.setrect( 
						key2, 
						xbkey,ybkey,zbkey, --xc,yc,zc
						0.1, 0.0, 0.1, --xr,yr,zr
						j1,j2,j3,j4,j5,j6);

					pictobj.setrect(  -- place ghostKey hint on ceiling
						key0, 
						xbkey, -htobj-htobj ,zbkey, --xc,yc,zc
						0.1, 0.0, 0.1, --xr,yr,zr
						j1,j2,j3,j4,j5,j6);

					bathasbkey:=false;
					snd4ada.playSnd(down);

				elsif not swordheld and bathassword then -- 10feb23 addendum
					--bat drops sword onto ground
					xsword:=xbat;
					zsword:=zbat;
					ysword:=-iymax+htobj;

					pictobj.setrect( 
						sword, 
						xsword, ysword, zsword, --xc,yc,zc
						0.1, 0.0, 0.1, --xr,yr,zr
						j1,j2,j3,j4,j5,j6);

					--set position of ghostSword:
					pictobj.setrect(
						sword0, 
						xsword,-htobj-htobj,zsword,
						rsword, 0.0, 0.2*rsword,
						j1,j2,j3,j4,j5,j6);

					bathassword:=false;
					snd4ada.playSnd(down);
				end if;
			else --draw bat
				drawbat(dt);
			end if;
		end if; --batfly

---------------- end bat insert ------------------

end draw_maze5;


