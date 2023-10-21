
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

procedure draw_maze7 is

	hhdoor : constant float := 0.9; --halfHeight
	hwdoor : constant float := 0.8;
	fhdoor : constant float := 1.8;

	dt: gldouble;
	frac : constant gldouble := 0.125; --fraction of normal duration

	yy: float;

	xeye,yeye,zeye: float;


begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;

		snake7rad := 7.0;

		snake7angl := 0.02*onepi*float(currenttime);

		-- given my coords, this defines a clockwise rotation:
		x7snake:= snake7rad*fmath.cos(snake7angl);
		z7snake:= snake7rad*fmath.sin(snake7angl);


		-- green mamba
		glUseProgram( pidsnake12 );

		gluniform3f(eyeid12, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv(mvpid12, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid12,0);
		glUniform3f(cenID12, 0.0, glfloat( -iymax+0.02 ), 0.0 );
		glUniform1f(radid12, glfloat(snake7rad) ); -- circle radius
		glUniform1f(anglid12, glfloat(snake7angl) ); -- angle around orbit
		glUniform1f(wvelid12, 1.0 ); --wiggle freq
		glUniform1f(wampid12, 4.0 ); --wiggle amplitude

		glUniform1i(darkid12, darkness7);
		if chapter=2 then
			gluniform1i(flevid12, foglev7 ); --2
			gluniform1i(fcolid12, fogclr7 ); --1
		else
			gluniform1i(flevid12, 1+foglev7 ); --  3 ); 
			gluniform1i(fcolid12, 2+fogclr7 ); --  4 );
		end if;

		glbindtexture(gl_texture_2d, snake_texid);
		longtube.draw( snake, vertbuff,uvbuff,elembuff );






		--for normal textured objects:
		glUseProgram( pidtex05 );

xdc:=xdc7;
zdc:=zdc7;
--gluniform1i(ndc05,ndc7);
gluniform1i(eawe05,eawe);
gluniform1i(noso05,noso);
gluniform1fv(xdc05,ndc7,xdc(1)'address);
gluniform1fv(zdc05,ndc7,zdc(1)'address);


		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );

		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness7);

		if chapter=2 then
		gluniform1i(flevid05, foglev7 ); 
		gluniform1i(fcolid05, fogclr7 ); 
		else
		gluniform1i(flevid05, 3 ); 
		gluniform1i(fcolid05, 4 ); 
		end if;

		glbindtexture(gl_texture_2d, mazeouter_texid);
		droomobj.draw(mdo7,vertbuff,uvbuff,elembuff); --textured room


		--floor
		glbindtexture(gl_texture_2d, grass_texid);
		rectobj.draw(mfloor, vertbuff,uvbuff,elembuff);



		--sign
		glbindtexture(gl_texture_2d, exit_texid); --exit sign
		pictobj.draw(ex7, vertbuff,uvbuff,elembuff);


		--maze entry door
		glbindtexture(gl_texture_2d, doort_texid);
		pictobj.draw(imazedoor, vertbuff,uvbuff,elembuff);


		-- transition doors
		pictobj.draw(al7, vertbuff,uvbuff,elembuff);
		pictobj.draw(ar7, vertbuff,uvbuff,elembuff);
		pictobj.draw(bl7, vertbuff,uvbuff,elembuff);
		pictobj.draw(br7, vertbuff,uvbuff,elembuff);

		pictobj.draw(cl7, vertbuff,uvbuff,elembuff);
		pictobj.draw(cr7, vertbuff,uvbuff,elembuff);
		pictobj.draw(dl7, vertbuff,uvbuff,elembuff);
		pictobj.draw(dr7, vertbuff,uvbuff,elembuff);

		pictobj.draw(el7, vertbuff,uvbuff,elembuff);
		pictobj.draw(er7, vertbuff,uvbuff,elembuff);
		pictobj.draw(fl7, vertbuff,uvbuff,elembuff);
		pictobj.draw(fr7, vertbuff,uvbuff,elembuff);

		--entry doors
		pictobj.draw(h7, vertbuff,uvbuff,elembuff);

		glbindtexture(gl_texture_2d, lab_texid); --labyrinth signs
		pictobj.draw(lh7, vertbuff,uvbuff,elembuff);



		drawBugs( float(currentTime) );





--begin addenda (sliding lab doors)

		if labopening then

			elapsed := currenttime-labtime;
			tt := elapsed/liftduration;
			if tt>1.0 then 
				labwait:=false; 
				labopen:=true;
				labopening:=false;
				tt:=1.0; 
			end if;

			yy:=ylab - float(tt)*hhdoor;
			pictobj.setrect( 
				sh7,
				xlab,yy,zlab,
				0.0, hhdoor*0.5, hwdoor*0.51,
				j1,j2,j3,j4,j5,j6);


			--disable KO
			koxlo(wlabko):=0.0;
			koxhi(wlabko):=0.0;
			koylo(wlabko):=0.0;
			koyhi(wlabko):=0.0;
			kozlo(wlabko):=0.0;
			kozhi(wlabko):=0.0;


		end if;

gluniform1i(eawe05,0); --always draw door, thus
gluniform1i(noso05,0); --temporarily elliminate discard

		glbindtexture(gl_texture_2d, keyhole_texid);
		pictobj.draw(sh7, vertbuff,uvbuff,elembuff);

gluniform1i(eawe05,eawe);
gluniform1i(noso05,noso);

--end addenda


		--maze walls
		glbindtexture(gl_texture_2d, hedge_texid); --barrier
		for row in -mrows..mrows loop
		for col in -mcols..mcols loop
		if 
			iswall(7,row,col) and
			(kgate=0 or sgate/=7 or rgate/=row or cgate/=col)
		then
			pictobj.draw(
				mzwall(7,row,col), 
					vertbuff,uvbuff,elembuff);
		end if;
		end loop;
		end loop;

		if kgate>0 and sgate=7 then
			glbindtexture(gl_texture_2d, frame_texid); --passthru
			pictobj.draw(
				mzwall(7,rgate,cgate), 
					vertbuff,uvbuff,elembuff);
		end if;



---------------------------------------------------------------

		if not gkeyheld and sgkey=7  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=7 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff); --Black Key
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key0, vertbuff,uvbuff,elembuff); --ghostKey 9apr21
		end if;

		if not wkeyheld and swkey=7 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=7 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);

			--draw ghostSword on ceiling:
			pictobj.draw(sword0, vertbuff,uvbuff,elembuff);

		end if;

		if not gateheld and sgate=7  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;






---------------- begin bat insert ------------------

		if batfly and scene=7 then --draw flying bat as per gun-beam
			dt:=(-1.0+(currenttime-batstart)/(frac*batduration)); 
			-- -1..+1

			if dt>0.0 and not batested7 then
				batested7:=true;

				if not bathaschalice then
					chaliceheld:=false;
					bathaschalice:=true;
					-- this flag being false indicates the bat
					-- succeeded in grabbing chalice!
					snd4ada.playSnd(up);
				end if;

			end if;

			if dt>=1.0 then
				batfly:=false;
				if bathaschalice then
					--bat drops chalice onto ground
					xchalice:=0.0;
					zchalice:=0.0; --@center of maze7
					ychalice:=hcup-iymax+htobj;
					schalice:=7;
					bathaschalice:=false;
					snd4ada.playSnd(down);
				end if;
			else --draw bat
				drawbat(dt);
			end if;
		end if; --batfly

---------------- end bat insert ------------------





	if drawchalice and schalice=7 and not chaliceheld and not bathaschalice then
	-- use uniforms to set position
		glUseProgram(pidcup06);

		gluniform1i(lFlagid06, 1 ); --no light effects
		glUniform1f(hangid06, 0.0);
		gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
		gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );




		glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
		glUniform1i(sampid06, 0);
		glUniform1i(darkid06, 0); --darkness7);
		glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
		glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

		gluniform1i(flevid06, 0); --foglev7 ); 
		gluniform1i(fcolid06, 0); --fogclr7 ); 

		glbindtexture(gl_texture_2d, chalice_texid);
		xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
	end if;


	if scene=7 then
		showWhatIsHeld;
	end if;


---------------------------------------------------------------------
		-- fancy fragshader draws cloud ceiling
		--glUseProgram( pidsky10 );
		--gluniformmatrix4fv( mvpid10, 1, gl_false, imvp(1,1)'address );
		--gluniform1f(timeid10, glfloat(currentTime) );
		--gluniform2f(resid10, glfloat(winwidth), glfloat(winheight) );
		--rectxobj.draw(rox,vertbuff,elembuff);
		--18nov19
---------------------------------------------------------------------

end draw_maze7;


