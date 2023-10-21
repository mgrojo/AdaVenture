
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

procedure draw_maze6 is

	hwdoor : constant float := 0.8;
	fhdoor : constant float := 1.8;

	xeye,yeye,zeye: float;

begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;

		snake6rad := 7.0;

		snake6angl := 0.02*onepi*float(currenttime);

		-- given my coords, this defines a clockwise rotation:
		x6snake:= snake6rad*fmath.cos(snake6angl);
		z6snake:= snake6rad*fmath.sin(snake6angl);


		-- green mamba
		glUseProgram( pidsnake12 );

		gluniform3f(eyeid12, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv(mvpid12, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid12,0);
		glUniform3f(cenID12, 0.0, glfloat( -iymax+0.02 ), 0.0 );
		glUniform1f(radid12, glfloat(snake6rad) ); -- circle radius
		glUniform1f(anglid12, glfloat(snake6angl) ); -- angle around orbit
		glUniform1f(wvelid12, 1.0 ); --wiggle freq
		glUniform1f(wampid12, 4.0 ); --wiggle amplitude

		glUniform1i(darkid12, darkness6);
		if chapter=1 then
		gluniform1i(flevid12, foglev6 ); 
		gluniform1i(fcolid12, fogclr6 ); 
		else
		gluniform1i(flevid12, 3 ); 
		gluniform1i(fcolid12, 4 ); 
		end if;

		glbindtexture(gl_texture_2d, snake_texid);
		longtube.draw( snake, vertbuff,uvbuff,elembuff );






		--for normal textured objects:
		glUseProgram( pidtex05 );

xdc:=xdc6;
zdc:=zdc6;
--gluniform1i(ndc05,ndc6);
gluniform1i(eawe05,eawe);
gluniform1i(noso05,0);
gluniform1fv(xdc05,ndc6,xdc(1)'address);
gluniform1fv(zdc05,ndc6,zdc(1)'address);
--put_line("|| EAWE: "&glint'image(eawe));

		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );

		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness6);

		if chapter=1 then
		gluniform1i(flevid05, foglev6 ); 
		gluniform1i(fcolid05, fogclr6 ); 
		else
		gluniform1i(flevid05, 3 ); 
		gluniform1i(fcolid05, 4 ); 
		end if;


		glbindtexture(gl_texture_2d, mazeouter_texid);
		droomobj.draw(mdo6,vertbuff,uvbuff,elembuff); --textured room


		--floor
		glbindtexture(gl_texture_2d, grass_texid);
		rectobj.draw(mfloor, vertbuff,uvbuff,elembuff);

		--bugfloor crevasse from which bugs spring forth
		--glbindtexture(gl_texture_2d, slime_texid);
		glbindtexture(gl_texture_2d, black_texid);
		rectobj.draw(bugfloor1, vertbuff,uvbuff,elembuff);
		rectobj.draw(bugfloor2, vertbuff,uvbuff,elembuff);


		glbindtexture(gl_texture_2d, doort_texid);
		-- transition doors
		pictobj.draw(doora1, vertbuff,uvbuff,elembuff);
		pictobj.draw(doora2, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorb1, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorb2, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorv6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorw6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorx6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doory6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorz6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorc6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doord6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doore6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorf6, vertbuff,uvbuff,elembuff);
		pictobj.draw(doorg6, vertbuff,uvbuff,elembuff);



		drawBugs( float(currentTime) );



		--maze walls
		glbindtexture(gl_texture_2d, hedge_texid); --barrier
		for row in -mrows..mrows loop
		for col in -mcols..mcols loop
		if 
			iswall(6,row,col) and
			(kgate=0 or sgate/=6 or rgate/=row or cgate/=col)
		then
				pictobj.draw(
					mzwall(6,row,col), 
						vertbuff,uvbuff,elembuff);
		end if;
		end loop;
		end loop;


		if kgate>0 and sgate=6 then
			glbindtexture(gl_texture_2d, frame_texid); --passthru
			pictobj.draw(
				mzwall(6,rgate,cgate), 
					vertbuff,uvbuff,elembuff);
		end if;







		if liongoingup and not lionopen then

			elapsed := currenttime-liontime;
			tt := elapsed/liftduration;
			if tt>1.0 then 
				lionwait:=false; 
				lionopen:=true;
				liongoingup:=false;
				tt:=1.0; 
			end if;

			ylion:=-iymax+fhdoor/2.0+float(tt)*fhdoor*0.8;
			pictobj.setrect( 
				dungdoor,
				xlion, ylion, zlion,
				hwdoor-0.05, fhdoor/2.0, 0.05,
				j1,j2,j3,j4,j5,j6);

			--disable KO #lko
			koxlo(lko):=0.0;
			koxhi(lko):=0.0;
			koylo(lko):=0.0;
			koyhi(lko):=0.0;
			kozlo(lko):=0.0;
			kozhi(lko):=0.0;

		end if;

		-- lion lifting door
		glbindtexture(gl_texture_2d, lion_texid);
		pictobj.draw(dungdoor, vertbuff,uvbuff,elembuff);

		--pillars
		glbindtexture(gl_texture_2d, gmarble_texid);
		cylobj.draw(pillar1, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar2, vertbuff,uvbuff,elembuff);
		pictobj.draw(beam12, vertbuff,uvbuff,elembuff);








		if not gkeyheld and sgkey=6  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=6 and not bathasbkey then

			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key0, vertbuff,uvbuff,elembuff); --ghostKey 9apr21

		end if;

		if not wkeyheld and swkey=6 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=6 and not bathassword then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
			pictobj.draw(sword0, vertbuff,uvbuff,elembuff); --ghostsword
		end if;

		if not gateheld and sgate=6  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;

---------------------------------------------------------------


		if drawchalice and schalice=6 and not chaliceheld then
		-- use uniforms to set position
			glUseProgram(pidcup06);

			gluniform1i(lFlagid06, 1 ); --no light effects
			glUniform1f(hangid06, 0.0);
			gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
			gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



			glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
			glUniform1i(sampid06, 0);
			glUniform1i(darkid06, 0); --darkness6);
			glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
			glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

			gluniform1i(flevid06, 0); --foglev6 ); 
			gluniform1i(fcolid06, 0); --fogclr6 ); 

			glbindtexture(gl_texture_2d, chalice_texid);
			xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
		end if;


	if scene=6 then
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

		if batfly and scene=6 and sbkey=6 then --draw flying bat as per gun-beam

			dt:=-0.5+1.0*(currenttime-batstart)/(0.5*batduration); -- -0.5..+0.5

			if dt>0.0 and not batested6 then
				batested6:=true;

				if not bathasbkey and not bkeyheld then 
					-- bat succeeded in grabbing key from the ground!
					bathasbkey:=true;
					snd4ada.playSnd(up);

				elsif not bathassword and not swordheld and ssword=6 and bkeyheld	then 
					--sword was dropped to pickup key, so bat grabs sword instead
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

					pictobj.setrect(  -- draw ghostKey hint on ceiling
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



end draw_maze6;


