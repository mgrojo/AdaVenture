
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

procedure draw_lab8 is







	barangle: float;
	ballsec: gldouble;
	ibsec,ii,kk: integer;
	nslic: constant integer := 1000; --partitions per direction
	xfb1 : constant float := -ixmax+barrad;       -- one end
	xfb2 : constant float := +ixmax-barrad;       -- other end
	xxfb,yyfb,zzfb : float;


procedure getFireBallPos(xxx,yyy,zzz: out float) is
begin
----------------------------------------------------------------------
			ballsec := 60.0*glfwGetTime;
			ibsec := integer( ballsec );
			kk:= ibsec/nslic;
			ii:= 1 + ibsec mod nslic; -- 1..nslic = 1..1000
			if (kk mod 2 = 1) then
				null;
			else
				ii:= 1 + nslic - ii; -- 1001-i = 1..1000
			end if;
			xxx := xfb1 + float(ii-1)/float(nslic) * (xfb2-xfb1);
			yyy:= -iymax+barrad;
			zzz:= -9.2;
-----------------------------------------------------------------------
end getFireBallPos;










	hwdoor : constant float := 0.8;
	fhdoor : constant float := 1.8;

	xtc,ytc,ztc, xtr,ytr,ztr, yb : glfloat;

	xeye,yeye,zeye: float;

	collided: boolean := false;

begin

	if thirdperson then
		xeye:=xcam;
		yeye:=ycam;
		zeye:=zcam;
	else
		xeye:=xme;
		yeye:=yme;
		zeye:=zme;
	end if;



		snake8rad := 7.0;

		snake8angl := 0.02*onepi*float(currenttime);

		-- given my coords, this defines a clockwise rotation:
		x8snake:= snake8rad*fmath.cos(snake8angl);
		z8snake:= snake8rad*fmath.sin(snake8angl);


		-- green mamba
		glUseProgram( pidsnake12 );

		gluniform3f(eyeid12, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		gluniformmatrix4fv(mvpid12, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid12,0);
		glUniform3f(cenID12, 0.0, glfloat( -iymax+0.02 ), 0.0 );
		glUniform1f(radid12, glfloat(snake8rad)); --circle radius
		glUniform1f(anglid12, glfloat(snake8angl)); --angle around orbit
		glUniform1f(wvelid12, 1.0 ); --wiggle freq
		glUniform1f(wampid12, 4.0 ); --wiggle amplitude

		glUniform1i(darkid12, darkness8);
		--if chapter=2 then
		gluniform1i(flevid12, foglev8 ); --snake
		gluniform1i(fcolid12, fogclr8 ); 
		--else
		--gluniform1i(flevid12, 3 ); 
		--gluniform1i(fcolid12, 4 ); 
		--end if;

		glbindtexture(gl_texture_2d, snake_texid);
		longtube.draw( snake, vertbuff,uvbuff,elembuff );






		--for normal textured objects:
		glUseProgram( pidtex05 );

--gluniform1i(eawe05,0);
--gluniform1i(noso05,0);

xdc:=xdc8;
zdc:=zdc8;
--gluniform1i(ndc05,ndc7);
gluniform1i(eawe05,eawe);
gluniform1i(noso05,0);
gluniform1fv(xdc05,ndc8,xdc(1)'address);
gluniform1fv(zdc05,ndc8,zdc(1)'address);


		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );


		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );

		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness8);

		--if chapter=2 then
		gluniform1i(flevid05, foglev8 ); --roomWallsFloor
		gluniform1i(fcolid05, fogclr8 ); 
		--else
		--gluniform1i(flevid05, 3 ); 
		--gluniform1i(fcolid05, 4 ); 
		--end if;

		glbindtexture(gl_texture_2d, roots_texid);
		droomobj.draw(mdo8,vertbuff,uvbuff,elembuff); --textured room


		--floor
		glbindtexture(gl_texture_2d, slime_texid); -- nice slime!
		rectobj.draw(lfloor, vertbuff,uvbuff,elembuff);

		--doors
		glbindtexture(gl_texture_2d, doort_texid);
		pictobj.draw(g8, vertbuff,uvbuff,elembuff);
		pictobj.draw(h8, vertbuff,uvbuff,elembuff);

		--exit
		glbindtexture(gl_texture_2d, exit_texid);
		pictobj.draw(eh8, vertbuff,uvbuff,elembuff);

		--darkmaze
		glbindtexture(gl_texture_2d, dmaze_texid);
		pictobj.draw(eg8, vertbuff,uvbuff,elembuff);



		--labyrinth walls
		glbindtexture(gl_texture_2d, slime_texid); -- nice slime!
		for row in -mrows..mrows loop
		for col in -mcols..mcols loop
		if 
			iswall(8,row,col) and
			(kgate=0 or sgate/=8 or rgate/=row or cgate/=col)
		then
			pictobj.draw(
				mzwall(8,row,col), 
					vertbuff,uvbuff,elembuff);
		end if;
		end loop;
		end loop;

		if kgate>0 and sgate=8 then
			glbindtexture(gl_texture_2d, frame_texid); --passthru
			pictobj.draw(
				mzwall(8,rgate,cgate), 
					vertbuff,uvbuff,elembuff);
		end if;







		if not gkeyheld and sgkey=8  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=8 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
		end if;

		if not wkeyheld and swkey=8 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=8 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
			pictobj.draw(sword0, vertbuff,uvbuff,elembuff);--ghostSword
		end if;

		if not gateheld and sgate=8  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;













---------------------------------------------------------------
		-- addendum draw tree begin

		xtc:=0.0;
		ztc:=0.0;

		xtr:=2.0;
		ytr:=2.0;
		ztr:=xtr;

		ytc:=glfloat(-iymax) + ytr - 0.5;

		yb := glfloat(-iymax); -- ytc-ytr;


		--draw tree

		glUseProgram(pidtree08);
		glUniform3f(eyeid08,glfloat(xeye),glfloat(yeye),glfloat(zeye));


		glUniformMatrix4fv(mvpid08, 1, GL_FALSE, imvp(1,1)'address);

		--if chapter=2 then
		glUniform1i(flevid08, foglev8); --19dec17 Tree
		glUniform1i(fcolid08, fogclr8); --19dec17
		--else
		--glUniform1i(flevid08, 3);
		--glUniform1i(fcolid08, 4);
		--end if;

		glUniform1i(sampid08, 0);
		glUniform1i(darkid08, darkness8);
		glUniform1f(timeid08, glfloat(currentTime) ); 
		--shader makes them sway

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

		----------------------------------------------------
		glUniform3f(cenid08, xtc,ytc,ztc );
		glUniform3f(radid08, xtr,ytr,ztr );
		glUniform1f(baseid08, yb );

		glUniform1i(trid08, glint(1) );
		glBindTexture(GL_TEXTURE_2D, tree8_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xtc),zeye-float(ztc)
			);

		----------------------------------------------------




---------------------------------------------------------------

		if drawchalice and schalice=8 and not chaliceheld then
		-- use uniforms to set position
			glUseProgram(pidcup06);

			gluniform1i(lFlagid06, 1 ); --light effects
			glUniform1f(hangid06, 0.0);
			gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
			gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );




			glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
			glUniform1i(sampid06, 0);
			glUniform1i(darkid06, 0); --darkness8);
			glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
			glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

			gluniform1i(flevid06, 0); --foglev8 ); 
			gluniform1i(fcolid06, 0); --fogclr8 ); 

			glbindtexture(gl_texture_2d, chalice_texid);
			xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);

		end if;


---------------------------------------------------------------


		if scene=8 then
			showWhatIsHeld;
		end if;


		-- do NOT show this ceiling, the default texture is awesome!
		-- fancy fragshader draws B&W cloud ceiling
		--glUseProgram( pidstar04 );
		--gluniformmatrix4fv( mvpid04, 1, gl_false, imvp(1,1)'address );
		--gluniform1f(timeid04, glfloat(currentTime) );
		--gluniform2f(resid04, glfloat(winwidth), glfloat(winheight) );
		--rectxobj.draw(rox,vertbuff,elembuff);




		if play9 then -- we are returning with chalice

		----------- draw fireball --------------------------------------

			glUseProgram(pidfire03); --===============================
			glUniform1i(sampid03, 0);
			glUniform1f(opacid03, 0.9);
			glUniform1f(radid03, glfloat(barrad));
			glBindTexture(GL_TEXTURE_2D, ball_texid);

			-- begin fireball roll --------------------------------------
			BMVP := IMVP;

			getFireBallPos(xxfb,yyfb,zzfb);

			axis:=zaxis;

			bmm:=identity;
			translate(bmm, -xxfb,-yyfb,-zzfb ); --Xlate to origin
			barangle := -twopi*xxfb/(twopi*barrad) *rad2deg;
			degrotate(bmm, barangle, axis(1), axis(2), axis(3) ); --rotate
			translate(bmm, xxfb,yyfb,zzfb ); --Xlate back
			bmvp := bmm;
			matXmat( bmvp, imv ); --ViewMatrix );
			matXmat( bmvp, pm ); --ProjectionMatrix );

			-- we have to register bmvp AFTER it is redefined:
			gluniformmatrix4fv( mvpid03, 1, gl_false, BMVP(1,1)'address );

			glUniform3f(cenid03,  --send current centroid to shaders
				glfloat(xxfb),glfloat(yyfb),glfloat(zzfb) );

			glUniform1f(timeid03, glfloat(currenttime));

			fireballobj.draw(fireball,vertbuff,elembuff);

			if fmath.sqrt( sqr(xxfb-xme)+sqr(yyfb-yme)+sqr(zzfb-zme) ) 
				< barrad+margin then
				imdead_fireball:=true;
				--snd4ada.stopLoop(dang8);
				--snd4ada.stopLoop(atmos8);
				snd4ada.stopLoops;
			end if;

			-- end fireball roll ------------------------------------------

		end if; --play9



	if  scene=8 then
		updateBull(
			currentTime,xme,zme,
			xbull,ybull,zbull,hbull,dbull, collided);

		if minorun then

			if collided then
				minorun:=false;
				mxdra:=xme;
				mzdra:=zme;
				mydra:=-iymax+2.0*htobj;
				pictobj.setrect( minotaur,
					mxdra,mydra,mzdra,
					0.6, 0.0, 0.6, --xr,yr,zr
					j1,j2,j3,j4,j5,j6);

				snd4ada.stopLoop(dang8);
				snd4ada.playSnd(roar); --Roar (in either case) 11sep16
				delay 0.8; -- give time to play roar

				if swordheld then --minotaur dies
					minotaurdead:=true;
					snd4ada.playSnd(die); -- dragondie.wav
					delay 0.8;
				else --minotaur eats you...
					snd4ada.stopLoop(atmos8);
					--snd4ada.playSnd(eat); -- eaten.wav
					--delay 1.0;
					userexit:=true;
					imdead_minotaur:=true;
				end if;

			end if;

		end if; --minorun

		if not minotaurdead then
			drawBull(currentTime,xbull,ybull,zbull,hbull,dbull);
		end if;

	end if; --scene8


	if scene=8 and minotaurdead then
		glUseProgram( pidtex05 );
		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid05,0);
		glUniform1i(darkid05, darkness8);
		--if chapter=2 then
		gluniform1i(flevid05, foglev8 ); --deadMinotaur
		gluniform1i(fcolid05, fogclr8 ); 
		--else
		--gluniform1i(flevid05, 3 ); 
		--gluniform1i(fcolid05, 4 ); 
		--end if;
		glbindtexture(gl_texture_2d, deadminotaur_texid);
		pictobj.draw(minotaur, vertbuff,uvbuff,elembuff);
	end if;


end draw_lab8;


