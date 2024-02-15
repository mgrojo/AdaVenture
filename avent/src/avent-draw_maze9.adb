
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

procedure draw_maze9 is

	hhdoor : constant float := 0.9; --halfHeight
	hwdoor : constant float := 0.8;
	fhdoor : constant float := 1.8;

	frac : constant float := 0.125; --fraction of normal duration



-- not good enough...output indices dont necessarily
-- correspond to walls...may be spaces...
	procedure getNearestRowCol(
		xchalice,zchalice: glfloat; 
		rm,rp, cm,cp: out integer ) is
		rs,cs, r0,c0, r1,c1, rr,cc : integer;
	begin

		r0 := integer( glfloat'floor(xchalice) ); -- +8
		r1 := r0+1; -- +9
		c0 := integer( glfloat'floor(zchalice) ); -- -5
		c1 := c0+1; -- -4

		if not iswall(9,r0,c0) then
			rs:=r0;
			cs:=c0;
		elsif not iswall(9,r0,c1) then
			rs:=r0;
			cs:=c1;
		elsif not iswall(9,r1,c0) then
			rs:=r1;
			cs:=c0;
		elsif not iswall(9,r1,c1) then
			rs:=r1;
			cs:=c1;
		else
			raise program_error;
		end if;


		rr:=rs; cc:=cs; -- +8,-5
		loop
		 	exit when iswall(9,rr,cc);
			exit when rr=-mrows;
			rr:=rr-1;
		end loop;
		rm:=rr; -- +8(-5)

		rr:=rs; cc:=cs; -- +9,-4
		loop
		 	exit when iswall(9,rr,cc);
			exit when rr=mrows;
			rr:=rr+1;
		end loop;
		rp:=rr; -- +10(-4)



		rr:=rs; cc:=cs; -- +8,-5
		loop
		 	exit when iswall(9,rr,cc);
			exit when cc=-mcols;
			cc:=cc-1;
		end loop;
		cm:=cc; -- (+8)-5

		rr:=rs; cc:=cs; -- +9,-4
		loop
		 	exit when iswall(9,rr,cc);
			exit when cc=mcols;
			cc:=cc+1;
		end loop;
		cp:=cc; -- (+9)-2


	end getNearestRowCol;


	rm,rp,cm,cp: integer;


	xeye,yeye,zeye: float;
begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;

		--for normal textured objects:
		glUseProgram( pidtex05 );

xdc:=xdc9;
zdc:=zdc9;
--gluniform1i(ndc05,ndc9);
gluniform1i(eawe05,eawe);
gluniform1i(noso05,noso);
gluniform1fv(xdc05,ndc9,xdc(1)'address);
gluniform1fv(zdc05,ndc9,zdc(1)'address);

		--not Using Ldraw here:
		--gluniform1f(pgmdif, 0.1); --fracMdif low on walls
		--gluniform1f(pgmspc, 0.1); --fracMspc low on walls

		gluniform1i(lflagid05,0); -- 0=>NO light effects
		-- note that only some walls shall show the golden glow...

		gluniform3f(lColrid05, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
		gluniform3f(lPosid05, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );




		--gluniformmatrix4fv( mvid, 1, gl_false, imv(1,1)'address );
		gluniformmatrix4fv(mvpid05, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid05,0);
		glUniform1i(darkid05, mazedarkness9); --2);
		if chapter=2 then
		gluniform1i(flevid05, foglev9 ); 
		gluniform1i(fcolid05, fogclr9 ); 
		else
		gluniform1i(flevid05, 2 ); 
		gluniform1i(fcolid05, 4 ); 
		end if;






-------------------------------------------------------------------
		--new open-ceiling-frame as of 24nov19:
		glbindtexture(gl_texture_2d, skylite_texid);
		pictobj.draw(roz,vertbuff,uvbuff,elembuff);
-------------------------------------------------------------------






		--floor
		glbindtexture(gl_texture_2d, sand_texid); --sand
		rectobj.draw(mfloor, vertbuff,uvbuff,elembuff);


		glbindtexture(gl_texture_2d, lab_texid); --labyrinth sign
		pictobj.draw(lg9, vertbuff,uvbuff,elembuff);

		--maze entry door
		glbindtexture(gl_texture_2d, doort_texid);
		pictobj.draw(imaze9door, vertbuff,uvbuff,elembuff);



		-- transition doors
		pictobj.draw(al9, vertbuff,uvbuff,elembuff);
		pictobj.draw(ar9, vertbuff,uvbuff,elembuff);
		pictobj.draw(bl9, vertbuff,uvbuff,elembuff);
		pictobj.draw(br9, vertbuff,uvbuff,elembuff);

		pictobj.draw(cl9, vertbuff,uvbuff,elembuff);
		pictobj.draw(cr9, vertbuff,uvbuff,elembuff);
		pictobj.draw(dt9, vertbuff,uvbuff,elembuff);
		pictobj.draw(db9, vertbuff,uvbuff,elembuff);

		pictobj.draw(et9, vertbuff,uvbuff,elembuff);
		pictobj.draw(eb9, vertbuff,uvbuff,elembuff);
		pictobj.draw(fl9, vertbuff,uvbuff,elembuff);
		pictobj.draw(fr9, vertbuff,uvbuff,elembuff);


		-- draw entire maze, first without lighing:

		--maze walls
		glbindtexture(gl_texture_2d, mazeouter9_texid); --leafy
		for row in -mrows..mrows loop
		for col in -mcols..mcols loop
		if 
			iswall(9,row,col) and
			(kgate=0 or sgate/=9 or rgate/=row or cgate/=col)
		then
			pictobj.draw(mzwall(9,row,col), vertbuff,uvbuff,elembuff);
			--pictobj.ldraw(mzwall(9,row,col), vertbuff,uvbuff,normbuff,elembuff);
		end if;
		end loop;
		end loop;

		if kgate>0 and sgate=9 then
			glbindtexture(gl_texture_2d, frame_texid); --passthru
			pictobj.draw(mzwall(9,rgate,cgate), vertbuff,uvbuff,elembuff);
			--pictobj.ldraw(mzwall(9,rgate,cgate), vertbuff,uvbuff,normbuff,elembuff);
		end if;



---------------------------------------------------------------

		if not gkeyheld and sgkey=9  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=9 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
		end if;

		if not wkeyheld and swkey=9 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=9 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
			pictobj.draw(sword0, vertbuff,uvbuff,elembuff);--ghostSword
		end if;

		if not gateheld and sgate=9  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;





---------------------------------------------------------------

-- draw lighting on maze walls closest to chalice to show glow:
		if not chaliceheld and schalice=9 then
			gluniform1i(lflagid05,1); -- 1=>use light effects
			glUniform1i(darkid05, mazedarkness9); --2);

		-- (low shine on bushes)
		--gluniform1f(pgmdif, 0.1); --fracMdif
		--gluniform1f(pgmspc, 0.01); --fracMspc

		else
			gluniform1i(lFlagid05,0); -- 1=>use light effects
			glUniform1i(darkid05, mazedarkness9); --3);
		end if;
		gluniform3f(lColrid05, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold


		getNearestRowCol( glfloat(xchalice),glfloat(zchalice), rm,rp, cm,cp );

		--maze walls nearest glowing chalice:
		glbindtexture(gl_texture_2d, mazeouter9_texid); --leafy

		-- col-slice of 2 nearest rows:
		for row in -mrows..mrows loop
			if row=rm or row=rp then
				for col in cm..cp loop
				if
					iswall(9,row,col) and
					(kgate=0 or sgate/=9 or rgate/=row or cgate/=col)
				then
					pictobj.ldraw(mzwall(9,row,col), vertbuff,uvbuff,normbuff,elembuff);
				end if;
				end loop;
			end if;
		end loop;

		-- row-slice of 2 nearest columns:
		for row in rm..rp loop
			for col in -mcols..mcols loop
				if (col=cm or col=cp) and
					iswall(9,row,col) and
					(kgate=0 or sgate/=9 or rgate/=row or cgate/=col)
				then
					pictobj.ldraw(mzwall(9,row,col), vertbuff,uvbuff,normbuff,elembuff);
				end if;
			end loop;
		end loop;




-- draw little hidden area with chalice with lighting here
-- because its only these walls show the full glow of chalice...
		if not chaliceheld and schalice=9 then
			gluniform1i(lflagid05,1); -- 1=>use light effects
			--if chapter=4 then
			--glUniform1i(darkid05, 3);
			--else
			glUniform1i(darkid05, mazedarkness9); -- 4
			--end if;
		else
			gluniform1i(lflagid05,0); -- 0=>NO light effects
			glUniform1i(darkid05, mazedarkness9); -- -1);
		end if;

		gluniform3f(lColrid05, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
		gluniform3f(lPosid05, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );

		--if chapter=4 then
		--gluniform1i(flevid05, 1 ); 
		--end if;

		--low shine on walls
		--gluniform1f(ldifid05, 0.1); --fracMdif
		--gluniform1f(lspcid05, 0.01); --fracMspc

		glbindtexture(gl_texture_2d, gleaves_texid); --greenleaves
		droomobj.ldraw(mdo9,vertbuff,uvbuff,normbuff,elembuff); --textured room
		--droomobj.draw(mdo9,vertbuff,uvbuff,elembuff); --textured room

		--gluniform1i(flevid05, 3 ); 


		-- draw chalice itself:
		if drawchalice and schalice=9 and not chaliceheld and not bathaschalice then
		-- use uniforms to set position
			glUseProgram(pidcup06);

			gluniform1i(lFlagid06,1); -- 1=>use light effects
			glUniform1f(hangid06, 0.0);
			--if chapter=4 then
			--gluniform3f(lColrid06, 246.0/255.0, 216.0/255.0, 12.0/255.0 ); --gold
			--else
			gluniform3f(lColrid06, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --dgold
			--end if;
			gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

			gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );




			glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
			glUniform1i(sampid06, 0);
			glUniform1i(darkid06, 0);
			glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
			glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

			gluniform1i(flevid06, 0); --foglev9 ); 
			gluniform1i(fcolid06, 0); --fogclr9 ); 

			glbindtexture(gl_texture_2d, chalice_texid);
			--xtreeobj.draw(chalice, vertbuff,uvbuff,elembuff);
			xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
		end if;


	if scene=9 then
		showWhatIsHeld;
	end if;

	if scene=9 then --(ie, dont draw if looking in from lab8)
		-- fancy fragshader draws cloud ceiling
		glUseProgram( pidstar04 );
		gluniformmatrix4fv( mvpid04, 1, gl_false, imvp(1,1)'address );
		gluniform1f(timeid04, glfloat(currentTime) );
		gluniform2f(resid04, glfloat(winwidth), glfloat(winheight) );
		rectxobj.draw(rox,vertbuff,elembuff);
	end if;




end draw_maze9;


