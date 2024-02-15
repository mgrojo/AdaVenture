
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



separate (avent)

procedure draw_exterior(chapter: integer) is
	dt: gldouble;
	yg, yy, xx,zz: float;

	frac : constant gldouble := 0.125; --fraction of normal duration

	yb1,yb2, yb3, yb4,yb5,yb6 : glfloat;

	near_success : boolean := false;

	xeye,yeye,zeye: float;

	--iti: integer;
	-- search iti to see how one might handle
	-- a sequence of texture files,
	-- eg. from a gif sequence.

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



-- 6feb18  changes:

-- chapter1
--		skyboxA= dark (most awesome)
--		darkA= 2
--		foglevA= 1 change to 0
--		fogclrA= 4 change to 0

--		skyboxB= sunny
--		darkB= 1
--		foglevB= 0 change to 1
--		fogclrB= 0 change to 1


-- chapter2
--		skyboxA= cloudy
--		darkA= 1
--		foglevA= 2
--		fogclrA= 1

--		skyboxB= moon
--		darkB= 2
--		foglevB= 0
--		fogclrB= 0

-- chapter3/4
--		foglev=3
--		fogclr=4


		-- begin draw common textured objects---------------------------

	near_success:=false;

	-- lev: 0=>none, 1=>normal, 2=>dense, 3=>extreme fog
	-- clr: 0=>true color, 1=>white, 2=>brown, 3=>purple, 4=>gray
	if chapter=1 then
		extdarkness:=2; --2;--dark, cloudy
		extfoglev:=0; --0; --fog obscures awesome skybox
		extfogclr:=1; --1;
		if schalice=1 or schalice=2 or (scene=1 and chaliceheld) then
			near_success:=true;
			extdarkness:=1; --sunny (flag for near-success)
			extfoglev:=0; --1; --fog would obscure awesome skybox
			extfogclr:=0; --1;
		end if;

	elsif chapter=2 then
		extdarkness:=1; -- bright, cloudy
		extfoglev:=2;
		extfogclr:=1;
		if schalice=1 or schalice=2 or (scene=1 and chaliceheld) then
			near_success:=true;
			extdarkness:=2; --nighttime (flag for near-success)
			extfoglev:=0;
			extfogclr:=0;
		end if;

	elsif chapter=3 then
		extdarkness:=2; --2;--dark, cloudy
		extfoglev:=3; --0; --fog obscures awesome skybox
		extfogclr:=4; --1;
		if schalice=1 or schalice=2 or (scene=1 and chaliceheld) then
			near_success:=true;
			extdarkness:=1; --sunny (flag for near-success)
			extfoglev:=0; --1; --fog would obscure awesome skybox
			extfogclr:=0; --1;
		end if;


	elsif chapter=4 then
		extdarkness:=2; -- dark, cloudy
		extfoglev:=3;
		extfogclr:=4; --1;
		if schalice=1 or schalice=2 or (scene=1 and chaliceheld) then
			near_success:=true;
			extdarkness:=2; --nighttime (flag for near-success)
			extfoglev:=0;
			extfogclr:=0;
		end if;


	end if;






--////////////// draw cube-mapped skybox //////////////////////////////////

		glUseProgram(pidskyb01);
		glUniform1i(flevid01, extfoglev);
		glUniform1i(fcolid01, extfogclr);
		glUniform1i(mapid01, 0);
		glUniformMatrix4fv(mvpid01, 1, GL_FALSE, mmvp(1,1)'address);
		glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

		if chapter=1 or chapter=3 then
			if near_success then
				glBindTexture(GL_TEXTURE_CUBE_MAP, sunnycubemap_texid);
			else
				glBindTexture(GL_TEXTURE_CUBE_MAP, darkcubemap_texid);
			end if;
		else
			if near_success then
				glBindTexture(GL_TEXTURE_CUBE_MAP, mooncubemap_texid);
			else
				glBindTexture(GL_TEXTURE_CUBE_MAP, cloudycubemap_texid);
			end if;
		end if;

		cubemapobj.Draw(skybox, vertbuff,elembuff);









	-- draw terrain:
		glUseProgram(pidterra02);

		glUniform3f(eyeid02,glfloat(xeye),glfloat(yeye),glfloat(zeye));

		glUniform1i(sampid02, 0);


	--if chapter=2 and near_success then --draw jupiter
	if near_success then --draw jupiter
		glUniform1i(darkid02, 0);
		glUniform1i(flevid02, 0);
		glUniform1i(fcolid02, 0);
		glUniformMatrix4fv(mvpid02, 1, GL_FALSE, satmvp(1,1)'address);
		--glUniformMatrix4fv(mvidp, 1, GL_FALSE, satmv(1,1)'address);
		glBindTexture(GL_TEXTURE_2D, jup_texid);
		ellobj.draw(jupit,vertbuff,uvbuff,elembuff); --EquiRect
	end if;



	glUniform1i(darkid02, extdarkness);
	glUniform1i(flevid02, extfoglev);
	glUniform1i(fcolid02, extfogclr);
	glUniformMatrix4fv(mvpid02, 1, GL_FALSE, imvp(1,1)'address);

	-- now choose a texture...
	if chapter=1 or chapter=3 then
		if near_success then
			glBindTexture(GL_TEXTURE_2D, grass_texid); -- lush, greener
		else
			glBindTexture(GL_TEXTURE_2D, grass1_texid); -- more undeveloped
			--glBindTexture(GL_TEXTURE_2D, sand1_texid); --too harsh
		end if;
	else
		if near_success then
			glBindTexture(GL_TEXTURE_2D, grass_texid); -- lush, greener
		else
			glBindTexture(GL_TEXTURE_2D, grass1_texid); -- more undeveloped
		end if;
	end if;
	terrain.Draw(grounds,vertbuff,uvbuff,elembuff);





		--castle slotted walls
		glbindtexture(gl_texture_2d, slot_texid);
		for i in 8..11 loop
			rectobj.draw(wallblok(i), vertbuff,uvbuff,elembuff);
		end loop;


		--castle walls
		glbindtexture(gl_texture_2d, stone_texid);
		for i in 4..19 loop
		if (i<8 or i>11) then
			rectobj.draw(wallblok(i), vertbuff,uvbuff,elembuff);
		end if;
		end loop;



		-- eXterior maze walls
		--glbindtexture(gl_texture_2d, mazeouter_texid);--orig
		glbindtexture(gl_texture_2d, mazex_texid);--interesting
		rectobj.draw(wallxp, vertbuff,uvbuff,elembuff);
		rectobj.draw(wallzp, vertbuff,uvbuff,elembuff);
		------------------------------------------------
		glbindtexture(gl_texture_2d, mazext_texid);
		--glbindtexture(gl_texture_2d, greenrockwall_texid);
		rectobj.draw(wallLf, vertbuff,uvbuff,elembuff);
		rectobj.draw(wallBt, vertbuff,uvbuff,elembuff);
		rectobj.draw(wallRt, vertbuff,uvbuff,elembuff);
		rectobj.draw(wallTp, vertbuff,uvbuff,elembuff); --26dec22 add

		-- maze door
		glbindtexture(gl_texture_2d, doortw_texid);
		pictobj.draw(omazedoor, vertbuff,uvbuff,elembuff);



		-- castle entry gates

		if zmup and not gateopen then -- ZM gate rising

			elapsed := currenttime-lifttime;
			tt := elapsed/liftduration;
			if tt>1.0 then 
				gatewait:=false; zmup:=false; tt:=1.0; gateopen:=true;
			end if;
			yg := 1.1*float(tt) + 0.4*(1.0-float(tt));
			pictobj.setrect( 
				gatezm, 
				xgzm, yg, zgzm, --xc,yc,zc
				0.4, 0.4, 0.0, --xr,yr,zr
				koxlo(gzmk),koxhi(gzmk),koylo(gzmk),
				koyhi(gzmk),kozlo(gzmk),kozhi(gzmk) );

		end if;



		if mazegoingup and not mazeopen then -- Z- gate rising

			elapsed := currenttime-mazetime;
			tt := elapsed/liftduration;
			yy := ymaze+float(tt)*0.6;
			if tt>1.0 then 
				mazewait:=false;
				mazeopen:=true;
				mazegoingup:=false; 
				tt:=1.0; 
			end if;

			pictobj.setrect( 
				gatem1, 
				xmaze, yy, zmaze-0.3, --xc,yc,zc
				0.3, 0.3, 0.01, --xr,yr,zr
				koxlo(gk1),koxhi(gk1),koylo(gk1),
				koyhi(gk1),kozlo(gk1),kozhi(gk1) );

		end if;






		glbindtexture(gl_texture_2d, gate_texid);

			--castle gate:
			pictobj.draw(gatezm, vertbuff,uvbuff,elembuff);

			--maze gates:
			pictobj.draw(gatem1, vertbuff,uvbuff,elembuff);
			pictobj.draw(gatem2, vertbuff,uvbuff,elembuff);


		--Zoroaster Symbol
		glbindtexture(gl_texture_2d, zoro_texid);
			pictobj.draw(zoro, vertbuff,uvbuff,elembuff);







		--still using program shaders for pidterra

		if not gkeyheld and sgkey=1  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;


		if not bkeyheld and sbkey=1 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
		end if;

		if not wkeyheld and swkey=1 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=1 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
		end if;

		if not gateheld and sgate=1  and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;



		-- draw other normal rectobj or pictobj here






---------------- begin bat insert ------------------

		if batfly and scene=1 and bat9sent then --final cup-grab

			dt:=(-1.0+(currenttime-batstart)/(frac*batduration)); 
			-- -1..+1

			if dt>0.0 and not batested9 then
				batested9:=true;

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
					if chapter=2 then
						xx:=+3.0; -- -xmaxu+1.0;
						zz:=+zmaxu-1.0;
					else
						xx:=-11.4;
						zz:=-2.0;
					end if;

					xchalice:=(xx);
					zchalice:=(zz);
					ychalice:=hcup+land_alt(xx,zz);
					schalice:=1;
					bathaschalice:=false;
					snd4ada.playSnd(down);
				end if;
			else --draw bat
				drawbat(dt);
				null;
			end if;


		elsif batfly and scene=1 then --draw flying bat as per gun-beam
			dt:=-0.5+1.5*(currenttime-batstart)/batduration; -- -0.5..+1.0

			if dt>0.0 and not batested1 then
				batested1:=true;

				if not bathaswkey and not wkeyheld then 
					bathaswkey:=true;
					snd4ada.playSnd(up);
					-- this flag being false indicates the bat
					-- succeeded in grabbing key from the ground!
				end if;

			end if;

			if dt>=1.0 then
				batfly:=false;
				if not wkeyheld and bathaswkey then
					--bat drops key onto ground
					xwkey:=xbat;
					zwkey:=zbat;
					ywkey:=htobj+land_alt(xwkey,zwkey);

					pictobj.setrect( 
						key1, 
						xwkey,ywkey,zwkey, --xc,yc,zc
						0.1, 0.0, 0.1, --xr,yr,zr
						j1,j2,j3,j4,j5,j6);

					bathaswkey:=false;
					snd4ada.playSnd(down);
				end if;
			else --draw bat
				drawbat(dt);
			end if;

		end if; --batfly

---------------- end bat insert ------------------




		if drawchalice and schalice=1 
		and not chaliceheld and not bathaschalice
		then
		-- use uniforms to set position
			glUseProgram(pidcup06);

			gluniform1i(lFlagid06, 1 ); -- light effects
			glUniform1f(hangid06, 0.0);
			gluniform3f(lColrid06, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --dgold
			gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

			glUniform3f(eyeid06,glfloat(xeye),glfloat(yeye),glfloat(zeye));



			glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
			glUniform1i(sampid06, 0);
			glUniform1i(darkid06, 0); --extdarkness);
			glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
			glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

			gluniform1i(flevid06, 0); --extfoglev );
			gluniform1i(fcolid06, 0); --extfogclr ); 

			glbindtexture(gl_texture_2d, chalice_texid);
			xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
		end if;


		if scene=1 then
			showWhatIsHeld;
			end if;

		-- 6 tree locations:
		xt1c:=13.0;
		zt1c:=3.0;

		xt2c:=10.0;
		zt2c:=3.0;

		xt3c:=4.0; --7.0;
		zt3c:=4.0; --3.0;


		xt4c:=3.0;
		zt4c:=13.0;

		xt5c:=3.0;
		zt5c:=10.0;

		xt6c:=3.0; 
		zt6c:=7.0;



		-- 6 tree sizes, vertical positions
		xt1r:=1.3;
		zt1r:=1.3;
		yt1r:=2.0;
		yt1c:=yt1r+glfloat(land_alt(float(xt1c),float(zt1c)));
		yb1:=yt1c-yt1r;

		xt2r:=1.4;
		zt2r:=1.4;
		yt2r:=1.0;
		yt2c:=yt2r+glfloat(land_alt(float(xt2c),float(zt2c)));
		yb2:=yt2c-yt2r;

		xt3r:=1.4;
		zt3r:=1.4;
		yt3r:=1.0;
		yt3c:=yt3r+glfloat(land_alt(float(xt3c),float(zt3c)));
		yb3:=yt3c-yt3r;



		xt4r:=1.3;
		zt4r:=1.3;
		yt4r:=2.0;
		yt4c:=yt4r+glfloat(land_alt(float(xt4c),float(zt4c)));
		yb4:=yt4c-yt4r;

		xt5r:=1.4;
		zt5r:=1.4;
		yt5r:=1.0;
		yt5c:=yt5r+glfloat(land_alt(float(xt5c),float(zt5c)));
		yb5:=yt5c-yt5r;

		xt6r:=1.4;
		zt6r:=1.4;
		yt6r:=1.0;
		yt6c:=-0.1+yt6r+glfloat(land_alt(float(xt6c),float(zt6c)));
		yb6:=yt6c-yt6r;










		--draw trees

		glUseProgram(pidtree08);

		glUniform3f(eyeid08,glfloat(xeye),glfloat(yeye),glfloat(zeye));


		glUniformMatrix4fv(mvpid08, 1, GL_FALSE, imvp(1,1)'address);


		glUniform1i(flevid08, extfoglev); --19dec17
		glUniform1i(fcolid08, extfogclr); --19dec17
		glUniform1i(darkid08, extdarkness);

		glUniform1i(sampid08, 0);
		glUniform1f(timeid08, glfloat(currentTime) ); 
		--shader makes them sway

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

		----------------------------------------------------#1
		glUniform3f(cenid08, xt1c,yt1c,zt1c );
		glUniform3f(radid08, xt1r,yt1r,zt1r );
		glUniform1f( baseid08, yb1 );

		glUniform1i(trid08, glint(1) );
		glBindTexture(GL_TEXTURE_2D, tree1_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff, 
			xeye-float(xt1c),zeye-float(zt1c));

		----------------------------------------------------#2
		glUniform3f(cenid08, xt2c,yt2c,zt2c );
		glUniform3f(radid08, xt2r,yt2r,zt2r );
		glUniform1f( baseid08, yb2 );

		glUniform1i(trid08, glint(2) );

		--iti := integer(float'rounding(10.0*currentTime));
		--iti := iti mod 2;
		--glBindTexture(GL_TEXTURE_2D, atree2_texid(iti) );

		glBindTexture(GL_TEXTURE_2D, tree2_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt2c),zeye-float(zt2c));

		----------------------------------------------------#3
--		glUniform3f(cenid08, xt3c,yt3c,zt3c );
--		glUniform3f(radid08, xt3r,yt3r,zt3r );
--		glUniform1f(baseid08, yb3 );

--		glUniform1i(trid08, glint(3) );
--		glBindTexture(GL_TEXTURE_2D, tree3_texid);
--		w3treeobj.Draw( 
--			tree, vertbuff,uvbuff,elembuff,
--			xeye-float(xt3c),zeye-float(zt3c));

		----------------------------------------------------#4
		glUniform3f(cenid08, xt4c,yt4c,zt4c );
		glUniform3f(radid08, xt4r,yt4r,zt4r );
		glUniform1f(baseid08, yb4 );

		glUniform1i(trid08, glint(4) );
		glBindTexture(GL_TEXTURE_2D, tree4_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt4c),zeye-float(zt4c));

		----------------------------------------------------#5
		glUniform3f(cenid08, xt5c,yt5c,zt5c );
		glUniform3f(radid08, xt5r,yt5r,zt5r );
		glUniform1f(baseid08, yb5 );

		glUniform1i(trid08, glint(5) );
		glBindTexture(GL_TEXTURE_2D, tree5_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt5c),zeye-float(zt5c));

		----------------------------------------------------#6
		glUniform3f(cenid08, xt6c,yt6c,zt6c );
		glUniform3f(radid08, xt6r,yt6r,zt6r );
		glUniform1f(baseid08, yb6 );

		glUniform1i(trid08, glint(6) );
		--glBindTexture(GL_TEXTURE_2D, spalm_texid);
		glBindTexture(GL_TEXTURE_2D, tree3_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt6c),zeye-float(zt6c));

		----------------------------------------------------

-- 26dec22 add 3 palms @ corners of maze area

		----------------------------------------------------#7
		glUniform3f(cenid08, 4.9, yt6c, 4.7 );
		glUniform3f(radid08, xt6r,yt6r,zt6r );
		glUniform1f(baseid08, yb6 );

		glUniform1i(trid08, glint(7) );
		glBindTexture(GL_TEXTURE_2D, spalm_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt6c),zeye-float(zt6c));

		----------------------------------------------------


		----------------------------------------------------#8
		glUniform3f(cenid08, 19.0, yt6c-0.1, 4.8 );
		glUniform3f(radid08, xt6r,yt6r,zt6r );
		glUniform1f(baseid08, yb6 );

		glUniform1i(trid08, glint(8) );
		glBindTexture(GL_TEXTURE_2D, spalm_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt6c),zeye-float(zt6c));

		----------------------------------------------------


		----------------------------------------------------#9
		glUniform3f(cenid08, 4.9, yt6c-0.05, 18.8 );
		glUniform3f(radid08, xt6r,yt6r,zt6r );
		glUniform1f(baseid08, yb6 );

		glUniform1i(trid08, glint(9) );
		glBindTexture(GL_TEXTURE_2D, spalm_texid);
		w3treeobj.Draw( 
			tree, vertbuff,uvbuff,elembuff,
			xeye-float(xt6c),zeye-float(zt6c));

		----------------------------------------------------





---------- end draw common textured objects----------------------------



-- update particle falls:
	myparticles.update(wfallp, float(wfyv), float(currenttime) );

-- update ribbon falls:
	myribbon.update(wfallr, float(wfyv), float(currenttime) );

-- draw falls:
	gluseprogram(pidwfall);

	glUniform3f(ieye43, glfloat(xeye),glfloat(yeye), glfloat(zeye) );
	glUniform1i(flev43, extfoglev); --fog-level 0..2=heavy
	glUniform1i(fcol43, extfogclr); --fog color 0=none, 1=white, 2=brownish, 4=gray
	glUniform1i(sprts43, 0);
	glUniform1i(dark43, extdarkness); --darkness 0..4

	gluniformmatrix4fv(mvp43,1,gl_false,imvp(1,1)'address);
	gluniform1f(time43, glfloat(currenttime));


	--these next 2 merely help to set the fog properly (not to reposition)
	glUniform3f(icen36, wfxc, wfyc, wfzc ); --xyz-centroid


	gluniform1i(flag43,0); --signals ribbon
	glbindtexture(gl_texture_2d, wfall_texid);
	myribbon.draw(wfallr, vertbuff, uvbuff, elembuff);

	gluniform1i(flag43,1); --signals particles
	glbindtexture(gl_texture_2d, white_texid);
	myparticles.draw(wfallp, vertbuff, uvbuff, elembuff);


-- draw cistern:

	glUseProgram(pidrock36);
	glUniform3f(ieye36, glfloat(xeye),glfloat(yeye), glfloat(zeye) );

	glUniform1i(flev36, extfoglev); --fog-level 0..2=heavy
	glUniform1i(fcol36, extfogclr); --fog color 0=none, 1=white, 2=brownish, 4=gray
	glUniform1i(dark36, extdarkness); --darkness 0..4

	glUniform1i(samp36, 0);
	glUniformMatrix4fv(imvp36, 1, GL_FALSE, imvp(1,1)'address);

	--these next 2 merely help to set the fog properly (not to reposition)
	glUniform3f(icen36, wfxc, wfyc-wfyr, glfloat(ciszc+cisrr) ); --xyz-centroid

	glBindTexture(GL_TEXTURE_2D, cistern_texid);
	cyl2texobj.draw(rococo,vertbuff,uvbuff,elembuff);




end draw_exterior;


