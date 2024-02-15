
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

procedure draw_castle is

	i,k: integer;

	xeye,yeye,zeye: float;


procedure drawarches(k: integer) is
begin

	glUseProgram(pidzpm34a);

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, albedo34a);

	glActiveTexture(GL_TEXTURE1);
	glBindTexture(GL_TEXTURE_2D, normal34a);

	glActiveTexture(GL_TEXTURE2);
	glBindTexture(GL_TEXTURE_2D, metallic34a);

	glActiveTexture(GL_TEXTURE3);
	glBindTexture(GL_TEXTURE_2D, roughness34a);

	--glActiveTexture(GL_TEXTURE4);
	--glBindTexture(GL_TEXTURE_2D, ao34a);

	gluniformmatrix4fv( proj34a, 1, gl_false, pm(1,1)'address ); --projMat
	gluniformmatrix4fv( view34a, 1, gl_false, imv(1,1)'address ); --viewMat
	gluniformmatrix4fv( modl34a, 1, gl_false, mm(1,1)'address ); --modelMat=Id


	gluniform1i(albm34a,0);
	gluniform1i(nrmm34a,1);
	gluniform1i(metm34a,2);
	gluniform1i(rufm34a,3);
	--gluniform1i(aom34a,4);


	if success then

		--gluniform3f(pColr34a, 250.0, 140.0, 0.0 ); --light-color=gold (chalicemode)
		--try this brighter version instead (4sep23...better):
		gluniform3f(pColr34a, 255.0, 200.0, 128.0 ); --very light-gold (chalicemode)

		--gold light source #1 from vicinity of chalice:
		gluniform3f(pos0_34a, glfloat(xchalice),glfloat(ychalice)+0.0,glfloat(zchalice) );


		-----------------------------------------------------------------------------
		-- pool center @ (x2c-x2r/2.0,0,z2c)...water reflections...

		--lightPos #2 from underwater towards -z = away from tapestry
		gluniform3f(pos1_34a, glfloat(x2c-x2r/2.0),glfloat(-1.0),glfloat(z2c-5.0) );

		--lightPos #3 from underwater towards +z = toward tapestry
		gluniform3f(pos2_34a, glfloat(x2c-x2r/2.0),glfloat(-1.0),glfloat(z2c+5.0) );
		-----------------------------------------------------------------------------

	else
		gluniform3f(pColr34a, 50.0, 50.0, 50.0 ); --light-Color=dim_white (normalmode)

		--lightPos #1 @ doorway:
		gluniform3f(pos0_34a, glfloat(x2c),glfloat(y2c),glfloat(z2c-5.0) );

		-----------------------------------------------------------------------------
		-- pool center @ (x2c-x2r/2.0,0,z2c)...water reflections...

		--lightPos #2 from underwater towards -x = away from alcove
		gluniform3f(pos1_34a, glfloat(x2c-x2r/2.0-5.0),glfloat(-1.0),glfloat(z2c) );

		--lightPos #3 from underwater towards +z = toward tapestry
		gluniform3f(pos2_34a, glfloat(x2c-x2r/2.0),glfloat(-1.0),glfloat(z2c+5.0) );
		-----------------------------------------------------------------------------

	end if;



--note:  need to use camera Pos in 3rd person mode:
	gluniform3f(CamPos34a, 
		glfloat(xeye), glfloat(yeye), glfloat(zeye));



	glUniform1f(phrad34a, glfloat(rr) );

	-- sort the arched "cornices" = rectangles with holes
	for ii in 1..4 loop
		ox(ii):=hox(ii,k);
		oz(ii):=hoz(ii,k);
	end loop;
	sort(cf2n,ox,oz,1,4,xme,zme);

	--now draw far to near
	for ii in 1..4 loop
		i:=cf2n(ii);
		glUniform3f(
			phole34a,
			glfloat(hox(i,k)),
			glfloat(hoy(i,k)),
			glfloat(hoz(i,k)) );
		rectobj.ldraw(cornice(i,k), vertbuff,uvbuff,normbuff,elembuff);
	end loop;

	glActiveTexture(GL_TEXTURE0);


end drawarches;







-- drawn ONLY is success (chalice is restored):
procedure drawAlcove is
begin

	glUseProgram(pidzpm34a);

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, albedo34b);

	glActiveTexture(GL_TEXTURE1);
	glBindTexture(GL_TEXTURE_2D, normal34b);

	glActiveTexture(GL_TEXTURE2);
	glBindTexture(GL_TEXTURE_2D, metallic34b);

	glActiveTexture(GL_TEXTURE3);
	glBindTexture(GL_TEXTURE_2D, roughness34b);

	gluniformmatrix4fv( proj34a, 1, gl_false, pm(1,1)'address ); --projMat
	gluniformmatrix4fv( view34a, 1, gl_false, imv(1,1)'address ); --viewMat
	gluniformmatrix4fv( modl34a, 1, gl_false, mm(1,1)'address ); --modelMat=Id


	gluniform1i(albm34a,0);
	gluniform1i(nrmm34a,1);
	gluniform1i(metm34a,2);
	gluniform1i(rufm34a,3);


	gluniform3f(pColr34a, 250.0, 140.0, 0.0 ); --light-color=gold (chalicemode)

	--gold light source #1 from vicinity of chalice:
	gluniform3f(pos0_34a, glfloat(xchalice),glfloat(ychalice+0.0),glfloat(zchalice) );
	gluniform3f(pos1_34a, glfloat(xchalice),glfloat(ychalice+0.0),glfloat(zchalice) );
	gluniform3f(pos2_34a, glfloat(xchalice),glfloat(ychalice+0.0),glfloat(zchalice) );





--note:  need to use camera Pos in 3rd person mode:
	gluniform3f(CamPos34a, 
		glfloat(xeye), glfloat(yeye), glfloat(zeye));


	glUniform1f(phrad34a, 0.0 ); --no hole removal here

	pictobj.ldraw(wallmoor, vertbuff,uvbuff,normbuff,elembuff);


	glActiveTexture(GL_TEXTURE0);


end drawAlcove;
















	angl: float;

	kbeet: integer := 0;


begin
	if thirdPerson then
		xeye:=xcam; yeye:=ycam; zeye:=zcam;
	else
		xeye:=xme; yeye:=yme; zeye:=zme;
	end if;



		glUseProgram( pidtex05 );

		gluniform1i(flevid05,0);
		gluniform1i(fcolid05,0);

gluniform1i(eawe05,0);
gluniform1i(noso05,0);



		gluniformmatrix4fv( mvpid05, 1, gl_false, imvp(1,1)'address );

		if success then
			gluniform1i(lflagid05,1); -- 1=>use light effects
			--gluniform3f(lColrid05, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
			gluniform3f(lColrid05, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --gold
			glUniform1i(darkid05, castledarkness); -- was -1
			gluniform1f(lspcid05, 0.5); --fracMspc (dflt=0.1)
		else
			gluniform1i(lflagid05,0); -- 0=>NO light effects
			gluniform3f(lColrid05, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --dgold
			glUniform1i(darkid05, castledarkness);
		end if;
		gluniform3f(lposid05, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid05, glfloat(xeye),glfloat(yeye),glfloat(zeye) );


		gluniform1i(sampid05,0);



		if not success then
			--moorish alcove (normal)
			glbindtexture(gl_texture_2d, moorwall_texid);
			pictobj.ldraw(wallmoor, vertbuff,uvbuff,normbuff,elembuff);
		--else
		--	glbindtexture(gl_texture_2d, moorwall_texid);
		--	pictobj.draw(wallmoor, vertbuff,uvbuff,elembuff);
		end if;


		--floor
		glbindtexture(gl_texture_2d, floor_texid);
		rectobj.ldraw(floor, vertbuff,uvbuff,normbuff,elembuff);

		--pedestal
		glbindtexture(gl_texture_2d, cherry_texid);
		pictobj.ldraw(pedestal, vertbuff,uvbuff,normbuff,elembuff);

		--tapestry
		glbindtexture(gl_texture_2d, rug_texid);
		pictobj.draw(rug, vertbuff,uvbuff,elembuff); --no shine on rug

		--persianArt
		glbindtexture(gl_texture_2d, art_texid);
		pictobj.draw(wallpicso, vertbuff,uvbuff,elembuff); --7feb24

		--guest mats 9feb24
		glbindtexture(gl_texture_2d, mat_texid);
		pictobj.draw(mat1, vertbuff,uvbuff,elembuff);
		pictobj.draw(mat2, vertbuff,uvbuff,elembuff);
		pictobj.draw(mat3, vertbuff,uvbuff,elembuff);
		pictobj.draw(mat4, vertbuff,uvbuff,elembuff);
		pictobj.draw(mat5, vertbuff,uvbuff,elembuff);



		if not gkeyheld and sgkey=2  then
			glbindtexture(gl_texture_2d, gkey_texid);
			pictobj.draw(key3, vertbuff,uvbuff,elembuff);
		end if;

		if not bkeyheld and sbkey=2 and not bathasbkey then
			glbindtexture(gl_texture_2d, bkey_texid);
			pictobj.draw(key2, vertbuff,uvbuff,elembuff);
		end if;

		if not wkeyheld and swkey=2 and not bathaswkey then
			glbindtexture(gl_texture_2d, key_texid);
			pictobj.draw(key1, vertbuff,uvbuff,elembuff);
		end if;

		if not swordheld and ssword=2 then
			glbindtexture(gl_texture_2d, sword_texid);
			pictobj.draw(sword, vertbuff,uvbuff,elembuff);
		end if;

		if not gateheld and sgate=2 and kgate=0 then
			glbindtexture(gl_texture_2d, frame_texid);
			pictobj.draw(gateway, vertbuff,uvbuff,elembuff);
		end if;




		glUseProgram( pidhole09 );
		gluniformmatrix4fv( mvpid09, 1, gl_false, imvp(1,1)'address );
		gluniform1i(sampid09,0);

		if success then
			gluniform1i(lFlagid09,1); -- 1=>use light effects
			gluniform3f(lColrid09, 246.0/255.0, 216.0/255.0, 11.0/255.0 ); --gold
			glUniform1i(darkid09, castledarkness); -- was -1
			gluniform1f(lspcid09, 0.5); --fracMspc (dflt=0.1)
		else
			gluniform1i(lFlagid09,0); -- 0=>NO light effects
			gluniform3f(lColrid09, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --dgold
			glUniform1i(darkid09, castledarkness);
		end if;

		gluniform3f(lPosid09, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid09, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		glUniform1f(radid09, 0.0 );
		glbindtexture(gl_texture_2d, adobe_texid); --13oct16
		cylobj.draw(iskylight, vertbuff,uvbuff,elembuff);


-------------------------------------------------------------------
		--new frame as of 24nov19:
		glbindtexture(gl_texture_2d, sgate_texid);
		pictobj.draw(roq,vertbuff,uvbuff,elembuff);
-------------------------------------------------------------------




---------------------- begin room & ceiling -----------------------------
		-- we now draw room & ceiling here to draw
		-- a hole in the ceiling center:
		glUniform1f(radid09, 0.5 );
		glUniform3f(cenid09, -5.0, glfloat(iymax), -5.0 );

		--main interior
		glbindtexture(gl_texture_2d, adobe_texid);
		droomobj.ldraw(cdo,vertbuff,uvbuff,normbuff,elembuff); --textured room

		--ceiling
		glbindtexture(gl_texture_2d, ceil_texid);
		pictobj.ldraw(ceil, vertbuff,uvbuff,normbuff,elembuff);

		--4 corner pillars
		--glbindtexture(gl_texture_2d, rosequartz_texid); --too colorful
		glbindtexture(gl_texture_2d, bmarble_texid);
		cylobj.draw(pillar11, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar12, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar13, vertbuff,uvbuff,elembuff);
		cylobj.draw(pillar14, vertbuff,uvbuff,elembuff);

---------------------- end room & ceiling -----------------------------

		glUniform1f(radid09, 0.0 ); --disable hole for other stuff



		-- I like the looks better without golden-discoloring
		-- of Korla, or the marble...
		gluniform1i(lFlagid09,0); -- 0=>NO light effects

		if success then
			angl:=float(currentTime)*onepi*0.1;

			--draw rotating Korla Pandit
			twictobj.setrect2(korla,
				-5.0, 1.0, -5.0,      -- xc,yc,zc
				0.2,  0.3, 0.2, angl, -- xr,yr,zr,angle
				j1,j2,j3,j4,j5,j6);

			if chapter<=2 then
				glbindtexture(gl_texture_2d, korla_texid);
			else
				glbindtexture(gl_texture_2d, kevin_texid);
			end if;
			twictobj.draw(korla, vertbuff,uvbuff,elembuff);
		end if;


		glbindtexture(gl_texture_2d, bmarble_texid);
		rectobj.ldraw(slab, vertbuff,uvbuff,normbuff,elembuff); --pool roof

		--4 pool edges
		rectobj.ldraw(edgezm, vertbuff,uvbuff,normbuff,elembuff);
		rectobj.ldraw(edgezp, vertbuff,uvbuff,normbuff,elembuff);
		rectobj.ldraw(edgexp, vertbuff,uvbuff,normbuff,elembuff);
		rectobj.ldraw(edgexm, vertbuff,uvbuff,normbuff,elembuff);

--------- begin pillar with arched copper brackets ----------------------------------

		for ii in 0..4 loop
			ox(ii):=mpx(ii);
			oz(ii):=mpz(ii);
		end loop;
		sort( pfar2near, ox, oz, 0,4, xme,zme );

		glbindtexture(gl_texture_2d, bmarble_texid);
		for i in 0..4 loop
			k:=pfar2near(i);
			cylobj.draw(moorishpillar(k), vertbuff,uvbuff,elembuff); -------LOOKHERE-----
		end loop;

		-- the only way I've found to make these arches
		-- look correct is to draw twice:

		-- draw arches:
		gldepthmask(gl_false);
		for ii in 0..4 loop
			i:=pfar2near(ii);
			drawarches(i);
		end loop;
		gldepthmask(gl_true);
		for ii in 0..4 loop
			i:=pfar2near(ii);
			drawarches(i);
		end loop;

--------- end pillar with arched copper brackets ------------------------------------


-------- begin reflective pool -------------------------------

	glUseProgram( pidpool15 ); --=================================
	glUniform3f(ieye15, glfloat(xeye),glfloat(yeye),glfloat(zeye));
	glenable(gl_texture_cube_map_seamless);
	glbindtexture(gl_texture_cube_map, cubemap_texid);
	gluniform1i(envm15, 0); -- "envMap" in .fs
	gluniformmatrix4fv( mvpid15, 1, gl_false, imvp(1,1)'address );
	gluniform1f(time15, glfloat(currentTime) );
	gluniform3f(icen15,glfloat(mpx(0)), 0.1, glfloat(mpz(0)));
	gluniform3f(irad15, 1.9, 0.1, 1.9);
	gluniform1f(wlev15, 0.15 ); --??waterlevel
	reflsurf.draw(rfo,vertbuff);

-------- end reflective pool -------------------------------











	if drawchalice and schalice=2 and not chaliceheld then
	-- use uniforms to set position
		glUseProgram(pidcup06);

		glUniformMatrix4fv(mvpid06, 1, GL_FALSE, imvp(1,1)'address);
		glUniform1i(sampid06, 0);
		gluniform1i(lFlagid06, 1 ); --light effects (always)
		glUniform1i(darkid06, 0); --castledarkness);
		if success then
			gluniform3f(lColrid06, 123.0/255.0, 108.0/255.0, 6.0/255.0 ); --dgold
		else
			gluniform3f(lColrid06, 30.0/255.0, 27.0/255.0, 2.0/255.0 ); --dddgold
		end if;
		glUniform1f(hangid06, 0.0);

		gluniform3f(lPosid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );

		gluniform3f(eyeid06, glfloat(xeye),glfloat(yeye),glfloat(zeye) );



		glUniform3f(cenid06, glfloat(xchalice),glfloat(ychalice),glfloat(zchalice) );
		glUniform3f(radid06, glfloat(wcup), glfloat(hcup), glfloat(wcup) );

		gluniform1i(flevid06, 0); --foglev2 ); 
		gluniform1i(fcolid06, 0); --fogclr2 ); 

		glbindtexture(gl_texture_2d, chalice_texid);
		xtreeobj.ldraw(chalice, vertbuff,uvbuff,normbuff,elembuff);
	end if;



	if success then
		drawAlcove; --with lighting from chalice
	end if;



	if scene=2 then
		showWhatIsHeld;
	end if;

end draw_castle;


