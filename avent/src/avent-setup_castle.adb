
--
-- Copy2right (C) 2024  <fastrgv@gmail.com>
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


procedure setup_castle is -- scene=2

	xm,xp,ym,yp,zm,zp, --discarded
	cy2c,cyh, rc,r2, wdoor,hdoor : float;

	-- -10 < x,z < 0
	--   0 <  y  < 3
	sx: constant float := x2r/2.0;
	sz: constant float := z2r/2.0;
	sy: constant float := y2r/2.0;

begin

	--Four corner pillars
	cylobj.setcyl( pillar11,
		0.0, y2c, 0.0,
--		-0.25, y2c, -0.25,
		y2r, 0.15,
		j1,j2,j3,j4,j5,j6);

	cylobj.setcyl( pillar12,
		-10.0, y2c, 0.0,
--		-9.75, y2c, -0.25,
		y2r, 0.15,
		j1,j2,j3,j4,j5,j6);

	cylobj.setcyl( pillar13,
		-10.0, y2c, -10.0,
--		-9.75, y2c, -9.75,
		y2r, 0.15,
		j1,j2,j3,j4,j5,j6);

	cylobj.setcyl( pillar14,
		0.0, y2c, -10.0,
--		-0.25, y2c, -9.75,
		y2r, 0.15,
		j1,j2,j3,j4,j5,j6);



	--walls
	droomobj.setroomwithZMdoor( cdo, 
		 --hw, fh
		 0.35, 0.9,
		 x2c, y2c, z2c,
		 x2r, y2r, z2r,
		 sx, sy, sz); --scaled for 16 mirrored copies each wall

	--ceiling
	pictobj.setrect(ceil, 
		--x2c,y2c+y2r-0.01,z2c, 
		x2c,y2c+y2r-0.3,z2c, 
		x2r,0.0,z2r, 
		j1,j2,j3,j4,j5,j6);


	--moorwall
	pictobj.setrect(wallmoor,
		x2c+x2r-0.01, y2c, z2c,
		0.01,       y2r, z2r*0.5, 
		j1,j2,j3,j4,j5,j6);

-----------------------------------
	--Korla Pandit
	--pictobj.setrect(korla,
	--	x2c-x2r+0.02, y2c, z2c+4.0,
	--	0.0,      0.6, 0.4,
	--	j1,j2,j3,j4,j5,j6);
-----------------------------------


	--floor
	nko:=nko+1;
	koscene(nko):=2;
	rectobj.setrect( 
		floor, 
		x2c, y2c-y2r+0.00, z2c,
		x2r,       0.01, z2r, --x2r,y2r,z2r
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
		--j1,j2,j3,j4,j5,j6);


	-- pedestal
	nko:=nko+1;
	koscene(nko):=2;
	xped:=(x2c+x2r-0.25);  zped:=(z2c);
	yped:=(y2c-y2r+0.5); --top, not center (base for chalice)
	pictobj.setrect( 
		pedestal, 
		float(xped), y2c-y2r+0.25, float(zped), --x2c,y2c,z2c
		0.25, 0.25, 0.25, --x2r,y2r,z2r
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );


	--zoroastrian art on SouthWall-west (xc,yc,zc)     (xr,yr,zr)
	pictobj.setrect(wallpicso, -2.5, 1.2, -10.0+0.1, 1.0,0.7,0.01, j1,j2,j3,j4,j5,j6);

	pictobj.setrect(mat1, -8.0, y2c-y2r+0.01, -9.5, 0.6, 0.01, 0.3, j1,j2,j3,j4,j5,j6);

	pictobj.setrect(mat2, -8.0, y2c-y2r+0.01, -0.5, 0.6, 0.01, 0.3, j1,j2,j3,j4,j5,j6);
	pictobj.setrect(mat3, -6.0, y2c-y2r+0.01, -0.5, 0.6, 0.01, 0.3, j1,j2,j3,j4,j5,j6);

	pictobj.setrect(mat4, -4.0, y2c-y2r+0.01, -0.5, 0.6, 0.01, 0.3, j1,j2,j3,j4,j5,j6);
	pictobj.setrect(mat5, -2.0, y2c-y2r+0.01, -0.5, 0.6, 0.01, 0.3, j1,j2,j3,j4,j5,j6);

	--tapestry
	pictobj.setrect( 
		rug, 
		x2c,  y2c-0.1, z2c+z2r-0.01, --x2c,y2c,z2c
		4.0, 0.8*y2r, 0.0, --x2r,y2r,z2r
		j1,j2,j3,j4,j5,j6);


	wdoor:=0.7;
	hdoor:=1.0;



-- initialize Sword
	xsword:=x2c+x2r-2.0;
	zsword:=z2c-1.0;
	ysword:=y2c-y2r+htobj;
	ssword:=2;
	pictobj.setrect( 
		sword, 
		xsword,ysword,zsword,
		rsword, 0.0, 0.2*rsword,
		j1,j2,j3,j4,j5,j6);



-- initialize Green Key

	xgkey:=x2c+x2r-2.0;
	zgkey:=z2c+1.0;
	ygkey:=y2c-y2r+htobj;
	sgkey:=2;

	pictobj.setrect( 
		key3, 
		xgkey,ygkey,zgkey, --x2c,y2c,z2c
		rkey, 0.0, 0.5*rkey, --x2r,y2r,z2r
		j1,j2,j3,j4,j5,j6);



---------- begin pillar with cornices ------------------

	rr:=1.0; --0.8; --arch radius increased 25mar21
	r2:=rr/2.0;

	rc:=0.10; --column radius
	cyh:=0.7;  --column half-height
	cy2c:=0.0+cyh; --column vertical centroid

	-- centroids of moorishpillars:
	mpx(1):=-9.0; mpz(1):=-4.0; --draw 1st
	mpx(2):=-9.0; mpz(2):=-6.0; --probably further

	mpx(3):=-7.0; mpz(3):=-4.0; --draw last
	mpx(4):=-7.0; mpz(4):=-6.0; --probably closer

	mpx(0):=-8.0; mpz(0):=-5.0;

	-- interior marble roof to pillars
	rectobj.setrect(slab,
		-8.0, cy2c+cyh+0.05, -5.0,
		 2.1,         0.05,  2.1, --match larger arch radius 25mar21
--		 1.9,         0.05,  1.9,
		 j1,j2,j3,j4,j5,j6 );




-- pool edges begin ---------------------------------

	nko:=nko+1;
	koscene(nko):=2;
	rectobj.setrect(edgezm,
		-7.95, 0.12, -7.0,
		 2.05, 0.12,  0.1,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
		koyhi(nko):=3.0;

	nko:=nko+1;
	koscene(nko):=2;
	rectobj.setrect(edgezp,
		-7.95, 0.12, -3.0,
		 2.05, 0.12,  0.1,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
		koyhi(nko):=3.0;

	nko:=nko+1;
	koscene(nko):=2;
	rectobj.setrect(edgexp,
		-6.0, 0.12, -5.0,
		 0.1, 0.12,  1.9,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
		koyhi(nko):=3.0;

----- edge addendum begin ----------------------------

	nko:=nko+1;
	koscene(nko):=2;
	rectobj.setrect(edgexm,
		-9.9, 0.12, -5.0,
		 0.1, 0.12,  1.9,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
		koyhi(nko):=3.0;


----- edge addendum end ----------------------------


	--"cornice" refers to arches @ top of each marble column

	for i in 0..4 loop

		hox(1,i):= mpx(i);
		hoy(1,i):= (cy2c+cyh)-rr;
		hoz(1,i):= mpz(i)+rr +rc;
		rectobj.setrect(cornice(1,i),
			mpx(i), cy2c+cyh-r2, mpz(i)+r2+rc, -- x2c,y2c,z2c
			 0.0, r2, r2,   --x2r,y2r,z2r
			 j1,j2,j3,j4,j5,j6 );

		hox(2,i):=mpx(i);
		hoy(2,i):= cy2c+cyh-rr;
		hoz(2,i):= mpz(i)-rr -rc;
		rectobj.setrect(cornice(2,i),
			mpx(i), cy2c+cyh-r2, mpz(i)-r2-rc, -- x2c,y2c,z2c
			 0.0, r2, r2,   --x2r,y2r,z2r
			 j1,j2,j3,j4,j5,j6 );

-------------------------------------------------------------
		hox(3,i):=mpx(i)-rr -rc;
		hoy(3,i):=cy2c+cyh-rr;
		hoz(3,i):=mpz(i);
		rectobj.setrect(cornice(3,i),
			mpx(i)-r2-rc, cy2c+cyh-r2, mpz(i), -- x2c,y2c,z2c
			 r2, r2, 0.0,   --x2r,y2r,z2r
			 j1,j2,j3,j4,j5,j6 );

		hox(4,i):=mpx(i)+rr +rc;
		hoy(4,i):=cy2c+cyh-rr;
		hoz(4,i):=mpz(i);
		rectobj.setrect(cornice(4,i),
			mpx(i)+r2+rc, cy2c+cyh-r2, mpz(i), -- x2c,y2c,z2c
			 r2, r2, 0.0,   --x2r,y2r,z2r
			 j1,j2,j3,j4,j5,j6 );

		--marble pillar
		nko:=nko+1;
		koscene(nko):=2;
		cylobj.setcyl(
			moorishpillar(i),
			mpx(i), cy2c, mpz(i),
			        cyh, rc,
			-- j1,j2,j3,j4,j5,j6 );
			koxlo(nko),koxhi(nko), koylo(nko),
			koyhi(nko), kozlo(nko),kozhi(nko) );

	end loop; --for i

---------- end pillar with cornices ------------------

		--13oct16 experiment:
		cylobj.setinterior(
			iskylight,
			-5.0, iymax-0.15, -5.0, --center
			        0.15, 0.45,     --ht, rad
			 j1,j2,j3,j4,j5,j6 );


-----------------------------------------------------

pictobj.setrect(roq, 
	-5.0, iymax-0.28, -5.0, 
	0.46,0.01,0.46,
	xm,xp,ym,yp,zm,zp --discard	
	);

-----------------------------------------------------



	--nonreflective pool using fancy frag.shader:
	newrectsurf.setrect(rso, mpx(0),mpz(0), 1.9,1.9 );
	reflsurf.setrect(rfo, mpx(0),mpz(0), 1.9, 1.9 );


--end if; --not restore_

	ixdoor:=x2c;
	iydoor:=y2c-y2r+hdoor;
	izdoor:=z2c-z2r;



end setup_castle;



