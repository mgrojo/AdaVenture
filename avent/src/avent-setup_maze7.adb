
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


procedure setup_maze7 is 
--   scene # 7

	zzr,xxr,zzc,xxc,
	--zar,zac,
	yr,yc, xr,xc, zr,zc: float;
	hwdoor : constant float := 0.8; --halfWidth
	hhdoor : constant float := 0.9; --halfHeight
	iymaze : constant float := -iymax+hhdoor;
	epsi : constant float := 0.01;

	type loc is record
		x,z: float;
	end record;
	nrk: constant integer := 6; --# random key locations
	subtype rngtype is integer range 1..nrk;
	bkey7: constant array(rngtype) of loc := (
		(-9.0,+7.0), ( 0.0, +2.0), 
		(-4.0,+5.0), (+4.0,+5.0),
		(-4.0,+9.0), (+4.0,+9.0)
	);

	package random_key is new ada.numerics.discrete_random(rngtype);
	gen : random_key.generator;
	r: rngtype;
	-- random_key.reset(gen); -- time-dependent randomization
	-- r := random_key.random(gen); -- chooses r in rngtype

begin


-- initialize Black Key Coords

	sbkey:=7;
	if chapter=2 then
		xbkey:= 0.0;
		zbkey:= 0.0;
	else --chapter 4
		random_key.reset(gen); --randomizer
		r := random_key.random(gen);
		xbkey:=bkey7(r).x;
		zbkey:=bkey7(r).z;
	end if;





	--walls: x,z in -10..+10;
	--ceil:  y= 0
	--floor: y=-3
	droomobj.setrect( mdo7,
		0.0, -iymax/2.0, 0.0,			-- 0, -1.5, 0
		ixmax,iymax/2.0,izmax,			--10,  1.5, 10
		ixmax/4.0,iymax,izmax/4.0); 	--2.5, 3, 2.5



	--floor (grass covered)
	rectobj.setrect( 
		mfloor, 
		0.0, -iymax+epsi+epsi, 0.0, --xc,yc,zc
		ixmax-epsi, 0.0, izmax-epsi, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);



-- begin maze walls => x,z within [-10,+10]

	yr:=0.3; -- 0.3 *2  is high enough
	yc:=-iymax+yr;
	xr:=0.5;
	zr:=0.5;
---------------------------------begin maze def

--put_line("Maze7 :");
--new_line;


	for row in -mrows..mrows loop
	for col in -mcols..mcols loop
		if iswall(7,row,col) then

			-- note:  my definitions had x,z reversed
			xc:=float(col);
			zc:=float(row);

			-- insertion to avoid intrusions
			-- into neighboring rooms:

			xxc:=xc;
			xxr:=xr;
			zzc:=zc;
			zzr:=zr;

			if col=10 then xxc:=9.75; xxr:=0.249;
			elsif col=-10 then xxc:=-9.75; xxr:=0.249; end if;
			if row=10 then zzc:=9.75; zzr:=0.249;
			elsif row=-10 then zzc:=-9.75; zzr:=0.249; end if;


			nko:=nko+1;
			koscene(nko):=7;
			pictobj.setrect( mzwall(7,row,col), 
				xxc,yc,zzc,  xxr,yr,zzr,
					koxlo(nko),koxhi(nko), koylo(nko),
						koyhi(nko), kozlo(nko),kozhi(nko) );


			korow(nko):=row;
			kocol(nko):=col;
			mazewall(nko):=true;


			if 7=sgate and row=rgate and col=cgate then
				insertgate(nko);
				insertable:=0;
			end if;



			--put("*");
		else -- not a wall
			null;
			--put(" ");

		end if;
	end loop; --col
	--new_line;
	end loop; --row
	--new_line;
	--put_line("==========================end M7");

---------------------------------end maze def










-- setup ceiling for fancy fragshader:
--rectxobj.setrect(rox,0.0,-0.01,0.0, ixmax,0.01,izmax);
-- no texture needed here;  fancy fragshader used instead
	
--------------------------------------------------------


	--this maze entrance is at "bottom" @Zmin
	--ixmaze:= 0.0;
	--izmaze:=-izmax;

	pictobj.setrect( 
		imazedoor,
		ixmaze, iymaze, izmaze+0.02,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);


	pictobj.setrect( -- exit sign
		ex7,
		ixmaze, iymaze+hhdoor+0.1, izmaze+0.02,
		hwdoor, 0.1, 0.0,
		j1,j2,j3,j4,j5,j6);

---------------------------------------------------


	-- doors

	pictobj.setrect( 
		al7,
		-10.0+eps, iymaze,-9.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(1):=-10.0+eps;
	zdc7(1):=-9.0;

	pictobj.setrect( 
		bl7,
		-10.0+eps, iymaze,-7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(2):=-10.0+eps;
	zdc7(2):=-7.0;

	pictobj.setrect( 
		cl7,
		-10.0+eps, iymaze,-4.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(3):=-10.0+eps;
	zdc7(3):=-4.0;


	pictobj.setrect( 
		dl7,
		-10.0+eps, iymaze,-2.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(4):=-10.0+eps;
	zdc7(4):=-2.0;

	pictobj.setrect( 
		el7,
		-10.0+eps, iymaze, 0.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(5):=-10.0+eps;
	zdc7(5):= 0.0;

	pictobj.setrect( 
		fl7,
		-10.0+eps, iymaze,+3.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(6):=-10.0+eps;
	zdc7(6):= 3.0;






	pictobj.setrect( 
		ar7,
		10.0-eps, iymaze,-2.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(7):=10.0-eps;
	zdc7(7):=-2.0;

	pictobj.setrect( 
		br7,
		10.0-eps, iymaze, 0.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(8):=10.0-eps;
	zdc7(8):= 0.0;

	pictobj.setrect( 
		cr7,
		10.0-eps, iymaze,+3.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(9):=10.0-eps;
	zdc7(9):= 3.0;


	pictobj.setrect( 
		dr7,
		10.0-eps, iymaze,-9.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(10):=10.0-eps;
	zdc7(10):=-9.0;

	pictobj.setrect( 
		er7,
		10.0-eps, iymaze,-7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(11):=10.0-eps;
	zdc7(11):=-7.0;

	pictobj.setrect( 
		fr7,
		10.0-eps, iymaze,-4.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(12):=10.0-eps;
	zdc7(12):=-4.0;









-- lab door


	xlab:=10.0-eps*1.5;
	ylab:=iymaze-hhdoor*0.5;
	zlab:=7.0;
	nko:=nko+1;
	koscene(nko):=7;
	wlabko:=nko;
	-- slider sh7 initial pos
	pictobj.setrect( 
		sh7,
		xlab,ylab,zlab,
		0.0, hhdoor*0.5, hwdoor*0.51,
		koxlo(wlabko),koxhi(wlabko),koylo(wlabko),
		koyhi(wlabko),kozlo(wlabko),kozhi(wlabko) );

	--lab8 opening
	pictobj.setrect( 
		h7,
		10.0-eps, iymaze,+7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc7(13):=10.0-eps;
	zdc7(13):= 7.0;



-- labyrinth sign:
	pictobj.setrect( 
		lh7,
		10.0-eps, iymaze+hhdoor+0.1,+7.0,
		0.0, 0.1, hwdoor,
		j1,j2,j3,j4,j5,j6);





end setup_maze7;



