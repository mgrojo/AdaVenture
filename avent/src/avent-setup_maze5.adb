
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


procedure setup_maze5 is 
--   scene #   5  (entry + above)

	zzr,xxr,zzc,xxc,
	yr,yc, xr,xc, zr,zc: float;
	hwdoor : constant float := 0.8; --halfWidth
	hhdoor : constant float := 0.9; --halfHeight

	fhdoor : constant float := 1.8; --fullHeight

	type loc is record
		x,z: float;
	end record;

	nrk: constant integer := 4; --# random key locations
	subtype rngtype is integer range 1..nrk;
	bkey5: constant array(rngtype) of loc := (
		(-5.0,-9.0), ( 0.0, 0.0)    -- Maze #5 coordinates
		 ,( 7.0, -8.0), (-7.0, -8.0)   -- Maze #6 coordinates
	);

	package random_key is new ada.numerics.discrete_random(rngtype);
	gen : random_key.generator;
	r: rngtype;
	-- random_key.reset(gen); -- time-dependent randomization
	-- r := random_key.random(gen); -- chooses r in rngtype

begin



-- initialize Black Key Coords

	if resuming then
		null; -- sbkey, xbkey, zbkey will be defined elsewhere

	elsif chapter=1 then
		xbkey:= -5.0;
		zbkey:= -9.0;
		sbkey:=5;

	else --chapter 3
		random_key.reset(gen); --randomizer
		r := random_key.random(gen);

		if r>2 then 
			sbkey:=6; 
		else 
			sbkey:=5; 
		end if;

		xbkey:=bkey5(r).x;
		zbkey:=bkey5(r).z;

	end if;






	--walls
	--droomobj.setrect( mdo5, -- no door needed here
	--	0.0, 0.0, 0.0,
	--	ixmax,iymax,izmax-0.01,
	--	ixmax/2.0,iymax,izmax/2.0); --scaled for 16 mirrored copies each wall

	droomobj.setrect( mdo5, -- no door needed here
		0.0, -iymax/2.0, 0.0,      -- iymax=3.0 ==> Yroom in [-3..0]
		ixmax,iymax/2.0,izmax-0.01,
		ixmax/4.0,iymax,izmax/4.0); --scaled for 16 mirrored copies each wall



	--floor (grass covered)
	rectobj.setrect( 
		mfloor, 
		0.0, -iymax+0.01, 0.0, --xc,yc,zc
		ixmax, 0.0, izmax, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);



-- begin maze walls => x,z within [-10,+10]

	yr:=0.3; -- 0.3 *2  is high enough
	yc:=-iymax+yr;
	xr:=0.5;
	zr:=0.5;
---------------------------------begin maze def

--put_line("Maze5 :");
--new_line;


	for col in reverse -mcols..mcols loop
	for row in -mrows..mrows loop
		if iswall(5,row,col) then

			-- definining coords were inverted so...
			-- negate both coords to match picture with
			-- leftward = +X, and upward = +Z
			xc:=float(row);
			zc:=float(col);


			-- insertion to avoid intrusions
			-- into neighboring rooms:

			xxc:=xc;
			xxr:=xr;
			zzc:=zc;
			zzr:=zr;

			if row=10 then xxc:=9.75; xxr:=0.249;
			elsif row=-10 then xxc:=-9.75; xxr:=0.249; end if;
			if col=10 then zzc:=9.75; zzr:=0.249;
			elsif col=-10 then zzc:=-9.75; zzr:=0.249; end if;


			nko:=nko+1;
			koscene(nko):=5;
			pictobj.setrect( mzwall(5,row,col), 
				xxc,yc,zzc,  xxr,yr,zzr,
					koxlo(nko),koxhi(nko), koylo(nko),
						koyhi(nko), kozlo(nko),kozhi(nko) );


			korow(nko):=row;
			kocol(nko):=col;
			mazewall(nko):=true;



			if 5=sgate and row=rgate and col=cgate then
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
	--put_line("------------------ end M5");

---------------------------------end maze def


-- setup ceiling for fancy fragshader:
--rectxobj.setrect(rox,0.0,-0.01,0.0, ixmax,0.01,izmax);
-- no texture needed here;  fancy fragshader used instead
	
--------------------------------------------------------


	--this maze entrance is at "bottom" @Zmin
	--ixmaze:= 0.0;
	--iymaze:=-iymax+fhdoor/2.0;
	--izmaze:=-izmax;

	pictobj.setrect( 
		imazedoor,
		ixmaze, iymaze, izmaze+0.02,
		hwdoor, fhdoor/2.0, 0.0,
		j1,j2,j3,j4,j5,j6);


	pictobj.setrect( -- exit sign
		ex5,
		ixmaze, iymaze+hhdoor+0.1, izmaze+0.02,
		hwdoor, 0.1, 0.0,
		j1,j2,j3,j4,j5,j6);


---------------------------------------------------


	--C
	pictobj.setrect( 
		doorc,
		10.0-eps, iymaze, 9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(1):=10.0-eps;
	zdc5(1):=9.0;

	--D
	pictobj.setrect( 
		doord,
		10.0-eps, iymaze, 4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(2):=10.0-eps;
	zdc5(2):=4.0;

	--E
	pictobj.setrect( 
		doore,
		10.0-eps, iymaze, 2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(3):=10.0-eps;
	zdc5(3):=2.0;

	--F
	pictobj.setrect( 
		doorf,
		10.0-eps, iymaze, 0.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(4):=10.0-eps;
	zdc5(4):=0.0;

	--G
	pictobj.setrect( 
		doorg,
		10.0-eps, iymaze,-2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(5):=10.0-eps;
	zdc5(5):=-2.0;

---------------------------------------------


	--Y
	pictobj.setrect( 
		doory,
		-10.0+eps, iymaze, 9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(6):=-10.0+eps;
	zdc5(6):= 9.0;

	--Z
	pictobj.setrect( 
		doorz,
		-10.0+eps, iymaze, 4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(7):=-10.0+eps;
	zdc5(7):= 4.0;

	--V
	pictobj.setrect( 
		doorv,
		-10.0+eps, iymaze, 2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(8):=-10.0+eps;
	zdc5(8):= 2.0;

	--W
	pictobj.setrect( 
		doorw,
		-10.0+eps, iymaze, 0.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(9):=-10.0+eps;
	zdc5(9):= 0.0;

	--X
	pictobj.setrect( 
		doorx,
		-10.0+eps, iymaze,-2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc5(10):=-10.0+eps;
	zdc5(10):=-2.0;


end setup_maze5;



