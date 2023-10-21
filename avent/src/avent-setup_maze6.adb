
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


procedure setup_maze6 is 
--   scene #   6 (exit + 2 below)

	zzr,xxr,zzc,xxc,
	--zar,zac,
	yr,yc, xr,xc, zr,zc: float;
	hwdoor : constant float := 0.8; --halfWidth
	fhdoor : constant float := 1.8; --fullHeight

begin

	-- define initial location of portable gate
	sgate:=6;  --scene#
	rgate:=-2; --row
	cgate:=+5; --col
	gateheld:=false;

-----------------------------------------------------






	--walls
	--droomobj.setroomwithZPdoor( mdo6,
	--	hwdoor, fhdoor,
	--	0.0, 0.0, 0.0,
	--	ixmax,iymax,izmax-0.01,
	--	ixmax/2.0,iymax,izmax/2.0); --scaled for 16 mirrored copies each wall

	droomobj.setroomwithZPdoor( mdo6,
		hwdoor, fhdoor,
		0.0, -iymax/2.0, 0.0,
		ixmax,iymax/2.0,izmax-0.01,
		ixmax/4.0,iymax,izmax/4.0); 
		--scaled for 16 mirrored copies on ceiling



	--floor (grass covered)
	rectobj.setrect( 
		mfloor, 
		0.0, -iymax+0.01, 0.0, --xc,yc,zc
		ixmax, 0.0, izmax, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);


	--bugfloor1 (slime covered)
	rectobj.setrect( 
		bugfloor1, 
		6.03, -iymax+0.015, -6.0, --xc,yc,zc
		0.03, 0.0, 1.0, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);

	--bugfloor2 (slime covered)
	rectobj.setrect( 
		bugfloor2, 
	  -6.03, -iymax+0.015, -6.0, --xc,yc,zc
		0.03, 0.0, 1.0, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);





-- begin maze walls => x,z within [-10,+10]

	yr:=0.3; -- 0.3 *2  is high enough
	yc:=-iymax+yr;
	xr:=0.5;
	zr:=0.5;
---------------------------------begin maze def

--put_line("Maze6 :");
--new_line;


	for col in reverse -mcols..mcols loop
	for row in -mrows..mrows loop
		if iswall(6,row,col) then

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
			koscene(nko):=6;
			pictobj.setrect( mzwall(6,row,col), 
				--xc,yc,zac,  xr,yr,zar,
				xxc,yc,zzc,  xxr,yr,zzr,
					koxlo(nko),koxhi(nko), koylo(nko),
						koyhi(nko), kozlo(nko),kozhi(nko) );

			korow(nko):=row;
			kocol(nko):=col;
			mazewall(nko):=true;


			if 6=sgate and row=rgate and col=cgate then --(r,c)=(-2,5)
				insertgate(nko);
				insertable:=0;
				--"insert" moveable gate here
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
	--put_line("===================end M6");

---------------------------------end maze def








----------- begin exit-to-temple door --------------------------------

	xtmpl:=0.0; --ixmax;
	ytmpl:=iymaze;
	ztmpl:=izmax;

   -- lift-open-lion-door
	xlion:=xtmpl; --ixmax-0.3;
	zlion:=ztmpl-0.3; --izmax-0.3;
	ylion:=ytmpl; --ymaze;
	nko:=nko+1;
	koscene(nko):=6;
	lko:=nko;
	pictobj.setrect( 
		dungdoor,
		xlion, ylion, zlion,
		hwdoor-0.05, fhdoor/2.0, 0.05,
		koxlo(lko),koxhi(lko),koylo(lko),
		koyhi(lko),kozlo(lko),kozhi(lko) );


-- marble top
	pictobj.setrect( 
		beam12,
		xtmpl, ytmpl+fhdoor/2.0, ztmpl-0.3,
		hwdoor+0.6, 0.3, 0.3,
		j1,j2,j3,j4,j5,j6);

-- need frame for temple entrance...
-- 2 cylindrical marble pillars with flat b crossbeam
-- centered at 
-- (ixmax, izmax-1.5*wdoor)
-- (ixmax-1.5*wdoor, izmax)

	nko:=nko+1;
	koscene(nko):=6;
	cylobj.setcyl(
		pillar1,
		xtmpl-hwdoor-0.2, ytmpl, ztmpl-0.3,
		fhdoor/2.0, 0.25,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=6;
	cylobj.setcyl(
		pillar2,
		xtmpl+hwdoor+0.2, ytmpl, ztmpl-0.3,
		fhdoor/2.0, 0.25,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );


----------- end exit-to-temple door --------------------------------









-- setup ceiling for fancy fragshader:
--rectxobj.setrect(rox,0.0,-0.01,0.0, ixmax,0.01,izmax);
-- no texture needed here;  fancy fragshader used instead
	
--------------------------------------------------------


	--A1 (left)
	pictobj.setrect( 
		doora1,
		10.0-eps, iymaze, 9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(1):=10.0-eps;
	zdc6(1):= 9.0;


	--B1 (left)
	pictobj.setrect( 
		doorb1,
		10.0-eps, iymaze, 4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(2):=10.0-eps;
	zdc6(2):= 4.0;

	--V6
	pictobj.setrect( 
		doorv6,
		10.0-eps, iymaze, 2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(3):=10.0-eps;
	zdc6(3):= 2.0;

	--W6
	pictobj.setrect( 
		doorw6,
		10.0-eps, iymaze, 0.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(4):=10.0-eps;
	zdc6(4):= 0.0;

	--X6
	pictobj.setrect( 
		doorx6,
		10.0-eps, iymaze,-2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(5):=10.0-eps;
	zdc6(5):=-2.0;

	--Y6
	pictobj.setrect( 
		doory6,
		10.0-eps, iymaze,-4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(6):=10.0-eps;
	zdc6(6):=-4.0;

	--Z6
	pictobj.setrect( 
		doorz6,
		10.0-eps, iymaze,-9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(7):=10.0-eps;
	zdc6(7):=-9.0;

---------------------------------------

	--C6
	pictobj.setrect( 
		doorc6,
		-10.0+eps, iymaze, 9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(8):=-10.0+eps;
	zdc6(8):= 9.0;

	--D6
	pictobj.setrect( 
		doord6,
		-10.0+eps, iymaze, 4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(9):=-10.0+eps;
	zdc6(9):= 4.0;

	--E6
	pictobj.setrect( 
		doore6,
		-10.0+eps, iymaze, 2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(10):=-10.0+eps;
	zdc6(10):= 2.0;

	--F6
	pictobj.setrect( 
		doorf6,
		-10.0+eps, iymaze, 0.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(11):=-10.0+eps;
	zdc6(11):= 0.0;

	--G6
	pictobj.setrect( 
		doorg6,
		-10.0+eps, iymaze,-2.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(12):=-10.0+eps;
	zdc6(12):=-2.0;

	--A2 (right)
	pictobj.setrect( 
		doora2,
		-10.0+eps, iymaze,-4.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(13):=-10.0+eps;
	zdc6(13):=-4.0;

	--B2 (right)
	pictobj.setrect( 
		doorb2,
		-10.0+eps, iymaze,-9.0,
		0.0, fhdoor/2.0, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc6(14):=-10.0+eps;
	zdc6(14):=-9.0;


end setup_maze6;



