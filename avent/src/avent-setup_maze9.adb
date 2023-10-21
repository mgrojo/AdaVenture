
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


procedure setup_maze9 is 
--   scene # 9

	xm,xp,ym,yp,zm,zp,
	zzr,xxr,zzc,xxc,
	--zar,zac,
	yr,yc, xr,xc, zr,zc: float;
	hwdoor : constant float := 0.8; --halfWidth
	hhdoor : constant float := 0.9; --halfHeight
	iymaze : constant float := -iymax+hhdoor;
	epsi : constant float := 0.01;

begin



	--walls
	--droomobj.setrect( mdo9,
	--	0.0, 0.0, 0.0,
	--	ixmax,iymax,izmax,
	--	ixmax/2.0,iymax,izmax/2.0); 

	--Note:  setrect2 is a cheap trick to show reflected light
	--       ONLY on 1 wall and ceiling...only those 2 normals
	--       are calculated correctly:
	droomobj.setrect2( mdo9,
		0.0, -iymax/2.0, 0.0,
		ixmax,iymax/2.0,izmax,
		ixmax/4.0,iymax,izmax/4.0); 



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

--put_line("Maze9...(Xrow,Zcol) xrow in 10..-10 :");
--new_line;

	for row in reverse -mrows..mrows loop
	for col in -mcols..mcols loop

		if iswall(9,row,col) then

			-- note:  my maze9-definitions had x,z reversed
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
--put_line("nko="&integer'image(nko));
			koscene(nko):=9;
			pictobj.setrect( mzwall(9,row,col), 
				xxc,yc,zzc,  xxr,yr,zzr,
					koxlo(nko),koxhi(nko), koylo(nko),
						koyhi(nko), kozlo(nko),kozhi(nko) );


			korow(nko):=row;
			kocol(nko):=col;
			mazewall(nko):=true;


			if 9=sgate and row=rgate and col=cgate then
				insertgate(nko);
				insertable:=0;
			end if;

			--put("*");
		else --not a wall
			null;
			--put(" ");
		end if;
	end loop; --col
	--new_line;
	end loop; --row

	--new_line;
	--put_line("end M9");

---------------------------------end maze def










-- setup ceiling for fancy fragshader:
rectxobj.setrect(rox, 0.0, -0.01, 0.0, ixmax/2.1,0.01,izmax/2.1);
-- no texture needed here;  fancy fragshader used instead

pictobj.setrect(roz, 
	0.0, -0.02, 0.0, 
	ixmax/2.0,0.01,izmax/2.0,
	xm,xp,ym,yp,zm,zp --discard	
	);
	
---------------------------------------------------



	-- doors

	pictobj.setrect( 
		al9,
		+1.0, iymaze,-10.0+eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(1):=+1.0;
	zdc9(1):=-10.0+eps;

	pictobj.setrect( 
		bl9,
		+5.0, iymaze,-10.0+eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(2):=+5.0;
	zdc9(2):=-10.0+eps;

	pictobj.setrect( 
		cl9,
		-4.0, iymaze,-10.0+eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(3):=-4.0;
	zdc9(3):=-10.0+eps;

	pictobj.setrect( 
		fl9,
		+8.0, iymaze,-10.0+eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(4):=+8.0;
	zdc9(4):=-10.0+eps;

------------------------------------------

	pictobj.setrect( 
		ar9,
	-9.0, iymaze,+10.0-eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(5):=-9.0;
	zdc9(5):= 10.0-eps;

	pictobj.setrect( 
		br9,
	-5.0, iymaze,+10.0-eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(6):=-5.0;
	zdc9(6):= 10.0-eps;

	pictobj.setrect( 
		cr9,
		+6.0, iymaze,+10.0-eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(7):=+6.0;
	zdc9(7):= 10.0-eps;

	pictobj.setrect( 
		fr9,
	-2.0, iymaze,+10.0-eps,
		hwdoor, hhdoor, 0.0,
		j1,j2,j3,j4,j5,j6);

	xdc9(8):=-2.0;
	zdc9(8):= 10.0-eps;


---------------------------------


	pictobj.setrect( 
		dt9,
		+10.0-eps, iymaze,-8.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc9(9):=+10.0-eps;
	zdc9(9):= -8.0;

	pictobj.setrect( 
		et9,
		+10.0-eps, iymaze, -1.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc9(10):=+10.0-eps;
	zdc9(10):= -1.0;


	pictobj.setrect( 
		db9,
	-10.0+eps, iymaze,-8.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc9(11):=-10.0+eps;
	zdc9(11):= -8.0;

	pictobj.setrect( 
		eb9,
	-10.0+eps, iymaze,-1.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc9(12):=-10.0+eps;
	zdc9(12):= -1.0;


--------------------------------------------------------

	--this maze entrance = exit to lab8
	pictobj.setrect( 
		imaze9door,
		-10.0+0.02, iymaze, +7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);

	xdc9(13):=-10.0+0.02;
	zdc9(13):= 7.0;


-- labyrinth sign:
	pictobj.setrect( 
		lg9,
	-10.0+eps, iymaze+hhdoor+0.1,+7.0,
		0.0, 0.1, hwdoor,
		j1,j2,j3,j4,j5,j6);






	-- inside secret room:
	xchalice:=-9.0;
	ychalice:=hcup-iymax;
	zchalice:=-3.0;
	schalice:=9;
	drawchalice:=true;

	-- bounds of secret room, needed to test for choir music
	xmin9 := -10.0;
	xmax9 := -8.0;
	zmin9 := -7.0;
	zmax9 := -2.0;
	-- these MUST match initial placement of chalice!






end setup_maze9;



