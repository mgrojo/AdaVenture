
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


procedure setup_lab8 is 
--   scene # 8

	zzr,xxr,zzc,xxc,
	yr,yc, xr,xc, zr,zc: float;
	hwdoor : constant float := 0.8; --halfWidth
	hhdoor : constant float := 0.9; --halfHeight
	iymaze : constant float := -iymax+hhdoor;

begin


	--walls
	--droomobj.setrect( mdo8,
	--	0.0, 0.0, 0.0,
	--	ixmax,iymax,izmax,
	--	ixmax/2.0,iymax,izmax/2.0); 

	droomobj.setrect( mdo8,
		0.0, -iymax/2.0, 0.0,
		ixmax,iymax/2.0,izmax,
		ixmax/4.0,iymax,izmax/4.0); 



	--floor (slime covered)
	rectobj.setrect( 
		lfloor, 
		0.0, -iymax+0.01, 0.0, --xc,yc,zc
		ixmax, 0.0, izmax, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);



-- begin maze walls => x,z within [-10,+10]

	yr:=0.3; -- 0.3 *2  is high enough
	yc:=-iymax+yr;
	xr:=0.5;
	zr:=0.5;
---------------------------------begin maze def

	for row in -mrows..mrows loop
	for col in -mcols..mcols loop
		if iswall(8,row,col) then

			-- note:  my definitions had x,z correct
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
			koscene(nko):=8;
			pictobj.setrect( mzwall(8,row,col), 
				xxc,yc,zzc,  xxr,yr,zzr,
					koxlo(nko),koxhi(nko), koylo(nko),
						koyhi(nko), kozlo(nko),kozhi(nko) );



------- begin insert ----------------------------------

			korow(nko):=row;
			kocol(nko):=col;
			mazewall(nko):=true;


			if 8=sgate and row=rgate and col=cgate then
				insertgate(nko);
				insertable:=0;
			end if;


------- end insert ----------------------------------

		end if;
	end loop; --col
	end loop; --row

---------------------------------end maze def

	-- setup ceiling for fancy fragshader:
	--rectxobj.setrect(rox,0.0,iymax-0.01,0.0, ixmax,0.01,izmax);
	-- fttb: unused!
	-- no texture needed here;  fancy fragshader used instead

--------------------------------------------------------

	--door to M9
	pictobj.setrect( 
		g8,
		+10.0-eps, iymaze,+7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);



	--door to M7
	pictobj.setrect( 
		h8,
		-10.0+eps, iymaze,+7.0,
		0.0, hhdoor, hwdoor,
		j1,j2,j3,j4,j5,j6);




	--exitG to m9
	pictobj.setrect( 
		eg8,
		+10.0-eps, iymaze+hhdoor+0.1,+7.0,
		0.0, 0.1, hwdoor,
		j1,j2,j3,j4,j5,j6);

xdc8(1):=10.0-eps;
zdc8(1):=7.0;

	--exitH to m7
	pictobj.setrect( 
		eh8,
		-10.0+eps, iymaze+hhdoor+0.1,+7.0,
		0.0, 0.1, hwdoor,
		j1,j2,j3,j4,j5,j6);

xdc8(2):=-10.0+eps;
zdc8(2):=7.0;






	kgate:=0; -- => inactive, lying on ground

	if chapter=4 then
		xgate:=9.0;
		zgate:=5.0;
	else
		xgate:= 0.0;
		zgate:=-0.5;
	end if;
	ygate:=-iymax+2.0*htobj;

	sgate:=8;
	gateheld:=false;

	pictobj.setrect( 
		gateway, 
		xgate,ygate,zgate, --xc,yc,zc
		0.2, 0.0, 0.2, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);



end setup_lab8;



