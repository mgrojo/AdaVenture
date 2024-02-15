
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


procedure setup_exterior(chapter: integer) is -- scene=1



function max( x,y : float ) return float is
begin
	if y>x then return y;
	else return x; end if;
end;


	yh, yd, halflength,depth: float;

	ydoor, wdoor,hdoor : float;

begin --setup_castle




	-- the 4 corner legs...

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(4), --xm,-zm
		-10.8, 0.5, -10.8, --xc,yc,zc
	 	 0.5, 1.0, 0.5, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(5), --xm, zp
		-10.8, 0.5, +0.8, --xc,yc,zc
		 0.5, 1.0, 0.5, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(6),  --xp, zp
		+0.8, 0.5, +0.8, --xc,yc,zc
		 0.5, 1.0, 0.5, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(7), --xp, zm
		+0.8, 0.5, -10.8, --xc,yc,zc
		 0.5, 1.0, 0.5, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );



	-- now the 4 corner towers...y=1.5..4.5

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(8), 
		-10.8, 3.0,-10.8, --xc,yc,zc
		  0.8, 1.5,  0.8, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(9), 
		-10.8, 3.0,+0.8, --xc,yc,zc
		  0.8, 1.5, 0.8, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(10), 
		+0.8, 3.0,+0.8, --xc,yc,zc
		 0.8, 1.5, 0.8, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(11), 
		+0.8, 3.0,-10.8, --xc,yc,zc
		 0.8, 1.5, 0.8, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );









	-- now 4 edge walls between the towers:

	--XM wall
	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(12),
		-10.2, 1.5,-5.0, --xc,yc,zc
		 0.05, 2.5,  5.4, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

--this ZM wall comes in 3 pieces

		nko:=nko+1;
		koscene(nko):=1;
		rectobj.setrect( 
			wallblok(13),  -- castle gate side...above door @ y=1.0...4.0
			-5.0, 2.5,-10.2, --xc,yc,zc
			 5.4, 1.5, 0.05, --xr,yr,zr
			koxlo(nko),koxhi(nko), koylo(nko),
			koyhi(nko), kozlo(nko),kozhi(nko) );

		nko:=nko+1;
		koscene(nko):=1;
		rectobj.setrect( 
			wallblok(14),  -- castle gate side...left of door
			-2.1, 0.3,-10.2, --xc,yc,zc
			 2.5, 0.7, 0.14, --xr,yr,zr
			koxlo(nko),koxhi(nko), koylo(nko),
			koyhi(nko), kozlo(nko),kozhi(nko) );

		nko:=nko+1;
		koscene(nko):=1;
		rectobj.setrect( 
			wallblok(15),  -- castle gate side...right of door
			-7.9, 0.3,-10.2, --xc,yc,zc
			 2.5, 0.7, 0.14, --xr,yr,zr
			koxlo(nko),koxhi(nko), koylo(nko),
			koyhi(nko), kozlo(nko),kozhi(nko) );




	--XP wall
	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(16),  --xp,zc
	    +0.2, 1.5,-5.0, --xc,yc,zc
		 0.05, 2.5, 5.4, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	--ZP wall
	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallblok(17),  --xc, zp
		-5.0, 1.5, 0.2, --xc,yc,zc
		 5.4, 2.5, 0.05, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );





	--highest step up into castle interior @ y=0
	rectobj.setrect( 
		wallblok(18),
		-5.0, -0.2,-10.2,
		 0.8,  0.2, 0.15,
		j1,j2,j3,j4,j5,j6);

	--lowest step up into castle interior @ y=0
	rectobj.setrect( 
		wallblok(19),
		-5.0, -0.3,-10.5,
		 0.9,  0.2, 0.40,
		j1,j2,j3,j4,j5,j6);










-- initialize the ZM gate and related KeepOuts

	xgzm:=-5.0;  zgzm:=-10.2;
	nko:=nko+1;
	koscene(nko):=1;
	gzmk:=nko;
	pictobj.setrect( 
		gatezm, 
		xgzm, 0.4,zgzm, --xc,yc,zc
		0.4, 0.4, 0.0, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	pictobj.setrect( 
		zoro, 
		xgzm, 2.0, zgzm-0.1, --xc,yc,zc
		0.6, 0.6, 0.0, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);


-- initialize White Key

	if chapter=1 then
		xwkey:=3.0;
		zwkey:=3.0;

	elsif chapter=3 then
		xwkey:=-11.4;
		zwkey:= -2.0;

-----------------------------------------
	elsif chapter=2 then
		xwkey:=-11.4;
		zwkey:= -9.3;

	elsif chapter=4 then
		xwkey:=-3.0; -- was -5.0 but cistern forced change 7feb24
		zwkey:= 0.5;

	end if;

	ywkey:=0.025+land_alt(xwkey,zwkey);
	swkey:=1;


	pictobj.setrect( 
		key1, 
		xwkey,ywkey,zwkey, --xc,yc,zc
		rkey, 0.0, 0.5*rkey, --xr,yr,zr
		j1,j2,j3,j4,j5,j6);


	--castle interior view

	wdoor:=0.5;
	hdoor:=0.7;





-- define maze door
	nko:=nko+1;
	koscene(nko):=1;
	yh:=0.5;

	yd:=land_alt(xmaze,zmaze);
	pictobj.setrect( 
		omazedoor, 
		xmaze, yd+0.6, zmaze,
		0.6, 0.6, 0.3,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	ymaze := yd+0.3;

-- gate for maze
	nko:=nko+1;
	koscene(nko):=1;
	gk1:=nko;
	pictobj.setrect( 
		gatem1, 
		xmaze, ymaze,zmaze-0.3, --xc,yc,zc
		0.3, 0.3, 0.01, --xr,yr,zr
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );



-- +X = west
-- +Z = north

-- maze perimeter walls: see scene1.txt
	nko:=nko+1;
	koscene(nko):=1;
	depth:=1.0;
	halflength:=7.0;
	rectobj.setrect( 
		wallxp,
		5.19, yd+yh-depth,5.0+halflength-0.19,
		0.2,   0.6+depth, halflength,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );


	nko:=nko+1;
	koscene(nko):=1;
	rectobj.setrect( 
		wallzp, 
		5.0+halflength, yd+yh-depth,  5.0,
		halflength,   0.6+depth, 0.2,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

----- begin new sections: ---------------------------------------

	halflength:=23.0/2.0;
	rectobj.setrect( 
		wallLf,
		18.2, yd-depth,-18.0+halflength,
		0.2, depth, halflength,
		j1,j2,j3, j4,j5,j6 );

	halflength:=36.0/2.0;
	rectobj.setrect( 
		wallRt,
		-18.2, yd-depth,-18.0+halflength,
		0.2, depth, halflength,
		j1,j2,j3, j4,j5,j6 );



	halflength:=36.0/2.0;
	rectobj.setrect( 
		wallBt,
		-18.0+halflength, yd-depth, -18.2,
		halflength+0.4, depth, 0.2,
		j1,j2,j3, j4,j5,j6 );

	halflength:=23.2/2.0;
	rectobj.setrect( 
		wallTp,
		-18.0+halflength, yd-depth, +18.2,
		halflength+0.4, depth, 0.2,
		j1,j2,j3, j4,j5,j6 );






-------------------------------------------------------------------

		--new waterfall params [on north wall => z=Zmax]

		--particle-waterfall initialization:
		myparticles.setrect(wfallp,
			float(wfxc), float(wfyc),
				float(wfxr), float(wfyr) );

		--ribbon-waterfall initialization:
		myribbon.setrect(wfallr, 
			float(wfxc), float(wfyc),
				float(wfxr), float(wfyr) );

-------------------------------------------------------------------

	--new cistern top:
	cyl2texobj.setcyl2(rococo, cisxc, cisyc, ciszc, cisrr );

	nko:=nko+1;
	koscene(nko):=1;
	koxlo(nko):= cisxc - cisrr;
	koxhi(nko):= cisxc + cisrr;

	koylo(nko):= cisyc;
	koyhi(nko):= float(wfyc);

	kozlo(nko):= ciszc-cisrr;
	kozhi(nko):= ciszc+cisrr;


-------------------------------------------------------------------






	-- all trees are drawn with this 1 object:
	w3treeobj.setrect(tree);
	-- trees are positioned using uniforms @ draw time


	jupit.yseteli(0.5,0.5,0.5); -- +Y=Npole

	-- this is not actually needed here, but its more of
	-- a reminder that we MUST reset prior to REUSE:
	jupit.setpos(0.0, 0.0, 0.0);
	--this works to put Jupiter @ any fixed position:



	wdoor:=0.5;
	hdoor:=0.7;

	xdoor:=-5.0;
	zdoor:=-10.0;
	ydoor:= hdoor/2.0 + land_alt(xdoor,zdoor);


end setup_exterior;



