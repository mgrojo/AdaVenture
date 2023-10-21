
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


procedure setup_temple is -- scene=4

	hwdoor : constant float := 0.8; --halfWidth
	fhdoor : constant float := 2.0; --fullHeight
	tdoor : constant float := 0.1;

begin

--if not restore_temple then --first time

-- aheight=0.5 so lets try to live with that...
-- default room size is [x,y,z] in [-10..10, -3..3, -10..10]

	--walls
	droomobj.setroomwithZMdoor( tdo,
		--dx, dy
		hwdoor, fhdoor,
		0.0, 0.0, 0.0,
		ixmax/3.0,iymax,izmax-0.5*htobj,

		ixmax/6.0,iymax/2.0,izmax/4.0); --scaled for 16 mirrored copies each wall

	--mural
	pictobj.setrect(zpmural, 
		0.0,-1.0, izmax-htobj, 
		ixmax/5.0,1.0,0.5*htobj, 
		j1,j2,j3,j4,j5,j6);

	-- walls
	rectobj.setrect(zpwall, 
		0.0, 2.0, izmax-htobj,  --htobj=0.02
		ixmax/3.0,1.0,0.5*htobj, 
		j1,j2,j3,j4,j5,j6);
	rectobj.setrect(zmwall, 
		0.0, 2.0, -izmax+htobj, 
		ixmax/3.0,1.0,0.5*htobj, 
		j1,j2,j3,j4,j5,j6);
	rectobj.setrect(xpwall, 
		ixmax/3.0-htobj, 2.0, 0.0, 
		0.5*htobj, 1.0, izmax,
		j1,j2,j3,j4,j5,j6);
	rectobj.setrect(xmwall, 
		-ixmax/3.0+htobj, 2.0, 0.0, 
		0.5*htobj, 1.0, izmax,
		j1,j2,j3,j4,j5,j6);


	--ceiling
	rectobj.setrect(rceil, 
		0.0,iymax-0.01,0.0, 
		ixmax-htobj,0.0,izmax-htobj, 
		j1,j2,j3,j4,j5,j6);

	rectobj.setrect(tfloor, 
		0.0,-iymax+0.01,0.0, 
		ixmax-htobj,0.0,izmax-htobj, 
		j1,j2,j3,j4,j5,j6);

-- define temple entry hall:
	ixtmpl:=0.0;
	iytmpl:=-iymax+fhdoor/2.0;
	iztmpl:=-izmax+htobj;


	--marble top beam
	pictobj.setrect( 
		beam34,
		ixtmpl,iytmpl+fhdoor/2.0,iztmpl+0.3,
		hwdoor+0.6, 0.3, 0.3,
		j1,j2,j3,j4,j5,j6);

	--2 marble entry pillars
	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar3,
		ixtmpl-hwdoor-0.2,iytmpl,iztmpl+0.3,
		fhdoor/2.0, 0.25,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar4,
		ixtmpl+hwdoor+0.2,iytmpl,iztmpl+0.3,
		fhdoor/2.0, 0.25,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );


--------- begin 6 tall marble pillars

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar5,
		-ixmax/3.0+0.5, 0.0, -izmax+0.5,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar6,
		-ixmax/3.0+0.5, 0.0, +izmax-0.5,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar7,
		-ixmax/3.0+0.5, 0.0, 0.0,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );




	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar8,
		+ixmax/3.0-0.5, 0.0, -izmax+0.5,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar9,
		+ixmax/3.0-0.5, 0.0, +izmax-0.5,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );

	nko:=nko+1;
	koscene(nko):=4;
	cylobj.setcyl(
		pillar10,
		+ixmax/3.0-0.5, 0.0, 0.0,
		iymax, 0.5,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );
















--define rocksafe
	nko:=nko+1;
	koscene(nko):=4;
	pictobj.setrect( 
		rsafe,
		0.0,-iymax+0.3, izmax-0.3,
		0.3, 0.3, 0.3,
		koxlo(nko),koxhi(nko), koylo(nko),
		koyhi(nko), kozlo(nko),kozhi(nko) );


	xchalice:=(0.0);
	ychalice:=hcup + (-iymax+0.6);
	zchalice:=(izmax-0.3);
	schalice:=4;
	drawchalice:=true;



--------------------------------------------------------


--end if; -- restore

	interior:=true;


	ixtmpl:=0.0;
	iytmpl:=-iymax+fhdoor/2.0;
	iztmpl:=-izmax;


--restore_temple:=true; --flag indicates already prepped

end setup_temple;



