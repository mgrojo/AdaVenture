separate(gameutils)

-- this assumes mm=ID, (xme,yme,zme)=virtual pos within skybox
-- [ actual pos versus skybox is always (0,0,0) ]
procedure updateMVPs( 
	et: gldouble;
	wid,hit : float; upd8: boolean := false ) is

	xlk,ylk,zlk, xrt,yrt,zrt,
	xpos,ypos,zpos, xup,yup,zup : float;
	degtilt: constant float := -60.0; -- -45.0;
	pang, padeg, px,py,pz: float;
begin

	if thirdPerson then
		xpos:=xcam; ypos:=ycam; zpos:=zcam;
	else -- firstPerson
		xpos:=xme; ypos:=yme; zpos:=zme;
		choriang:=horiang;
		cxlook := fmath.cos(vertang)*fmath.sin(choriang);
		cylook := fmath.sin(vertang);
		czlook := fmath.cos(vertang)*fmath.cos(choriang);
	end if;

	-- Look Vector
	xlk:=xpos+cxlook;
	ylk:=ypos+cylook;
	zlk:=zpos+czlook;

	-- Right unit-Direction
	xrt:= fmath.sin(choriang-halfpi);
	yrt:= 0.0;
	zrt:= fmath.cos(choriang-halfpi);

	-- calculate UP unit-Direction
	cross( xrt,yrt,zrt, cxlook,cylook,czlook, xup,yup,zup );

	perspective(pm, 45.0, wid/hit,  0.1, 100.0);

	-- note:  in this app we have mm=id 
	--        so modelview = mv = mm*vm = vm = viewmatrix

	lookat(mv, 0.0,0.0,0.0, cxlook,cylook,czlook, xup,yup,zup ); 
	--ViewMat (pos within skybox)

	lookat(imv, xpos,ypos,zpos, xlk,ylk,zlk, xup,yup,zup ); 
	--ViewMat (pos within landscape)

	mmvp:=mv;
	imvp:=imv;
	matXmat(mmvp,pm); -- used for skybox only
	matXmat(imvp,pm); -- used everywhere else


	--myInvTransp(mm,nm3);
	-- For a given modelmatrix (MM: mat44), 
	-- we can get a normalMatrix (NM: mat33) by:
	-- myInvTransp(MM,NM)
	-- However, in this app, MM=ID => NM=ID


	if upd8 then
	-- begin update Jupiter:

		-- position
		pang:=float(et)*twopi*0.025;
		px:=1.0*fmath.sin(pang);
		pz:=1.0*fmath.cos(pang);
		py:=20.0;

		-- spinAngle
		--padeg := -et*twopi*2.5;
		padeg := -float(et)*twopi*rad2deg*0.1; -- 10sec/rev
	----------------------------------------------------
		jupit.setpos(px,py,pz);

		satmm:=identity;
		translate(satmm, -px,-py,-pz); --first, move to origin

	-- begin rotations

		--spin on polar axis:
		degRotate(satmm, padeg, 0.0,1.0,0.0); -- Y=polar-axis

		--axis-tilt:
		degRotate(satmm, degtilt, 1.0, 0.0, 0.0);

	-- end rotations

		translate(satmm, px,py,pz); --now, move back

		satmv:=satmm;
		matXmat(satmv,mv);
		satmvp:=satmv;
		matXmat(satmvp,pm);
	---------------------------------------------

	end if; -- if upd8


end updateMVPs;






