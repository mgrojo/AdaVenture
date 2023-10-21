separate(gameutils)


procedure setState is
begin

	if bdragondead then
				pictobj.setrect( 
					bdragon, 
					bxdra,bydra,bzdra,
					0.6, 0.0, 0.6, --xr,yr,zr
					j1,j2,j3,j4,j5,j6);
	end if;

	if rdragondead then
				pictobj.setrect( 
					rdragon, 
					rxdra,rydra,rzdra,
					0.6, 0.0, 0.6, --xr,yr,zr
					j1,j2,j3,j4,j5,j6);
	end if;

	if not bkeyheld then

		pictobj.setrect( 
			key2, 
			xbkey,ybkey,zbkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

		pictobj.setrect( 
			key0, 
			xbkey, -htobj-htobj, zbkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);


	end if;

	if not gkeyheld then

		pictobj.setrect( 
			key3, 
			xgkey,ygkey,zgkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

	end if;

	if not wkeyheld then

		pictobj.setrect( 
			key1, 
			xwkey,ywkey,zwkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

	end if;

	if not swordheld then

		pictobj.setrect( 
			sword, 
			xsword,ysword,zsword,
			rsword, 0.0, 0.2*rsword,
			j1,j2,j3,j4,j5,j6);


		--set position of ghostSword:
		pictobj.setrect(
			sword0, 
			xsword,-htobj-htobj,zsword,
			rsword, 0.0, 0.2*rsword,
			j1,j2,j3,j4,j5,j6);

	end if;

	if not chaliceheld then
		null;
	end if;

	if not gateheld then

			pictobj.setrect( 
				gateway, 
				xgate,ygate,zgate, --xc,yc,zc
				0.2, 0.0, 0.2, --xr,yr,zr
				j1,j2,j3,j4,j5,j6);

	end if;

	if 
		lionopen 
		and (chapter=1 or chapter=3)
	then

			pictobj.setrect( 
				dungdoor,
				xlion, -iymax+1.8*1.3, zlion,
				0.8-0.05, 0.9, 0.05,
				j1,j2,j3,j4,j5,j6);

			--disable KO #lko
			koxlo(lko):=0.0;
			koxhi(lko):=0.0;
			koylo(lko):=0.0;
			koyhi(lko):=0.0;
			kozlo(lko):=0.0;
			kozhi(lko):=0.0;


	end if;

	if 
		labopen 
		and (chapter=2 or chapter=4)
	then

			pictobj.setrect( 
				sh7,
				xlab,ylab-0.9,zlab,
				0.0, 0.9*0.5, 0.8*0.46,
				j1,j2,j3,j4,j5,j6);

			--disable KO
			koxlo(wlabko):=0.0;
			koxhi(wlabko):=0.0;
			koylo(wlabko):=0.0;
			koyhi(wlabko):=0.0;
			kozlo(wlabko):=0.0;
			kozhi(wlabko):=0.0;


	end if;

	if mazeopen then

			pictobj.setrect( 
				gatem1, 
				xmaze, ymaze+0.6, zmaze-0.3, --xc,yc,zc
				0.3, 0.3, 0.01, --xr,yr,zr
				koxlo(gk1),koxhi(gk1),koylo(gk1),
				koyhi(gk1),kozlo(gk1),kozhi(gk1) );
			--koylo(gk1):=20.0;
			--koyhi(gk1):=21.0;
	
	end if;


	if gateopen then --castleGate

			pictobj.setrect( 
				gatezm, 
				-5.0, 1.1, -10.2, --xc,yc,zc
				0.4, 0.4, 0.0, --xr,yr,zr
				koxlo(gzmk),koxhi(gzmk),koylo(gzmk),
				koyhi(gzmk),kozlo(gzmk),kozhi(gzmk) );
			--koylo(gzmk):=20.0;
			--koyhi(gzmk):=19.0;
	
	end if;





end setState;


