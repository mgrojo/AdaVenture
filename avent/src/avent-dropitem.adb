separate(avent)

procedure dropitem( item: itemtype ) is
begin

	if item=pgate then --purple Gate portal

		if insertable>0 then

			insertgate(insertable);
			insertable:=0;

		else

			kgate:=0; -- => inactive gateway (on ground)
			xgate:=xme;
			zgate:=zme;
			sgate:=scene;
			gateheld:=false;
			if scene=2 then
				ygate:=htobj;
			elsif interior then
				ygate:=-iymax+2.0*htobj;
			else
				ygate:=2.0*htobj+land_alt(xme,zme);
			end if;

			pictobj.setrect( 
				gateway, 
				xgate,ygate,zgate, --xc,yc,zc
				0.2, 0.0, 0.2, --xr,yr,zr
				j1,j2,j3,j4,j5,j6);

		end if;


	elsif item=gkee then --green key

		xgkey:=xme;
		zgkey:=zme;
		sgkey:=scene;
		gkeyheld:=false;
		if scene=2 then
			ygkey:=htobj;
		elsif interior then
			ygkey:=-iymax+2.0*htobj;
		else
			ygkey:=htobj+land_alt(xme,zme);
		end if;

		pictobj.setrect( 
			key3, 
			xgkey,ygkey,zgkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);


	elsif item=bkee then --black key

		xbkey:=xme;
		zbkey:=zme;
		sbkey:=scene;
		bkeyheld:=false;
		if scene=2 then
			ybkey:=htobj;
		elsif interior then
			ybkey:=-iymax+2.0*htobj;
		else
			ybkey:=htobj+land_alt(xme,zme);
		end if;

		pictobj.setrect( 
			key2, 
			xbkey,ybkey,zbkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

		--update ghostKey
		pictobj.setrect( 
			key0, 
			xbkey,-htobj-htobj,zbkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);


	elsif item=wkee then --white key

		xwkey:=xme;
		zwkey:=zme;
		swkey:=scene;
		wkeyheld:=false;
		if scene=2 then
			ywkey:=htobj;
		elsif interior then
			ywkey:=-iymax+2.0*htobj;
		else
			ywkey:=htobj+land_alt(xme,zme);
		end if;

		pictobj.setrect( 
			key1, 
			xwkey,ywkey,zwkey, --xc,yc,zc
			0.1, 0.0, 0.1, --xr,yr,zr
			j1,j2,j3,j4,j5,j6);

	elsif item=srd then

		xsword:=xme;
		zsword:=zme;
		ssword:=scene;
		swordheld:=false;
		if scene=2 then
			ysword:=htobj;
		elsif interior then
			ysword:=-iymax+2.0*htobj;
		else
			ysword:=htobj+land_alt(xme,zme);
		end if;


		pictobj.setrect( 
			sword, 
			xsword,ysword,zsword,
			rsword, 0.0, 0.2*rsword,
			j1,j2,j3,j4,j5,j6);

		--update ghostSword:
		pictobj.setrect( 
			sword0, 
			xsword,-htobj-htobj,zsword,
			rsword, 0.0, 0.2*rsword,
			j1,j2,j3,j4,j5,j6);


	elsif item=cup then

		xchalice:=xme; --glfloat(xme);
		if scene=2 then
			ychalice:=htobj+hcup;
		elsif interior then
			ychalice:=-iymax+htobj+hcup;
		else
			ychalice:=htobj+land_alt(xme,zme)+hcup;
		end if;
		zchalice:=zme; --glfloat(zme);
		schalice:=scene;
		chaliceheld:=false;
		-- uses [dynamic] uniform-positioning @ draw-time

	end if;

end dropitem;


