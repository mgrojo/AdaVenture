separate(gameutils)

--NOTICE:  typically, dt<<0.1 second  (between iterations of main loop)
--         but when it is that big, it's probably because foldtime is stale
procedure moveforward( currenttime: gldouble ) is
	dt : gldouble := currenttime-foldtime;
	et : float := float(dt);
	--lagfac : constant float := 1.0;

	yyme, yhalf,
	kxl,kxh,kyl,kyh,kzl,kzh,
	okxl,okxh, okzl, okzh: float;
	ixmx: float;

	txc,tzc,dx,dz,fac,kt2: float;
	gxc,gzc: glfloat;
	function sqr(x: glfloat) return glfloat is
	begin
		return x*x;
	end sqr;
	function min6(r: glfloat) return boolean is
		d1,d2,d3,d4,d5,d6: glfloat;
		res: boolean := true;
		gxme: constant glfloat := glfloat(xme);
		gzme: constant glfloat := glfloat(zme);
	begin
		d1:=sqr(gxme-xt1c)+sqr(gzme-zt1c);
		d2:=sqr(gxme-xt2c)+sqr(gzme-zt2c);
		d3:=sqr(gxme-xt3c)+sqr(gzme-zt3c);
		d4:=sqr(gxme-xt4c)+sqr(gzme-zt4c);
		d5:=sqr(gxme-xt5c)+sqr(gzme-zt5c);
		d6:=sqr(gxme-xt6c)+sqr(gzme-zt6c);

		if d1<r then
			gxc:=xt1c; gzc:=zt1c;
		elsif d2<r then
			gxc:=xt2c; gzc:=zt2c;
		elsif d3<r then
			gxc:=xt3c; gzc:=zt3c;
		elsif d4<r then
			gxc:=xt4c; gzc:=zt4c;
		elsif d5<r then
			gxc:=xt5c; gzc:=zt5c;
		elsif d6<r then
			gxc:=xt6c; gzc:=zt6c;
		else
			gxc:=99.0; gzc:=99.0;
			res:=false;
		end if;
		txc:=float(gxc); tzc:=float(gzc);

		return res;

	end min6;

begin

if et>0.1 then et:=0.0; end if;
foldtime:=currenttime;

if scene=4 then
	ixmx:=ixmax/3.0;
else
	ixmx:=ixmax;
end if;

if 
	not gatewait
	and not labwait
	and not lionwait
	and not mazewait
	--and not batwait
then

	oxme:=xme;
	oyme:=yme;
	ozme:=zme;

	xme:=xme+et*speed*fspd*xlook;
	zme:=zme+et*speed*fspd*zlook;
	direction:=1;

	-- Velocity Direction
	veldir := fmath.arctan(xlook,zlook);

	-- set yme appropriately:
	if scene=2 then
		yme := aheight;
	elsif not interior then
		yme := aheight + land_alt(xme,zme);
	else
		yme := -iymax+aheight;
	end if;




	if scene=2 then -- (x,z) in [-10..0, -10..0]

		if (xme>-margin) then xme:=-margin; end if;
		if (xme<-10.0+margin) then xme:=-10.0+margin; end if;
		if (zme>-margin) then zme:=-margin; end if;
		if (zme<-10.0+margin) then zme:=-10.0+margin; end if;

	elsif interior then

		-- limit pos to be within walls:
		if (xme>+ixmx-margin) then xme:=+ixmx-margin; end if;
		if (xme<-ixmx+margin) then xme:=-ixmx+margin; end if;
		if (zme>+izmax-margin) then zme:=+izmax-margin; end if;
		if (zme<-izmax+margin) then zme:=-izmax+margin; end if;

	else --exterior scene

		-- limit pos to be within user bounds:
		if (xme>+xmaxu-margin) then xme:=+xmaxu-margin; end if;
		if (xme<-xmaxu+margin) then xme:=-xmaxu+margin; end if;
		if (zme>+zmaxu-margin) then zme:=+zmaxu-margin; end if;
		if (zme<-zmaxu+margin) then zme:=-zmaxu+margin; end if;

	end if;


	-- set yhalf appropriately:
	if scene=2 then
		yyme:=aheight;
		yhalf:=yyme-aheight/2.0;
	elsif scene=1 then
		yyme:=aheight;
		yhalf:=yyme-aheight/2.0;
	else
		yyme:=-iymax+aheight;
		yhalf:=yyme-aheight/2.0;
	end if;





	-- further, limit pos to avoid ko zones:
	for i in 1..nko loop
	if scene=koscene(i) and i/=kgate then --this KO applies here

		kxl:=xme-koxlo(i)+margin;
		kxh:=koxhi(i)+margin-xme;

		kyl:=yhalf-koylo(i);
		kyh:=koyhi(i)-yhalf;

		kzl:=zme-kozlo(i)+margin;
		kzh:=kozhi(i)+margin-zme;


		if (kxl*kxh>0.0) and (kyl*kyh>0.0) and (kzl*kzh>0.0) then 
		--intrusion into ko

			okxl:=oxme-koxlo(i)+margin;
			okxh:=koxhi(i)+margin-oxme;
			okzl:=ozme-kozlo(i)+margin;
			okzh:=kozhi(i)+margin-ozme;

--			if      ( (okxl*okxh>0.0) and (okzl*okzh<=0.0) ) then
--				zme:=ozme;
--			elsif ( (okzl*okzh>0.0) and (okxl*okxh<=0.0) ) then
--				xme:=oxme;
--			end if;

			--improvement 10apr21:
			if      ( (okxl*okxh>0.0) and (okzl*okzh<=0.0) ) then
				zme:=ozme;
			end if;

			if ( (okzl*okzh>0.0) and (okxl*okxh<=0.0) ) then
				xme:=oxme;
			end if;


		end if; --intrusion into ko

	end if;
	end loop; --for i

--------- addendum 19apr21 begin ---------------------------------------
	if scene=1 then --exterior...check versus tree trunks
		if min6(0.09) then -- txc,tzc are defined as closest to (xme,zme)
			dx := xme-txc;
			dz := zme-tzc;
			kt2 := float(dx*dx+dz*dz);
			if kt2 < 0.09 then -- 0.09 = 0.3^2
				fac:=fmath.sqrt(0.09/kt2);
				xme:=txc+fac*dx;
				zme:=tzc+fac*dz; --move radially away from tree
			end if;
		end if;
	end if;
--------- addendum 19apr21 end ---------------------------------------

	updategamestate;

end if;

end moveforward;


