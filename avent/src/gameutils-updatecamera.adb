separate(gameutils)


procedure updateCamera( init: boolean := false )  is
	ixmx, kxl,kxh,kyl,kyh,kzl,kzh, tt: float;
	ixgoal,iygoal,izgoal,xc,yc,zc,ff: float;

	-- camera  buffer  distance:  bigger than the
	-- offset of wall textures from room bounds,
	-- yet smaller than margin:
	buf: constant float := 0.05; 

	s_slewrate: constant float := 0.005; --Slowest slew
	m_slewrate: constant float := 0.01; --Medium
	h_slewrate: constant float := 0.03; --High
	f_slewrate: constant float := 0.15; --Fastest slew
	-- % towards goal each update (5-10-15 seems ok)


	--note:  we must prevent KO in between *cam, *me
	okcam: boolean := true;
	nc : constant integer := 5;
	c: integer;
	-- # cuts of segment between "me" and "camera"

	hoffsetu, voffsetu: float; -- h=0.8, v=0.1

	amazed: constant boolean := (scene>=5 and scene<=9);

	zooming: boolean;

begin


	zooming:=false;
	if camdist/=ocamdist then 
		zooming:=true; 
		ocamdist:=camdist;
	end if;



	-- 1st, check camera versus scene bounds:

	-- set interior X-bounds
	if scene=4 then
		ixmx:=ixmax/3.0;
	else
		ixmx:=ixmax;
	end if;


	--adjust per zoom setting
	voffsetu:=camdist*voffset; --above avatar
	hoffsetu:=camdist*hoffset; --behind avatar
	if amazed then voffsetu:=camdist*voffset*4.0; end if;


	-- initialize ideal camera position:
	tt:=1.0;
	ixcam:=xme - tt*hoffsetu*xlook;
	izcam:=zme - tt*hoffsetu*zlook;
	iycam:=yme + tt*voffsetu;


	--if interior then adjust ideal until within room bounds:
	if scene=2 then -- (x,z) in [-10..0, -10..0]

		loop
			ixcam:=xme - tt*hoffsetu*xlook;
			izcam:=zme - tt*hoffsetu*zlook;
			iycam:=yme + tt*voffsetu;
			exit when 
				(iycam<iymax-buf) and 
				(abs(ixcam-x2c)<x2r-buf) and 
				(abs(izcam-z2c)<z2r-buf);
			tt:=tt*0.9;
		end loop;

	elsif interior then -- inside some maze/lab

		loop
			ixcam:=xme - tt*hoffsetu*xlook;
			izcam:=zme - tt*hoffsetu*zlook;
			iycam:=yme + tt*voffsetu;
			exit when 
				(iycam<iymax-buf) and 
				(abs(ixcam)<ixmx-buf) and 
				(abs(izcam)<izmax-buf);
			tt:=tt*0.9;
			exit when tt<0.01; 
			--this happens when going thru maze and we
			--transition between 5 & 6.
		end loop;

	end if;

	ixgoal:=ixcam;
	iygoal:=iycam;
	izgoal:=izcam;

	-- adjust ideal camera position versus ko zones:
	c:=0;
	while okcam loop

		ff:=float(c)/float(nc); --move from avatar towards ideal
		xc:= xme*(1.0-ff) + ixgoal*ff;
		yc:= yme*(1.0-ff) + iygoal*ff;
		zc:= zme*(1.0-ff) + izgoal*ff;

		okcam:=true;
		for i in 1..nko loop -- check all KOs that apply
		if scene=koscene(i) and i/=kgate then --this KO applies here
			kxl:=xc-(koxlo(i)-buf);
			kxh:=(koxhi(i)+buf)-xc;
			kyl:=yc-(koylo(i)-buf);
			kyh:=(koyhi(i)+buf)-yc;
			kzl:=zc-(kozlo(i)-buf);
			kzh:=(kozhi(i)+buf)-zc;
			if (kxl*kxh>0.0) and (kyl*kyh>0.0) and (kzl*kzh>0.0) then 
				okcam:=false;
			end if; --intrusion into ko
		end if;
		end loop; --for i

		if okcam then -- this standoff is valid
			ixcam:=xc; iycam:=yc; izcam:=zc;
		end if;

		c:=c+1;
		exit when c>nc;

	end loop; --while not okcam




	if init or not thirdPerson then
		xcam:=ixcam; ycam:=iycam; zcam:=izcam;
		-- need these initialized in case user 
		-- switches to 3rd person before moving,
		-- but also when entering new room...
	end if;

	cylook := fmath.sin(vertang);
	cxlook := fmath.cos(vertang)*fmath.sin(choriang);
	czlook := fmath.cos(vertang)*fmath.cos(choriang);



if lazyCam then

	--if direction=0 then
	if direction=0 and not zooming then
		slewToAv; --per AG

	elsif direction<0 then --moving backward

		if scene=2 or scene=4 then --temple or castle...loose camera
			-- s_slewrate=0.005
			xcam := (1.0-s_slewrate)*xcam + s_slewrate*ixcam;
			zcam := (1.0-s_slewrate)*zcam + s_slewrate*izcam;
			ycam := (1.0-s_slewrate)*ycam + s_slewrate*iycam; 
			slewToAv;

		elsif interior then --mazes...
			-- s_slewrate=0.005 Ok now with hiview
			xcam := (1.0-s_slewrate)*xcam + s_slewrate*ixcam;
			zcam := (1.0-s_slewrate)*zcam + s_slewrate*izcam;
			ycam := (1.0-s_slewrate)*ycam + s_slewrate*iycam; 
			slewToAv;

		else --exterior...fairly tight
			-- m_slewrate=0.01
			xcam := (1.0-m_slewrate)*xcam + m_slewrate*ixcam;
			zcam := (1.0-m_slewrate)*zcam + m_slewrate*izcam;
			ycam := (1.0-m_slewrate)*ycam + m_slewrate*iycam; 
			slewToAv;

		end if;

	--elsif direction>0 then 
	elsif direction>0 or zooming then 
	--fairly tight camera, particularly in mazes

		-- was using 0.08 but
		-- h_slewrate=0.03 good, looser Ok with hiview & slewToAvLook
		xcam := (1.0-h_slewrate)*xcam + h_slewrate*ixcam;
		zcam := (1.0-h_slewrate)*zcam + h_slewrate*izcam;
		ycam := (1.0-h_slewrate)*ycam + h_slewrate*iycam; 
		slewToAvLook; --more demanding than slewToAv
		--slewToAv; --27jan18

	end if;

else -- not lazyCam

	--if direction=0 then --not moving
	if direction=0 and not zooming then --neither moving nor zooming
		slewToAv; --per AG
	else --moving or zooming
		-- previous, good, method, forw+back:
		-- f_slewrate=0.15
		xcam := (1.0-f_slewrate)*xcam + f_slewrate*ixcam;
		zcam := (1.0-f_slewrate)*zcam + f_slewrate*izcam;
		ycam := (1.0-f_slewrate)*ycam + f_slewrate*iycam; 
		slewToAvLook;
	end if;

end if; -- not lazyCam




end updateCamera;



