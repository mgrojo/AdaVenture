separate(gameutils)

procedure updategamestate is

	gateang,
	cupang, swordang, wkeyang,bkeyang,gkeyang: float := 1.0;
	minonear, firenear: boolean := false;

	wang,dxz,dy,dx,dz,d2gate,
	xluk,yluk,zluk, xmi,ymi,zmi: float;

	-- (xmin9..xmax9, zmin9..zmax9) = secret chalice room
	x9ok : constant boolean := (xmin9<xme) and (xme<xmax9);
	z9ok : constant boolean := (zmin9<zme) and (zme<zmax9);
begin

	if scene=9 and x9ok and z9ok and not play9 then
		snd4ada.playLoop(tmpl4);
		play9:=true;
	end if;

	if thirdPerson then
		xluk:=cxlook;
		yluk:=cylook;
		zluk:=czlook;
		xmi:=xcam;
		ymi:=ycam;
		zmi:=zcam;
	else
		xluk:=xlook;
		yluk:=ylook;
		zluk:=zlook;
		xmi:=xme;
		ymi:=yme;
		zmi:=zme;
	end if;

	if scene=8 then

		if not minotaurdead then

			minonear:=(abs(xme)<5.5) and (abs(zme+1.0)<4.5); --21jan23 update

			--if minonear and not minosent then 21jan23 attention!!!
			if minonear and not dangerMusic then
				snd4ada.playLoop(dang8); dangerMusic:=true;
				minorun:=true;
				--dragonstart:=glfwGetTime; 21jan23 attention!!!
				--minosent:=true;
			end if;

			if not minonear and dangerMusic then
				snd4ada.stopLoop(dang8); dangerMusic:=false;
				minorun:=false;
			end if;


		end if;

-----------------------------------------------------------------

		if minotaurdead and play9 then --return trip thru M8 (fireBall)

			firenear:=( zme <= -8.0 );

			if firenear and not dangerMusic then
				snd4ada.playLoop(dang8); dangerMusic:=true;

			elsif not firenear and dangerMusic then
				snd4ada.stopLoop(dang8); dangerMusic:=false;

			end if;

		end if;


	elsif scene=1 then

		gatenear:=(abs(xme-xgzm)<neargate*1.5) and (abs(zme-zgzm)<neargate*1.5);

		if wkeyheld and gatenear and not opengate then liftgate(gzmk); end if;

		opengate := opengate or (wkeyheld and gatenear);

------- now deal with opening maze

		mazenear :=
			(abs(xme-xmaze)<neargate*1.8) and (abs(zme-zmaze)<neargate*1.8);
		if gkeyheld and mazenear and not openmaze then liftmaze; end if;
		openmaze := openmaze or (gkeyheld and mazenear);



	elsif scene=6 then

		lionnear :=
			(abs(xme-xlion)<neargate*1.8) and (abs(zme-zlion)<neargate*1.8);

		if bkeyheld and lionnear and not openlion then liftlion; end if;

		openlion := openlion or (bkeyheld and lionnear);


	elsif scene=7 then

		labnear :=
			(
				(abs(xme-xlab)<neargate*1.8)  --"H", sh7 (xlab~10)
					--or (abs(xme+xlab)<neargate*1.8) "G", sg7 (defunct)
			)
			
			and (abs(zme-zlab)<neargate*1.8); -- zlab~7

		if bkeyheld and labnear and not openlab then slidelab; end if;

		openlab := openlab or (bkeyheld and labnear);


	end if;





	if not chaliceheld then
		cupang := angl(xluk,yluk,zluk, 
			float(xchalice)-xmi,
			float(ychalice)-ymi,
			float(zchalice)-zmi);
	end if;

	if not wkeyheld then
		wkeyang := angl(xluk,yluk,zluk, xwkey-xmi,  ywkey-ymi, zwkey-zmi);
	end if;

	if not bkeyheld then
		bkeyang := angl(xluk,yluk,zluk, xbkey-xmi,  ybkey-ymi, zbkey-zmi);
	end if;

	if not gkeyheld then
		gkeyang := angl(xluk,yluk,zluk, xgkey-xmi,  ygkey-ymi, zgkey-zmi);
	end if;

	if not swordheld then
		swordang :=angl(xluk,yluk,zluk, xsword-xmi,ysword-ymi,zsword-zmi);
	end if;


	if not gateheld then
		gateang :=angl(xluk,yluk,zluk, xgate-xmi,ygate-ymi,zgate-zmi);
		dx := xgate-xme;
		dz := zgate-zme;
		d2gate := safeSqrt(dx*dx+dz*dz);
	end if;



	wkeyseen := ( -- used in bat test
		not wkeyheld
		and (abs(xme-xwkey)<nearkey*2.0)
		and (abs(zme-zwkey)<nearkey*2.0) 
		and (wkeyang<thirdpi) );

	bkeyseen := ( -- used in bat test
		not bkeyheld
		and (abs(xme-xbkey)<nearkey*2.0)
		and (abs(zme-zbkey)<nearkey*2.0) 
		and (bkeyang<thirdpi) );


	gkeynear := (
		not gkeyheld
		and (sgkey=scene)
		and (abs(xme-xgkey)<nearkey)
		and (abs(zme-zgkey)<nearkey) 
		and (gkeyang<thirdpi) );

	bkeynear := (
		not bkeyheld
		and (sbkey=scene)
		and (not bathasbkey)
		and (abs(xme-xbkey)<nearkey)
		and (abs(zme-zbkey)<nearkey) 
		and (bkeyang<thirdpi) );

	wkeynear := (
		not wkeyheld
		and (swkey=scene)
		and (not bathaswkey)
		and (abs(xme-xwkey)<nearkey)
		and (abs(zme-zwkey)<nearkey) 
		and (wkeyang<thirdpi) );

	swordnear := (
		not swordheld
		and (ssword=scene)
		and (abs(xme-xsword)<nearsword)
		and (abs(zme-zsword)<nearsword) 
		and (swordang<thirdpi) );



	chalicenear := (
		not chaliceheld
		and (schalice=scene)
		and (not bathaschalice)
		and (abs(xme-float(xchalice))<nearchalice)
		and (abs(zme-float(zchalice))<nearchalice) 
		and (cupang<thirdpi) );

	pedestalnear := -- pedestal is @ (x,z)=(0,0)
		(   (abs(xme-float(xped))<nearpedestal) 
		and (abs(zme-float(zped))<nearpedestal) 
		and (scene=2)
		and (abs(yluk)<0.5) ); -- looking near horizontal


	pgatenear := ( -- Portable Bridge/Gate
		not gateheld
		and (sgate=scene)
		and (d2gate<nearpgate)
		and ( (d2gate>toonearpgate) or (kgate=0) )
		and (gateang<thirdpi/2.0) );


	if gateheld then

		insertable:=0;
		for i in 1..nko loop
		if scene=koscene(i) then --this KO applies here

	-- this strategy for placing gateway thru wall requires 
	-- being close enough AND looking toward wall:
			if mazewall(i) then

				dx:=0.5*(koxlo(i)+koxhi(i))-xme;
				dy:=0.5*(koylo(i)+koyhi(i))-yme;
				dz:=0.5*(kozlo(i)+kozhi(i))-zme;
				dxz := safeSqrt( dx*dx+dz*dz );

				wang :=angl(xluk,yluk,zluk, dx,dy,dz);

				if 
					(wang<thirdpi/2.0)
					and
					(dxz<mazeThreshold)
				then
					insertable:=i;
				end if;

			end if;

		end if;
		end loop;

	end if; --gateheld


end updategamestate;


