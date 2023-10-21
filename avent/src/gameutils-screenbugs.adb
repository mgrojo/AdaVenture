separate(gameutils)


procedure screenBugs( nowtime: float ) is

	beet, obeet, ebeet: integer;
	angvar, hpos, vpos0, dv: float;

	sz: constant integer := 90;

	nbh: constant integer := 18; --12; --even
	nbv: constant integer := 12; -- 8;

begin --bugs go upward across the screen

	angvar:=fmath.sin(10.0*nowTime);
	if angvar<-0.3 then obeet:=lfbeet; ebeet:=rtbeet;
	elsif angvar>0.3 then obeet:=rtbeet; ebeet:=lfbeet;
	else obeet:=upbeet; ebeet:=upbeet; end if;

	dv := (0.25*nowTime*bugspeed); --slower than on floor

	for i in 1..nbh loop
	for j in 1..nbv loop

		hpos:=float(i)/float(nbh+1);

		vpos0 := float(j)/float(nbv);
		if i mod 2 = 0 then vpos0:=vpos0+0.5/float(nbv); end if;

		if i mod 2 = 0 then beet:=ebeet; else beet:=obeet; end if;
		utex.print2d(beet, hpos, fmod(vpos0+dv,1.0), sz);

	end loop;
	end loop;

end screenBugs;


