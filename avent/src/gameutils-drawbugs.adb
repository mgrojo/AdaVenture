separate(gameutils)


procedure drawBugs( nowtime: float ) is

	dxx2,dzz2,dxx,dzz,angvar, angrot, bx,by,bz, dx,dz: float;

	bxc,bzc,bxr,bzr,bsx,bsz: float;

begin

if scene=6 or scene=7 then

	if scene=6 then

		bxc := 0.0;
		bzc := -7.5;
		bxr := 6.0; -- was 8.0; 10apr21
		bzr := 2.5;

	elsif scene=7 then

		bxc := -5.0;
		bzc := 8.0;
		bxr := 5.0;
		bzr := 3.0;

	end if;

	bsx := 2.0*bxr/float(mxbo);
	bsz := 2.0*bzr/float(mzbo);


	angrot:=halfpi;  --dominant heading
	angvar:= 0.1*fmath.sin(10.0*nowTime); --small sideways tilt

	dx:=-nowTime*fmath.sin(angrot)*bugspeed;
	dz:=-nowTime*fmath.cos(angrot)*bugspeed;
	dx := fmod(dx,2.0*bxr); -- dx in [0..2bxr]
	dz := fmod(dz,2.0*bzr); -- dz in [0..2bzr]

	--add slight sideways movement too:
	dxx := beetrr*fmath.cos(angrot+angvar-halfpi);
	dzz := beetrr*fmath.sin(angrot+angvar-halfpi);

	dxx2:= beetrr*fmath.cos(angrot-angvar-halfpi);
	dzz2:= beetrr*fmath.sin(angrot-angvar-halfpi);

	for i in 1..mxbo loop
	for j in 1..mzbo loop

		--initial positions in a grid
		bx:= bxc-float(mxbo/2)*bsx + float(i-1)*bsx;
		if odd(j) then bx:=bx+0.3*bsx; end if;
		by:= -iymax+0.03; --note: floor=-iymax+0.01
		bz:= bzc-float(mzbo/2)*bsz + float(j-1)*bsz;
		if odd(i) then bz:=bz+0.3*bsz; end if;

		--move bugs forward:
		bx := bx+dx;
		bz := bz+dz;

		--add small motion to side:
		if odd(i+j) then
			bx := bx+dxx;
			bz := bz+dzz;
		else
			bx := bx+dxx2;
			bz := bz+dzz2;
		end if;


		--check bounds
		if bx<bxc-bxr then bx:=bx+2.0*bxr;
		elsif bx>bxc+bxr then bx:=bx-2.0*bxr; end if;

		--   < -10             bz+4
		if bz<bzc-bzr then bz:=bz+2.0*bzr;
		--      > -6              bz-4
		elsif bz>bzc+bzr then bz:=bz-2.0*bzr; end if;

		--setpos
		if odd(i+j) then
		bugobj.setrect( beetleobj, 
			bx, by, bz,
			beetrr,beetrr, angrot+angvar);
		else
		bugobj.setrect( beetleobj, 
			bx, by, bz,
			beetrr,beetrr, angrot-angvar);
		end if;

		--draw
		glbindtexture(gl_texture_2d, bug_texid);
		bugobj.draw(beetleobj, vertbuff,uvbuff,elembuff);

	end loop;
	end loop;


end if; -- scene=6/7

end drawBugs;


