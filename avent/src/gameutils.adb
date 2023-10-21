
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


----------------------------------------------------------------
with interfaces;
with interfaces.c;


with matutils;  use matutils;
with gametypes;  use gametypes;

with ada.calendar;
with ada.calendar.time_zones;
with ada.calendar.formatting;
with ada.strings.fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;


with ada.directories;
with ada.characters.handling;
with ada.numerics; 
with text_io; use text_io;
with utex;

with gl;  use gl;

with glfw3; use glfw3;
with zoomwheel;


with cubemapobj;
with rectobj;
with pictobj;
with bugobj;
with avatarolay;

with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

with system;
with Interfaces.C;
use  type interfaces.c.unsigned;
with Interfaces.C.Pointers;
with interfaces.c.strings;

with pngloader;
with shader;

with snd4ada; --use snd4ada;



package body gameutils is

	use ada.calendar;
	use pngloader;
	use shader;

	dangerMusic: boolean := false; --true=>iBeat.wav playing (M8)



-- calculate floatingPoint remainder of floor(val/ub)
-- returns value in [0..ub]
function fmod( val, ub : float ) return float is
	--k : integer := integer(val/ub);
	k : float := float'floor(val/ub); --21may20
begin
	--return val - float(k)*ub;
	return val - k*ub; --21may20
end fmod;


function odd( i: integer ) return boolean is
begin
	return ( i mod 2 = 1 );
end odd;



function min( x,y: float ) return float is
begin
	if x<y then return x;
	else return y; end if;
end min;

function mini(i,j: integer) return integer is
begin
	if i<j then return i;
	else return j; end if;
end mini;


procedure myassert( 
	condition : boolean;  
	flag: integer:=0;
	msg: string := ""
	) is
begin
  if condition=false then
		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put( "@ " & integer'image(flag) &" : " );
		end if;
		put_line(msg);
		new_line;
		raise program_error;
  end if;
end myassert;




function max( x,y : float ) return float is
begin
	if y>x then return y;
	else return x; end if;
end;

function sqr( x:float ) return float is
begin
	return x*x;
end;

function dotprod( x,y,z, a,b,c : float ) return float is
begin
	return x*a + y*b + z*c;
end;









function hordistance( x1,y1, x2,y2 : float ) return float is
	arg: float := sqr(x2-x1) + sqr(y2-y1);
begin
	if arg>0.0 then
		return safeSqrt( arg );
	else
		return 0.0;
	end if;
end hordistance;

function signum( x : float ) return float is
begin
	if x>0.0 then
		return +1.0;
	elsif x<0.0 then
		return -1.0;
	else
		return 0.0;
	end if;
end signum;






	use interfaces.c;
	use interfaces.c.strings;
	use glext;
	use glext.pointers;
	use glext.binding;
	use gl;
	use gl.binding;
	use gl.pointers;



procedure InitGlfw( width, height : glint; name: string ) is separate;

procedure InitGlfwFs( name: string ) is separate;





package myfloat_io is new text_io.float_io(float);

package mygint_io is new text_io.integer_io(glint);


--we skip blank lines and comments beginning with "#":
function is_blank2( line : string; len:integer ) return boolean is
	ch: character;
begin

	if( len < 1 ) then return true; end if;

	ch:=line( line'first );

	if ch='#' then return true;
	elsif ch = ' ' then	return true;

	--4jul18 added safeguard to handle DOS-format files [ada2012]:
	--elsif ada.characters.handling.is_line_terminator(ch) then return true;

	--28oct21 added safeguard to handle DOS-format files (cr/lf):
	elsif character'pos(ch)=13 or character'pos(ch)=10 then return true;
	-- works in Ada prior to 2012

	end if;

	return false;

end is_blank2;






procedure GetInt( Rcd:string;
						Bgn:in out natural;
						Int: in out glint ) is
begin
mygint_io.get( From => Rcd(Bgn..Rcd'last),
					Item => Int,
					Last => Bgn);
Bgn:=Bgn+1;
end GetInt;



procedure getNbInt(tfile:file_type; rcd: in out string; k: in out glint) is
	len,bgn: natural:=1;
begin
	while not end_of_file(tfile) loop
		get_line(tfile, rcd, len);
		if not is_blank2(rcd,len) then exit; end if;
	end loop;
	bgn:=rcd'first;
	GetInt(rcd,bgn,k);
end getNbInt;


procedure GetFlt( Rcd:string;
					  Bgn:in out natural;
					  Flt: in out float ) is
nd: positive;
begin
myfloat_io.get( From => Rcd(Bgn..Rcd'last),
					 Item => Flt,
					 Last => nd);
Bgn := nd+1;
end GetFlt;


procedure getNbFlt(tfile:file_type; rcd: in out string; t: in out float) is
	len,bgn: natural:=1;
begin
	while not end_of_file(tfile) loop
		get_line(tfile, rcd, len);
		if not is_blank2(rcd,len) then exit; end if;
	end loop;
	bgn:=rcd'first;
	GetFlt(rcd,bgn,t);
end getNbFlt;










procedure first_prep(HiRes: boolean:=false) is separate;







--calculate the radian measure of the solid angle between two vectors
function angl( x1,y1,z1, x2,y2,z2 : float ) return float is
	l1 : constant float := x1*x1+y1*y1+z1*z1;
	l2 : constant float := x2*x2+y2*y2+z2*z2;
	len1, len2 : float;
	dot12, cosang : float;
begin
	if l1<0.0001 then len1:=0.01;
	else len1:=safeSqrt(l1); end if;

	if l2<0.0001 then len2:=0.01;
	else len2:=safeSqrt(l2); end if;

	dot12:=x1*x2+y1*y2+z1*z2;
	cosang := dot12/len1/len2;

	-- safeguard added 8jan18:
	if cosang>1.0 then cosang:=1.0;
	elsif cosang<-1.0 then cosang:=-1.0; end if;

	return fmath.arccos(cosang);
end angl;







--circular angle is reset to lie between -pi..+pi
function mindifangl( angle: float ) return float is
	angl: float := angle;
begin

	while angl > +onepi loop
		angl:=angl-twopi;
	end loop;

	while angl < -onepi loop
		angl:=angl+twopi;
	end loop;

	return angl;

end mindifangl;


-- choose camera angle closest to avatar-look-dir:
function branchnear( angAva, angCam : float ) return float is
begin 
	return angAva+mindifangl(angCam-angAva);
end branchnear;


--gradually slew choriang [camera] toward avatar's look-direction:
procedure slewToAvLook is -- use horiang to set cam-look, choriang
	ahoriang : float; -- look angle towards avatar
	ghoriang : float; -- ideal goal camera angle
	atan,dang: float;
begin

	if abs(xme-xcam)+abs(zme-zcam) < 0.001 then
		ahoriang:=horiang;
	else
		atan := fmath.arctan(xme-xcam,zme-zcam);
		ahoriang := branchnear(horiang,atan); --new
	end if;

	dang := mindifangl(horiang-ahoriang);
	ghoriang := ahoriang + 0.15*dang;
	-- reset camera goal to turn slightly [25%] toward horiang
	-- where horiang = avatar's walking direction

	dang := mindifangl(ghoriang-choriang);
	choriang := choriang + 0.02*dang;
	-- move 5% towards camera goal

	cxlook := fmath.cos(vertang)*fmath.sin(choriang);
	cylook := fmath.sin(vertang);
	czlook := fmath.cos(vertang)*fmath.cos(choriang);

exception -- added 8jan18

	when ada.numerics.argument_error =>

		new_line;
		put("slewToAvLook:  xme="&float'image(xme));
		put("slewToAvLook:  zme="&float'image(zme));
		new_line;

		put("slewToAvLook:  xcam="&float'image(xcam));
		put("slewToAvLook:  zcam="&float'image(zcam));
		new_line;

		put("slewToAvLook:  xme-xcam="&float'image(xme-xcam));
		put("slewToAvLook:  zme-zcam="&float'image(zme-zcam));
		new_line;

		raise;

end slewToAvLook;




--gradually slew choriang [camera] to look toward avatar
procedure slewToAv is -- uses me-look=horiang to set cam-look=choriang
	ahoriang : float; -- look angle towards avatar
	dang: float;
begin

	if abs(xme-xcam)+abs(zme-zcam) < 0.001 then
		ahoriang:=horiang;
	else
		ahoriang := branchnear(horiang,fmath.arctan(xme-xcam,zme-zcam));
	end if;

	dang := mindifangl(ahoriang-choriang);
	choriang := choriang + 0.02*dang; --move 2% toward goal

	cxlook := fmath.cos(vertang)*fmath.sin(choriang);
	cylook := fmath.sin(vertang);
	czlook := fmath.cos(vertang)*fmath.cos(choriang);


exception -- added 8jan18

	when ada.numerics.argument_error =>

		new_line;
		put("s2av:  xme="&float'image(xme));
		put("s2av:  zme="&float'image(zme));
		new_line;

		put("s2av:  xcam="&float'image(xcam));
		put("s2av:  zcam="&float'image(zcam));
		new_line;

		put("s2av:  xme-xcam="&float'image(xme-xcam));
		put("s2av:  zme-zcam="&float'image(zme-zcam));
		new_line;

		raise;

end slewToAv; --not used, but might want to sometime.





procedure updateCamera( init: boolean := false )  is separate;







-- this assumes mm=ID, (xme,yme,zme)=virtual pos within skybox
-- [ actual pos versus skybox is always (0,0,0) ]
procedure updateMVPs( 
	et: gldouble;
	wid,hit : float; upd8: boolean := false ) is separate;








procedure updategamestate is separate;





procedure liftgate( n: integer ) is
  -- n is to be removed from sequential list
  -- so we simply make it non-effectual
  -- then redefine gate, KO
begin
	myassert( n <= nko, 987 );
	myassert( (n=gxpk) or (n=gxmk) or (n=gzpk) or (n=gzmk), 986 );


	lifttime := glfwGetTime;

	gatewait:=true;
	snd4ada.playSnd(stone); --concrete

	if n=gxpk then
		xpup:=true;
	elsif n=gxmk then
		xmup:=true;
	elsif n=gzpk then
		zpup:=true;
	elsif n=gzmk then
		zmup:=true;
	end if;

end liftgate;







procedure slidelab is
begin

	labtime := glfwGetTime;

	labwait:=true;
	snd4ada.playSnd(stone); --concrete

	labopening:=true;

end slidelab;





procedure liftlion is
begin

	liontime := glfwGetTime;

	lionwait:=true;
	snd4ada.playSnd(stone); --concrete

	liongoingup:=true;

end liftlion;






procedure liftmaze is
begin

	mazetime := glfwGetTime;

	mazewait:=true;
	snd4ada.playSnd(stone); --concrete

	mazegoingup:=true;

end liftmaze;














procedure moveforward( currenttime: gldouble ) is separate;

procedure movebackward( currenttime: gldouble ) is separate;



procedure handle_gc_look( gcx,gcy,slu:float ) is separate;

procedure handle_gc_move( nowtime: gldouble; gcx,gcy:float ) is separate;



oldMdTime : gldouble := 0.0;
xold,yold: aliased gldouble := 0.0;

procedure handle_mouse_move( nowTime : gldouble ) is separate;















------------------ end game specific code -----------------------------


procedure output( a : mat44 ) is
begin
	for row in 1..4 loop
	for col in 1..4 loop
		put( float'image( a(row,col) ) &" ");
	end loop;
	new_line;
	end loop;
	new_line;
end;



------- begin addendum 5jan15 -------------------------------------------------







-- note that xmax,ymax,zmax = (20,20,20):
function land_alt( x,z : float ) return float is
	cycx: constant float := x*twopi/xmax;
	cycz: constant float := z*twopi/zmax;
	amp : constant float := ymax/50.0; --0.4
	alt, hdist : float;
begin


	alt := -0.1 + amp*( fmath.sin(cycx)+fmath.sin(cycz) )/2.0;

	-- special provision @ castle gate (x,y,z)=(-5,0,-10)
	hdist:=safeSqrt( sqr(-5.0-xme) + sqr(-10.0-zme) );

	-- want floor-level=zero @ doorway
	if hdist<5.0 then
		alt := alt*(hdist/5.0);
	end if;

	return alt;
end land_alt;


procedure sendBat is
begin
-- NOTE: define bat source=destination: (xbat,ybat,zbat)
--			if KEY (or sword)
--			...chalice destination coded elsewhere.
	batfly:=true;
	batstart:=glfwGetTime;
	-- ensure margin<1.0:
	if interior then

		--bat initial/final position...
		--(x,z) determine drop coords for M5/M6 only
		xbat:=0.0;        -- @center
		ybat:=iymax-0.1;  -- @top

		if scene=5 or scene=6 then
			--zbat:=-izmax+1.0; -- try drop near doorway (or southroom maze6)
			zbat := 2.0; --So, (x,z)=(0,2) works Ok either M5 or M6

		elsif scene=7 then --bat grabs cup; drops @ (0,y,0); see avent-draw_maze7
			--zbat:=0.0;
			null;
		else -- unhandled
			zbat:=0.0;
		end if;

	else -- far reaches of exterior:

		if chaliceheld then --exitting maze7; bat takes chalice
			--xbat:=+xmaxu-1.0=17; ybat:=ymax-1.0=19; zbat:=-zmaxu+1.0=-17;
			null;
			--see avent-draw_exterior; drops @ (3,17) or (-11.4,-2)

		else -- bat takes white key
			xbat:=-xmaxu+1.0; ybat:=ymax-1.0; zbat:=-zmaxu+1.0;
			-- (-17,19,-17)
			-- see avent-draw_exterior
		end if;

	end if;
end sendBat;

-- fly bat in a linear trajectory starting and ending
-- at a randomly chosen, but pickable (x,z) location
-- make the chalice/key-pickup sound, then disappear...
-- but key only if lying on ground and not being held.
-- In case of chalice (scene=7,1), bat grabs it anyway.
procedure drawbat( dt: gldouble ) is
	et : constant float := float(dt); -- -0.5 .. +0.5
	v4, vcc : vec4;
	xt,yt,zt, rtgt,dot, yt0,yt1 : float;
	mindot : constant float := fmath.cos(fourthpi);
	s : integer;
	nx: constant float := 2.0; -- #transitions per second
	i: integer;
	ets, xkey,ykey,zkey: float;
	rng : float;
	xx,yy,zz,xluk,yluk,zluk: float;
begin

	--addendum 2mar18
	if thirdPerson then
		xx:=xcam; yy:=ycam; zz:=zcam;
		xluk:=cxlook; yluk:=cylook; zluk:=czlook;
	else -- firstPerson
		xx:=xme; yy:=yme; zz:=zme;
		xluk:=xlook; yluk:=ylook; zluk:=zlook;
	end if;


	if not bat9sent then

		if scene=1 then
			xkey:=xwkey; ykey:=ywkey; zkey:=zwkey;
			ets:=et;
		elsif scene=5 then
			xkey:=xbkey; ykey:=ybkey; zkey:=zbkey;
			ets:=et*8.0;
		end if;

	end if;

	myassert( et>=-1.01, 985 );
	myassert( et<=+1.01, 984 );
	-- et in (-1..1)

	i := integer( float(batduration) * et * nx );
	if odd(i) then
		s:=bat1;
	else
		s:=bat2;
	end if;

	if 
		bat9sent -- scene=1 final grab of chalice
	or
		scene=7 
	then 

	-- cup-bat flys more like dragon...right at you
	-- and grabs chalice out of your hand...

		rng := abs(et)*10.0;
		if rng<0.5 then rng:=0.5; end if;
		--if bat9sent then
		if scene=1 then
			yt0 := yme + abs(et)*(ymax-yme);
		else
			yt0 := yme + abs(et)*(iymax-yme);
		end if;

		-- this trajectory approaches from exact [dynamic] look direction:
		xt :=xx+rng*xluk;
		yt1:=yy+rng*yluk;
		zt :=zz+rng*zluk;

		-- this trajectory mixes the two:
		yt:= abs(et)*yt0 + (1.0-abs(et))*yt1;

		rtgt := safeSqrt( sqr(xt-xx) + sqr(yt-yy) + sqr(zt-zz) );
		if rtgt<0.5 then rtgt:=0.5; end if;

		v4 := (xt,yt,zt,1.0);
		matXvec( imvp, v4, vcc );

		utex.print1a(s,
			vcc(1),vcc(2),
			vcc(3),vcc(4), 600, rtgt);

	else -- more typical bat flight with key (or sword):
		xt:=xkey+abs(ets)*(xbat-xkey);
		yt:=ykey+abs(ets)*(ybat-ykey);
		zt:=zkey+abs(ets)*(zbat-zkey);

		rtgt := safeSqrt( sqr(xt-xx) + sqr(yt-yy) + sqr(zt-zz) );
		dot := (xt-xx)*xluk + (yt-yy)*yluk + (zt-zz)*zluk;
		v4 := (xt,yt,zt,1.0);
		matXvec( imvp, v4, vcc );

		if (dot/rtgt > mindot) then
			utex.print1a(s,
				vcc(1),vcc(2),
				vcc(3),vcc(4), 600, rtgt);
		end if;

	end if;


end drawbat;







procedure drawspider( dt: gldouble ) is
	et : constant float := float(dt);
	v4, vcc : vec4;
	xt,yt,zt, rtgt,dot : float;
	mindot : constant float := fmath.cos(fourthpi);
	ym: constant float := htobj;
	yx: constant float := iymax-htobj;
	yc: constant float := 0.5*(yx+ym);
	yr: constant float := 0.5*(yx-ym);
	tt,xx,yy,zz,xluk,yluk,zluk: float;
begin

if scene=2 then

	--addendum 2mar18
	if thirdPerson then
		xx:=xcam; yy:=ycam; zz:=zcam;
		xluk:=cxlook; yluk:=cylook; zluk:=czlook;
	else -- firstPerson
		xx:=xme; yy:=yme; zz:=zme;
		xluk:=xlook; yluk:=ylook; zluk:=zlook;
	end if;


	tt := fmath.sin(et*0.1); -- in [-1..1]

	xt:=-9.95;
	zt:=-0.05;
	yt:=yc+tt*yr;

	rtgt := safeSqrt( sqr(xt-xx) + sqr(yt-yy) + sqr(zt-zz) );
	dot := (xt-xx)*xluk + (yt-yy)*yluk + (zt-zz)*zluk;
	v4 := (xt,yt,zt,1.0);
	matXvec( imvp, v4, vcc );

	if (dot/rtgt > mindot) then
		utex.print1a(spider,
			vcc(1),vcc(2),
			vcc(3),vcc(4), 100, rtgt);
	end if;

end if; -- scene=2

end drawspider;
























-- 7sep16 revision:  Once the dragon is seen,
-- you cannot turn away...he approaces from
-- whatever your look direction may be !!!
procedure drawBdragon( dt: gldouble ) is -- 0<et<1
	et : constant float := float(dt);
	v4, vcc : vec4;
	yt0,
	xt1,yt1,zt1,
	xt,yt,zt : float;
	s : integer;
	rng : float := (1.0-et)*20.0;
	ydra,
	xx,yy,zz,xluk,yluk,zluk: float;

begin

	--addendum 2mar18
	if thirdPerson then
		xx:=xcam; yy:=ycam; zz:=zcam;
		xluk:=cxlook; yluk:=cylook; zluk:=czlook;
	else -- firstPerson
		xx:=xme; yy:=yme; zz:=zme;
		xluk:=xlook; yluk:=ylook; zluk:=zlook;
	end if;


	if interior then ydra:=iymax*0.5;
	else ydra:=ymax; end if;

	myassert( et>=-0.01, 983 );
	myassert( et<=+1.01, 982 );
	-- et in (0..1)

	if rng<0.3 then rng:=0.3; end if;
	if et>0.9 then
		s:=blakil;
	else
		s:=bladra;
	end if;



	-- this trajectory approaches from a fixed point (xdra,ydra,zdra) 
	-- above the horizon chosen at the initial look direction:
	--xt0:=xdra+et*(xx-xdra);
	yt0:=ydra+et*(yy-ydra);
	--zt0:=zdra+et*(zz-zdra);

	-- this trajectory approaches from exact [dynamic] look direction:
	xt1:=xx+rng*xluk;
	yt1:=yy+rng*yluk;
	zt1:=zz+rng*zluk;

	-- this trajectory mixes the two:
	yt:= et*yt1 + (1.0-et)*yt0;

	xt:=xt1;
	zt:=zt1;

	v4 := (xt,yt,zt,1.0);
	matXvec( imvp, v4, vcc );

	utex.print1a(s,
		vcc(1),vcc(2),
			vcc(3),vcc(4), 400, rng);

end drawBdragon;










-- 7sep16 revision:  Once the dragon is seen,
-- you cannot turn away...he approaces from
-- whatever your look direction may be !!!
procedure drawRdragon( dt: gldouble ) is -- 0<et<1
	et : constant float := float(dt);
	v4, vcc : vec4;
	yt0,
	xt1,yt1,zt1,
	xt,yt,zt : float;

	s : integer;

	rng : float := (1.0-et)*5.0;
	ydra,
	xx,yy,zz,xluk,yluk,zluk: float;

begin

	--addendum 2mar18
	if thirdPerson then
		xx:=xcam; yy:=ycam; zz:=zcam;
		xluk:=cxlook; yluk:=cylook; zluk:=czlook;
	else -- firstPerson
		xx:=xme; yy:=yme; zz:=zme;
		xluk:=xlook; yluk:=ylook; zluk:=zlook;
	end if;


	if interior then ydra:=iymax*0.5;
	else ydra:=ymax; end if;

	myassert( et>=-0.01, 981 );
	myassert( et<=+1.01, 980 );
	-- et in (0..1)

	if rng<0.3 then rng:=0.3; end if;
	if et>0.9 then
		s:=redkil; --r2;
	else
		s:=reddra; --r1;
	end if;


	-- this trajectory approaches from a fixed point (xdra,ydra,zdra) 
	-- above the horizon chosen at the initial look direction:
	--xt0:=xdra+et*(xx-xdra);
	yt0:=ydra+et*(yy-ydra);
	--zt0:=zdra+et*(zz-zdra);

	-- this trajectory approaches from exact [dynamic] look direction:
	xt1:=xx+rng*xluk;
	yt1:=yy+rng*yluk;
	zt1:=zz+rng*zluk;

	-- this trajectory mixes the two:
	yt:= et*yt1 + (1.0-et)*yt0;

	xt:=xt1;
	zt:=zt1;

	v4 := (xt,yt,zt,1.0);
	matXvec( imvp, v4, vcc );

	utex.print1a(s,
		vcc(1),vcc(2),
			vcc(3),vcc(4), 400, rng);

end drawRdragon;













procedure drawMinotaur( dt: gldouble ) is -- 0<et<1
	et : constant float := float(dt);
	v4, vcc : vec4;
	yt0,
	xt1,yt1,zt1,
	xt,yt,zt : float;
	rng : float := (1.0-et)*20.0;
	ydra,
	xx,yy,zz,xluk,yluk,zluk: float;

begin

	--addendum 2mar18
	if thirdPerson then
		xx:=xcam; yy:=ycam; zz:=zcam;
		xluk:=cxlook; yluk:=cylook; zluk:=czlook;
	else -- firstPerson
		xx:=xme; yy:=yme; zz:=zme;
		xluk:=xlook; yluk:=ylook; zluk:=zlook;
	end if;


	ydra:=iymax*0.5;

	myassert( et>=-0.01, 979 );
	myassert( et<=+1.01, 978 );
	-- et in (0..1)

	if rng<0.3 then rng:=0.3; end if;

	-- this trajectory approaches from a fixed point (xdra,ydra,zdra) 
	-- above the horizon chosen at the initial look direction:
	--xt0:=xdra+et*(xme-xdra);
	yt0:=ydra+et*(yy-ydra);

	-- this trajectory approaches from exact [dynamic] look direction:
	xt1:=xx+rng*xluk;
	yt1:=yy+rng*yluk;
	zt1:=zz+rng*zluk;

	-- this trajectory mixes the two:
	yt:= et*yt1 + (1.0-et)*yt0;

	xt:=xt1;
	zt:=zt1;

	v4 := (xt,yt,zt,1.0);
	matXvec( imvp, v4, vcc );

	utex.print1a(gnu,
		vcc(1),vcc(2),
			vcc(3),vcc(4), 500, rng);

end drawMinotaur;










procedure initializeNewMazes is separate;

-- note transition table below

-- scene		no		so		ea									we

-- 	5		x		in		6Y(5,-10,9)->(6,+10,-4)		6C(5,10,9)->(6,-10,9)
-- 	5		x		in		6Z(5,-10,4)->(6,+10,-9)		6D(5,10,4)->(6,-10,4)

-- 	5		x		in		6V(5,-10,2)->(6,+10,2)		6E(5,+10,2)->(6,-10,2)
-- 	5		x		in		6W(5,-10,0)->(6,+10,0)		6F(5,+10,0)->(6,-10,0)
-- 	5		x		in		6X(5,-10,-2)->(6,+10,-2)	6G(5,+10,-2)->(6,-10,-2)



--		6		out	x		5C(6,-10,9)->(5,+10,9)		6A(6,+10,+9)->(6,-10,-4)
--		6		out	x		5D(6,-10,4)->(5,+10,4)		6B(6,+10,+4)->(6,-10,-9)

--		6		out	x		5E(6,-10,2)->(5,+10,2)		5V(6,+10,+2)->(5,-10,+2)
--		6		out	x		5F(6,-10,0)->(5,+10,0)		5W(6,+10,0)->(5,-10,0)
--		6		out	x		5G(6,-10,-2)->(5,+10,-2)	5X(6,+10,-2)->(5,-10,-2)

--		6		out	x		6A(6,-10,-4)->(6,+10,+9)	5Y(6,+10,-4)->(5,-10,+9)
--		6		out	x		6B(6,-10,-9)->(6,+10,+4)	5Z(6,+10,-9)->(5,-10,+4)

-- denote transition ID by (ea/we, initial-scene, letter):
--
-- {ea,we} + {5,6} + {a,b,c,d,e,f,g, v,w,x,y,z}
-- so ...
-----------------------------------------
-- ea5y: x+20, z-13
-- ea5z: x+20, z-13
-- ea5v: x+20
-- ea5w: x+20
-- ea5x: x+20
--
-- we5c: x-20
-- we5d: x-20
-- we5e: x-20
-- we5f: x-20
-- we5g: x-20
----------------------------------
-- ea6c: x+20
-- ea6d: x+20
-- ea6e: x+20
-- ea6f: x+20
-- ea6g: x+20
-- ea6a: x+20, z+13
-- ea6b: x+20, z+13
--
-- we6a: x-20, z-13
-- we6b: x-20, z-13
-- we6v: x-20
-- we6w: x-20
-- we6x: x-20
-- we6y: x-20, z+13
-- we6z: x-20, z+13








function snakehiss return boolean is
	tooclose : constant float := 1.0;
	dist : float;
begin

	if scene=7 then
	  dist:=sqr(x7snake-xme)+sqr(z7snake-zme);
	elsif scene=8 then
	  dist:=sqr(x8snake-xme)+sqr(z8snake-zme);
	elsif scene=6 then
	  dist:=sqr(x6snake-xme)+sqr(z6snake-zme);
	elsif scene=5 then
	  dist:=sqr(x5snake-xme)+sqr(z5snake-zme);
	else
		dist:=999.0;
	end if;

	if dist<4.0*tooclose*tooclose then
		return true;
	else
		return false;
	end if;

end snakehiss;




function nearsnake return boolean is
	tooclose : constant float := 1.0;
	dist : float;
begin

	if scene=7 then
	  dist:=sqr(x7snake-xme)+sqr(z7snake-zme);
	elsif scene=8 then
	  dist:=sqr(x8snake-xme)+sqr(z8snake-zme);
	elsif scene=6 then
	  dist:=sqr(x6snake-xme)+sqr(z6snake-zme);
	elsif scene=5 then
	  dist:=sqr(x5snake-xme)+sqr(z5snake-zme);
	else
		dist:=999.0;
	end if;

	if dist<tooclose*tooclose then
		return true;
	else
		return false;
	end if;

end nearsnake;



-- for when we have to draw objects in order from far to near
-- note that bubble sort is actually very efficient for short lists.
procedure sort( 
	f2n: in out sortarray;  -- output permutations
	ox,oz : limarray;       -- pos of each object
	lo, hi : integer;       -- bounds of sort
	eyex,eyez : float       -- xme, zme
	) is

	isave : integer;
	di, dj : float;
begin

	myassert( lo>=nsortrng'first, 977 );
	myassert( hi<=nsortrng'last, 976  );

	for i in lo..hi loop
		f2n(i):=i; -- initialize permutation
	end loop;

	for i in reverse lo..hi loop
		for j in 1..i loop

			di := sqr( ox( f2n(i) ) - eyex ) + sqr( oz( f2n(i) ) - eyez );
			dj := sqr( ox( f2n(j) ) - eyex ) + sqr( oz( f2n(j) ) - eyez );
			if( di > dj ) then -- swap i,j
				isave:=f2n(i);
				f2n(i):=f2n(j);
				f2n(j):=isave;
			end if;

		end loop; -- for j
	end loop; -- for i

end sort;














procedure emptyGLerrorQueue is
	use gl.binding;
begin
	while glGetError /= gl_no_error loop
		null;
	end loop;
end emptyGLerrorQueue;



function dumpGLerrorQueue(id: string) return integer is
	use gl.binding;
	errno: interfaces.c.unsigned;
	isAnError: boolean;
	ercount: integer := 0;
begin
	isAnError:=false;
	loop
		errno:=glGetError;
		exit when errno=gl_no_error;
		ercount:=ercount+1;
		isAnError:=true;
		put("GLerror=");
		put(interfaces.c.unsigned'image(errno));
		new_line;
	end loop;
	if isAnError then
		put_line("...@ id="&id);
	end if;
	return ercount;
end dumpGLerrorQueue;
--
-- 16#0#   =    0 = no_error
-- 16#500# = 1280 = invalid_enum
-- 16#501# = 1281 = invalid_value
-- 16#502# = 1282 = invalid_operation ?reusing uniformID?
-- 16#503# = 1283 = stack_overflow
-- 16#504# = 1284 = stack_underflow
-- 16#505# = 1285 = out_of_memory
--








-------------- begin generic crossing function ----------
oldcrossing: float := 0.0;


function atThreshold(	
	timenow: gldouble; 
	str: string;
	sene: integer ) return boolean is

	near: constant float := here;
	dist : float;
	dt : float := float(timenow) - oldcrossing;
	dirOk: boolean := false;

	mazr: mazeRec;
	xx,zz,passdir: float;
begin


	mazr:=mtab.find(str);
	xx:=mazr.x;
	zz:=mazr.z;
	passdir:=mazr.dir;


	-- check velocity direction
	if abs(mindifangl(veldir-passdir))<fourthpi then
		dirOk:=true;
	else
		dirOk:=false;
	end if;

	if dt>repassage then
		if (scene/=sene) then return false;
		else
			dist := safeSqrt( sqr(xx-xme) + sqr(zz-zme) );
			if dist<near and dirOk then
				oldcrossing:=float(timenow);
				return true;
			else
				return false;
			end if;
		end if;
	else
		return false;
	end if;

end atThreshold;



procedure showWhatIsHeld is
begin

		-- glyphs drawn @ screen center
			showingGlyph:=false;

		if gateheld then
			if insertable>0 then
				utex.print2d(gatner,0.5,0.5,50); -- "^" => gateway
			else
				utex.print2d(gatfar,0.5,0.5,50); -- "^" => gateway
			end if;
			showingGlyph:=true;

		elsif gkeyheld then --draw key
			utex.print2d(grekey,0.5,0.5,50); -- "^" => green key
			showingGlyph:=true;

		elsif wkeyheld then --draw key
			utex.print2d(whikey,0.5,0.5,50); -- "~" => white key
			showingGlyph:=true;

		elsif bkeyheld then --draw key
			utex.print2d(blakey,0.5,0.5,50); -- "|" => black key
			showingGlyph:=true;

		elsif swordheld then --draw sword
			utex.print2d(csword,0.5,0.5,120); -- "=" => sword
			showingGlyph:=true;

		elsif chaliceheld then --draw chalice
			utex.print2d(chalis,0.5,0.5,80); -- ";" => chalice
			showingGlyph:=true;
		end if;

		if 
			(pgatenear or gkeynear or bkeynear or 
			 wkeynear or swordnear or chalicenear)
			and not gatewait and not lionwait and not mazewait
		then
			utex.print2d(hand,0.5,0.4,80); -- "`" = hand
			showingHand:=true;
		else
			showingHand:=false;
		end if;


end showWhatIsHeld;





-- purple portal thru maze wall:
procedure insertgate( k: integer ) is
begin

	kgate:=k;
	rgate:=korow(k);
	cgate:=kocol(k);
	sgate:=koscene(k);

	xgate:=0.5*(koxhi(k)+koxlo(k));
	zgate:=0.5*(kozhi(k)+kozlo(k));
	ygate:=0.5*(koyhi(k)+koylo(k));

end insertgate;





procedure updateBull( --28may18
	mytime: gldouble;
	xme,zme: float;
	xm,ym,zm,am: in out float;
	idir: out integer;
	collide: out boolean
	) is

	dt : float := float(mytime-bulltime);

	odx,odz, oxluk,ozluk,
	ndx,ndz,nam,dam,
	oam,oxm,oym,ozm,
	xluk,zluk,dx,dz,hdist: float;

	xtrip: constant float := -5.0;
	ztrip: constant float := +3.0;
	dtrip: constant float := 0.5;

	closeEnough: constant float := 0.7;

	arenaMinx: constant float := -5.0;
	arenaMinz: constant float := -5.0;
	arenaMaxx: constant float := +5.0;
	arenaMaxz: constant float := +3.0;

begin

	ym:=-iymax;

	collide:=false;
	if not minorun or minotaurdead then
		idir:=0;
	else
		idir:=1;
	end if;
	bulltime:=mytime;

	oam:=mindifangl(am);
	oxm:=xm;
	oym:=ym;
	ozm:=zm;

	oxluk:=fmath.sin(am);
	ozluk:=fmath.cos(am);
	odx:=dt*0.7*speed*fspd*oxluk;
	odz:=dt*0.7*speed*fspd*ozluk;

	dx:=xme-xm;
	dz:=zme-zm;
	hdist:=fmath.sqrt(dx*dx+dz*dz);
	xluk:=dx/hdist;
	zluk:=dz/hdist;
	nam:=mindifangl(fmath.arctan(xluk,zluk));

	dam := mindifangl(nam-oam);
	oam := nam-dam;

	ndx:=dt*0.7*speed*fspd*xluk;
	ndz:=dt*0.7*speed*fspd*zluk;

	dx:=0.98*odx+0.02*ndx; --gradually [2%] change
	dz:=0.98*odz+0.02*ndz; --direction toward
	am:=0.98*oam+0.02*nam; --avatar

	if idir>0 then
		xm:=xm+dx;
		zm:=zm+dz;


		if xm<arenaMinx then
			xm:=arenaMinx;
		elsif xm>arenaMaxx then
			xm:=arenaMaxx;
		end if;


		if zm<arenaMinz then
			zm:=arenaMinz;
		elsif zm>arenaMaxz then
			zm:=arenaMaxz;
		end if;


	end if;


	--distance between minotaur, avatar:
	hdist:= fmath.sqrt( sqr(xm-xme)+sqr(zm-zme) );

	-- test for stopping conditions
	if
		( hdist < closeEnough )
	then
		idir:=0;
		xm:=oxm;
		zm:=ozm;
		collide:=true;
	end if;


	if minotaurdead then
		idir:=0;
		xm:=oxm;
		zm:=ozm;
	end if;




end updateBull;





procedure drawBull( 
	mytime: gldouble;
	xb,yb,zb,hang : float;
	idir: integer
	) is
	use glext.binding;
	use gl.binding;
begin

	glUseProgram(pidbull14);
	glUniformMatrix4fv(imvp14, 1, GL_FALSE, imvp(1,1)'address);
	glUniform1f(hang14, glfloat(hang));
	glUniform1i(idir14, glint(idir));

	glUniform1f(time14, glfloat(mytime));
	glUniform3f(icen14, 
		glfloat(xb),glfloat(yb),glfloat(zb) );
	glUniform1i(samp14, 0);

	glbindtexture(gl_texture_2d, bull_texid);

	avatarolay.draw(bull, vertbuff,uvbuff,elembuff);

end drawBull;



--------- begin new state I/O procs 30oct19 ----------------
procedure printstate is
begin

	put(float'image(xme));
	put(float'image(yme));
	put(float'image(zme));
	new_line;

end printstate;


procedure writeState( aborting: boolean := false ) is separate;

procedure readState( statefile: in string ) is separate;

procedure setState is separate;

procedure drawBugs(nowtime:float) is separate;
procedure screenBugs(nowtime:float) is separate;

end gameutils;



