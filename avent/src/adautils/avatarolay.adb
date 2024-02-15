
--
-- Copyright (C) 2024  <fastrgv@gmail.com>
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

with system;
with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

with interfaces.c;
use type interfaces.c.unsigned_short;


with ada.finalization;
with unchecked_deallocation;

with text_io;


package body avatarolay is -- normals to show exterior...
-- textured object made up of 7 cubelets, including
-- one HeadOverlay for helmets, crowns, hats, etc.
-- that will map to minecraft character bodies:
-- 1) head, 2) headOverlay, 3) torso,
-- 4,5) legs, 6.7) arms:
-------------------------------------
-- y>0.5 => head
-- y>0.0 => torso
----------------------- y<0 :
-- x>+0.5 => leg
-- x<-0.5 => leg
----------------------- abs(x)<0.5 :
-- x>0 => arm
-- x<0 => arm






procedure initialize( rx: in out avatar ) is
begin
	rx.vert := new varray;
	rx.elem := new earray;
	rx.txuv := new tarray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure efree is new unchecked_deallocation(earray,eap);
procedure tfree is new unchecked_deallocation(tarray,tap);

procedure finalize( rx: in out avatar ) is
begin
	vfree( rx.vert );
	efree( rx.elem );
	tfree( rx.txuv );
end finalize;







procedure setrect( rx: avatar ) is

	xc,yc,zc,xr,yr,zr,
	xm,xp,ym,yp,zm,zp : float;

	j,k,t : integer := 0;
	jj : glushort := 0;

	xre,yre,zre,
	umin, vmin,
	umax, vmax : float;

	-- HeadOverlay size increase:
	feps: constant float := 1.2; -- +20%

begin


-- head

	xc:=0.0;  xr:=1.0; 
	zc:=0.0;  zr:=1.0;
	yc:=0.75; yr:=0.24; --top quarter => head

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;



	-- texture UV coords for cube: FTKBRL
	for i in 0..5 loop -- HEAD

		if  i=0  then --front (face)
			umin:= 8.0/64.0;  umax:= 16.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 16.0/32.0;
		elsif i=1 then --top
			umin:= 8.0/64.0;  umax:= 16.0/64.0;
			vmin:= 0.0/32.0;	vmax:= 8.0/32.0;
		elsif i=2 then --back
			umin:= 24.0/64.0; umax:= 32.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 16.0/32.0;
		elsif i=3 then --bottom
			umin:= 16.0/64.0; umax:= 24.0/64.0;
			vmin:= 0.0/32.0;	vmax:= 8.0/32.0;
		elsif i=5 then --left
			umin:= 16.0/64.0; umax:= 24.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 16.0/32.0;
		elsif i=4 then --right
			umin:= 0.0/64.0;  umax:= 8.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 16.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;



	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;


--------- end head;  begin headOverlay








	xc:=0.0;  xr:=1.0; 
	zc:=0.0;  zr:=1.0;
	yc:=0.75; yr:=0.24; --top quarter => head

	xre:=xr*feps;
	yre:=yr*0.99;
	zre:=zr*feps;

	xm := xc-xre;
	xp := xc+xre;
	ym := yc-yre;
	yp := yc+yre;
	zm := zc-zre;
	zp := zc+zre;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;



	-- texture UV coords for cube: FTKBRL
	-- note that vmin=15.5 rather than 16
	-- to hide unwanted artifacts;  same for
	-- duke-must-have @ 32.5 rather than 32.
	for i in 0..5 loop -- HeadOverlay (crowns,hats,etc)

		if  i=0  then --front (face)
			umin:= 40.0/64.0; umax:= 48.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 15.5/32.0;
		elsif i=1 then --top
			umin:= 40.0/64.0; umax:= 48.0/64.0;
			vmin:= 0.0/32.0;	vmax:= 8.0/32.0;
		elsif i=2 then --back
			umin:= 56.0/64.0; umax:= 64.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 15.5/32.0;
		elsif i=3 then --bottom
			umin:= 48.0/64.0; umax:= 56.0/64.0;
			vmin:= 0.0/32.0;	vmax:= 8.0/32.0;
		elsif i=5 then --left
			umin:= 48.0/64.0; umax:= 56.0/64.0;
			vmin:= 8.0/32.0;	vmax:= 15.5/32.0;
		elsif i=4 then --right
			umin:= 32.5/64.0; umax:= 40.0/64.0; --duke-must-have
			vmin:= 8.0/32.0;	vmax:= 15.5/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;



	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;











--------- end headOverlay;  begin torso



	xc:=0.0;  xr:=1.0; 
	zc:=0.0;  zr:=1.0;
	yc:=0.25; yr:=0.24; --2nd quarter => torso

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;



	-- texture UV coords for cube: FTKBRL
	for i in 0..5 loop

		if  i=0  then --front (torso)
			umin:= 20.0/64.0; umax:= 28.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=1 then --top
			umin:= 20.0/64.0; umax:= 28.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=2 then --back
			umin:= 32.0/64.0; umax:= 40.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=3 then --bottom
			umin:= 28.0/64.0; umax:= 36.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=5 then --left
			umin:= 28.0/64.0; umax:= 32.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=4 then --right
			umin:= 16.0/64.0; umax:= 20.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;



	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;











--------- end torso;  begin left leg



	--lower half, front left quarter:
--xc:= 0.75; xr:=0.24; --left half
	xc:= 0.50; xr:=0.49; --left half
	zc:= 0.50; zr:=0.49; --front half
	yc:=-0.50; yr:=0.49; --lower half

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;




	-- texture UV coords for cube:
	for i in 0..5 loop

		if  i=0  then --front (left leg)
			umin:= 4.0/64.0;  umax:= 8.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=1 then --top
			umin:= 4.0/64.0;  umax:= 8.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=2 then --back
			umin:= 16.0/64.0; umax:= 12.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=3 then --bottom
			umin:= 8.0/64.0;  umax:= 12.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=5 then --left
			umin:= 8.0/64.0;  umax:= 12.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=4 then --right
			umin:= 0.0/64.0;  umax:= 4.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;



	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;




--------- end left leg;  begin right leg

	-- lower half, front right quarter
--xc:=-0.75; xr:=0.24;  --right half
	xc:=-0.50; xr:=0.49;  --right half
	zc:= 0.50; zr:=0.49;  --front half
	yc:=-0.50; yr:=0.49;  --lower half

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..5 loop

		if  i=0  then --front (right leg)
			umin:= 8.0/64.0;  umax:= 4.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=1 then --top
			umin:= 4.0/64.0;  umax:= 8.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=2 then --back
			umin:= 12.0/64.0; umax:= 16.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=3 then --bottom
			umin:= 8.0/64.0;  umax:= 12.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=4 then --right
			umin:=12.0/64.0;  umax:= 8.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=5 then --left
			umin:= 0.0/64.0;  umax:= 4.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;




	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;








--------- end right leg;  begin left arm



	--lower half, rear left quarter:
--xc:= 0.75; xr:=0.24; --left half
	xc:= 0.50; xr:=0.49; --left half
	zc:=-0.50; zr:=0.49; --rear half
	yc:=-0.50; yr:=0.49; --lower half

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;




	-- texture UV coords for cube:
	for i in 0..5 loop

		if  i=0  then --front (left arm) (was 0)
			umin:= 48.0/64.0; umax:= 44.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=1 then --top
			umin:= 48.0/64.0; umax:= 44.0/64.0; --*
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=2 then --back
			umin:= 56.0/64.0; umax:= 52.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=3 then --bottom
			umin:= 48.0/64.0; umax:= 52.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=5 then --left
			umin:= 52.0/64.0; umax:= 48.0/64.0; --*
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=4 then --right (was 4)
			umin:= 40.0/64.0; umax:= 44.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;



	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;




--------- end left arm;  begin right arm

	-- lower half, rear right quarter
--xc:=-0.75; xr:=0.24;  --right half
	xc:=-0.50; xr:=0.49;  --right half
	zc:=-0.50; zr:=0.49;  --rear half
	yc:=-0.50; yr:=0.49;  --lower half

	xm := xc-xr;
	xp := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm := zc-zr;
	zp := zc+zr;


	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm;
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp;
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..5 loop

		if  i=0  then --front (right arm)
			umin:= 44.0/64.0;	umax:= 48.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=1 then --top
			umin:= 44.0/64.0; umax:= 48.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=2 then --back (was 2)
			umin:= 52.0/64.0; umax:= 56.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=3 then --bottom
			umin:= 48.0/64.0; umax:= 52.0/64.0;
			vmin:= 16.0/32.0; vmax:= 20.0/32.0;
		elsif i=5 then --left
			umin:= 48.0/64.0; umax:= 52.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		elsif i=4 then --right (was 4)
			umin:= 40.0/64.0; umax:= 44.0/64.0;
			vmin:= 20.0/32.0; vmax:= 32.0/32.0;
		end if;

		rx.txuv(t+1):=umin;  rx.txuv(t+2):=vmax;
		rx.txuv(t+3):=umax;  rx.txuv(t+4):=vmax;
		rx.txuv(t+5):=umax;  rx.txuv(t+6):=vmin;
		rx.txuv(t+7):=umin;  rx.txuv(t+8):=vmin;
		t:=t+8;

	end loop;




	-- element indices:
	for i in 0..5 loop
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
		jj:=jj+4;
		j:=j+6;
	end loop;


-------------- end ---------------------



	if k/=nvert or j/=nelm or t/=nuv then
		raise constraint_error;
	end if;



end setrect;



-- note:  must allow the shaders to show transparency

use gl;
use glext;
use glext.binding;
use gl.binding;
use type interfaces.c.unsigned_char;

procedure draw( rx: avatar;  vertbuff, uvbuff, elembuff : gluint ) is
	blendWasEnabled : glboolean :=gl.binding.glIsEnabled(gl_blend);
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rx.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), rx.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rx.elem(1)'address, gl_static_draw);

	--allow transparency
	gl.binding.glenable(gl_blend);
	gl.binding.glblendfunc(gl_src_alpha, gl_one_minus_src_alpha);


	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );


	if blendWasEnabled=gl_false then
		gl.binding.gldisable(gl_blend);
	end if;

	glDisableVertexAttribArray(0);

end draw;



end avatarolay;

