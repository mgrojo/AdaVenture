
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

with system;
with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;


with interfaces.c;
use type interfaces.c.unsigned_short;


with ada.finalization;
with unchecked_deallocation;

with text_io; use text_io;

with matutils; use matutils;



package body troomobj is 

-- for rectangular textured interiors
-- optionally supports fixed floor-gaps (for pools) 
-- or fixed Z-door-gaps for see-thru doors

-- this variation maps multiple copies of texture to walls



	procedure myassert( 
		condition : boolean;  
		flag: integer:=0;
		msg: string := ""
		) is
	begin
	  if condition=false then
			text_io.put("ASSERTION Failed!  ");
			if flag /= 0 then
				text_io.put( "@ " & integer'image(flag) &" : " );
			end if;
			text_io.put_line(msg);
			text_io.new_line;
			raise program_error;
	  end if;
	end myassert;




procedure initialize( rm: in out room ) is
begin
	rm.vert := new varray;
	rm.norm := new varray;
	rm.txuv := new tarray;
	rm.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rm: in out room ) is
begin
	vfree( rm.vert );
	vfree( rm.norm );
	tfree( rm.txuv );
	efree( rm.elem );
	--text_io.put_line("room Free");
end finalize;






procedure setroomwithZMdoor( 
	rm: in out room;  
	dx,dy,  xc,yc,zc, xr,yr,zr, sx,sy,sz : float ) is

	--sx, sy, sz : constant float := 1.0; --potentially scales texture repetition

	xm,xp,ym,yp,zm,zp, ytp,xlf,xrt : float;

	k, j, m : integer := 0;
	jj : glushort;

	ax,ay,az,bx,by,bz,nx,ny,nz: float;

begin


	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;



	-- front (ZP) ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;












	ytp := ym+dy; -- door top
	myassert( ytp < yp );
	xlf  := xc-dx; -- door left
	xrt  := xc+dx; -- door right

	-- back top (ytp replaces ym) ccw inward normal
	rm.vert(k+ 1):=xrt;  rm.vert(k+ 2):=ytp;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xrt;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xlf;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xlf;  rm.vert(k+11):=ytp;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ytp/sy;  rm.txuv(j+2):=xlf/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xlf/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xrt/sx;
	rm.txuv(j+7):=ytp/sy;  rm.txuv(j+8):=xrt/sx;
	j:=j+8;


	-- back left (xlf replaces xp) ccw inward normal
	rm.vert(k+ 1):=xlf;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xlf;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xlf/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xlf/sx;
	j:=j+8;


	-- back right (xrt replaces xm) ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xrt;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xrt;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xrt/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xrt/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;











	-- bottom Y- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --SE
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=ym;  rm.vert(k+ 6):=zp; --NE
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=ym;  rm.vert(k+ 9):=zp; --NW
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --SW
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=xm/sx;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=xp/sx;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=xp/sx;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=xm/sx;  rm.txuv(j+8):=zp/sz;
	j:=j+8;


	-- right X- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zp/sz;
	j:=j+8;


	-- left X+ ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zp/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zp/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zm/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zm/sz;
	j:=j+8;

	rm.nv:=k; --84
	rm.nt:=j; --56
	rm.ne:=42;

	if rm.nv/=84 or rm.nt/=56 then
		put("Error 254 troomobj.adb"); new_line;
		raise program_error;
	end if;



	-- element indices:
	for i in 0..6 loop
		jj:=glushort(i*4);
		m := i*6;
		rm.elem(m+1):=jj+0;
		rm.elem(m+2):=jj+1;
		rm.elem(m+3):=jj+2;
		rm.elem(m+4):=jj+2;
		rm.elem(m+5):=jj+3;
		rm.elem(m+6):=jj+0;
	end loop;

--text_io.put_line("nv="&integer'image(nv));
--text_io.put_line("nt="&integer'image(nt));
--text_io.put_line("ne="&integer'image(ne));

end setroomwithZMdoor;


procedure setroomwithZPdoor( 
	rm: in out room;  
	dx,dy,  xc,yc,zc, xr,yr,zr, sx,sy,sz : float ) is

	xm,xp,ym,yp,zm,zp, ytp,xlf,xrt : float;
	k, j, m : integer := 0;
	jj : glushort;

	ax,ay,az,bx,by,bz,nx,ny,nz: float;

begin


	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr; --is: 3-3,  was: 0-3
	yp  := yc+yr; --is: 3+3,  was: 0+3
	zm  := zc-zr;
	zp  := zc+zr;

	ytp := ym+dy; -- door top
	myassert( ytp < yp );
	xlf  := xc-dx; -- door left
	xrt  := xc+dx; -- door right



	-- front (ZP) top (replace ym with ytp) ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ytp;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ytp;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ytp/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ytp/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;


	-- front (ZP) left (replace xp with xlf) ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xlf;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xlf;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xlf/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xlf/sx;
	j:=j+8;


	-- front (ZP) right (replace xm with xrt) ccw inward normal
	rm.vert(k+ 1):=xrt;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xrt;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xrt/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xrt/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;




















	-- back Z- ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;


	-- bottom Y- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --SE
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=ym;  rm.vert(k+ 6):=zp; --NE
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=ym;  rm.vert(k+ 9):=zp; --NW
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --SW
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=xm/sx;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=xp/sx;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=xp/sx;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=xm/sx;  rm.txuv(j+8):=zp/sz;
	j:=j+8;


	-- right X- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zp/sz;
	j:=j+8;


	-- left X+ ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zp/sz;
	j:=j+8;


	rm.nv:=k; --84
	rm.nt:=j; --56
	rm.ne:=42;

	if rm.nv/=84 or rm.nt/=56 then
		put("Error 437 troomobj.adb"); new_line;
		raise program_error;
	end if;


	-- element indices:
	for i in 0..6 loop
		jj:=glushort(i*4);
		m := i*6;
		rm.elem(m+1):=jj+0;
		rm.elem(m+2):=jj+1;
		rm.elem(m+3):=jj+2;
		rm.elem(m+4):=jj+2;
		rm.elem(m+5):=jj+3;
		rm.elem(m+6):=jj+0;
	end loop;

--text_io.put_line("nv="&integer'image(nv));
--text_io.put_line("nt="&integer'image(nt));
--text_io.put_line("ne="&integer'image(ne));

end setroomwithZPdoor;



















procedure setrect( rm: in out room;  xc,yc,zc, xr,yr,zr, sx,sy,sz : float ) is

xm,xp,ym,yp,zm,zp : float;

	k, j : integer := 0;
	jj : glushort;

	ax,ay,az,bx,by,bz,nx,ny,nz: float;

begin
	--sanity check on scale factor...
	-- s=10 => reduce texture coords by factor of 10 => fewer images per wall
   --myassert( s>=1.0 );
	--myassert( s<=10.0 );

	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;



	-- front Z+ ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;








	-- back Z- ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=xm/sx;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=xm/sx;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=xp/sx;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=xp/sx;
	j:=j+8;




	-- bottom Y- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --SE
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=ym;  rm.vert(k+ 6):=zp; --NE
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=ym;  rm.vert(k+ 9):=zp; --NW
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --SW
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=xm/sx;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=xp/sx;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=xp/sx;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=xm/sx;  rm.txuv(j+8):=zp/sz;
	j:=j+8;





	-- right X- ccw inward normal
	rm.vert(k+ 1):=xm;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zm; --LR
	rm.vert(k+ 4):=xm;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zm; --UR
	rm.vert(k+ 7):=xm;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zp; --UL
	rm.vert(k+10):=xm;  rm.vert(k+11):=ym;  rm.vert(k+12):=zp; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zm/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zm/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zp/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zp/sz;
	j:=j+8;




	-- left X+ ccw inward normal
	rm.vert(k+ 1):=xp;  rm.vert(k+ 2):=ym;  rm.vert(k+ 3):=zp; --LR
	rm.vert(k+ 4):=xp;  rm.vert(k+ 5):=yp;  rm.vert(k+ 6):=zp; --UR
	rm.vert(k+ 7):=xp;  rm.vert(k+ 8):=yp;  rm.vert(k+ 9):=zm; --UL
	rm.vert(k+10):=xp;  rm.vert(k+11):=ym;  rm.vert(k+12):=zm; --LL
-------- begin insert ---------------------------------------
	ax:=rm.vert(k+1)-rm.vert(k+4);
	ay:=rm.vert(k+2)-rm.vert(k+5);
	az:=rm.vert(k+3)-rm.vert(k+6);
	bx:=rm.vert(k+7)-rm.vert(k+4);
	by:=rm.vert(k+8)-rm.vert(k+5);
	bz:=rm.vert(k+9)-rm.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rm.norm(k+ 1):=nx;  rm.norm(k+ 2):=ny;  rm.norm(k+ 3):=nz;
	rm.norm(k+ 4):=nx;  rm.norm(k+ 5):=ny;  rm.norm(k+ 6):=nz;
	rm.norm(k+ 7):=nx;  rm.norm(k+ 8):=ny;  rm.norm(k+ 9):=nz;
	rm.norm(k+10):=nx;  rm.norm(k+11):=ny;  rm.norm(k+12):=nz;
-------- end insert ---------------------------------------
	k:=k+12;

	rm.txuv(j+1):=ym/sy;  rm.txuv(j+2):=zp/sz;
	rm.txuv(j+3):=yp/sy;  rm.txuv(j+4):=zp/sz;
	rm.txuv(j+5):=yp/sy;  rm.txuv(j+6):=zm/sz;
	rm.txuv(j+7):=ym/sy;  rm.txuv(j+8):=zm/sz;
	j:=j+8;



	rm.nv:=k; --60
	rm.nt:=j; --40
	rm.ne:=30;

	if rm.nv/=60 or rm.nt/=40 then
		put("Error 775 troomobj.adb"); new_line;
		raise program_error;
	end if;




	-- element indices:
	for i in 0..4 loop
		jj:=glushort(i*4);
		j := i*6;
		rm.elem(j+1):=jj+0;
		rm.elem(j+2):=jj+1;
		rm.elem(j+3):=jj+2;
		rm.elem(j+4):=jj+2;
		rm.elem(j+5):=jj+3;
		rm.elem(j+6):=jj+0;
	end loop;



end setrect;



--
-- note:  the shaders for these objects must have two 
-- input "layouts", as well as whatever uniforms are needed:
--
-- layout(location=0) in vec3 vertPosName
-- layout(location=1) in vec3 vertRgbName
--
-- ...where their actual names can be whatever is convenient
--
use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rm: room;  vertbuff, uvbuff, elembuff : gluint ) is
begin


	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*rm.nv), rm.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*rm.nt), rm.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*rm.ne), rm.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(rm.nv), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;



procedure ldraw( rm: room;  vertbuff, uvbuff, normbuff, elembuff : gluint ) is
begin


	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*rm.nv), rm.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*rm.nt), rm.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- 2nd attribute:  normals
	glBindBuffer(gl_array_buffer, normbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*rm.nv), rm.norm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2,3,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*rm.ne), rm.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(rm.nv), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(2);

end ldraw;




end troomobj;

