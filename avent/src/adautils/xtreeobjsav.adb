
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

with matutils; use matutils;


	with text_io;

package body xtreeobj is -- for fixed X-shaped object (chalice)

-- maps texture coordinates UV to cover the full extent of two
-- perpendicular flat rectangles...the whole *.png file is visible



procedure initialize( rect: in out treeangle ) is
begin
	rect.vert := new varray;
	rect.norm := new varray;
	rect.txuv := new tarray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out treeangle ) is
begin
	vfree( rect.vert );
	vfree( rect.norm );
	tfree( rect.txuv );
	efree( rect.elem );
	--text_io.put_line("rect Free");
end finalize;





procedure setrect( rect: treeangle ) is

	xc,yc,zc : constant float := 0.0;
	xr,yr,zr : constant float := 1.0;

	k, ejj, j : integer := 0;
	jj : glushort;

	ax,ay,az,bx,by,bz, nx,ny,nz,
	xm,xp,ym,yp,zm,zp : float;

begin

	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;

---------- first we setup the Z-Y faces -------------------------


--Z-

	--left -XC
	--================================================================
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zm;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zm;

	---------------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;

	k:=k+12;
	--================================================================

	--right +XC
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zm;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zm;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;
	--------------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
	----------------------------------------------

	k:=k+12;
	--================================================================


--############################################################################


	-- 14dec14 changed so that "nose" of image is always at the same
	--         end of object whether looking from left or right.
	--         See palm trees no longer "shift", and fish always
	--         swim with their nose leading.
	--
	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( (i=0) or (i=2) or (i=4) ) then
			rect.txuv(j+1):=0.0;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.5;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.5;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.0;  rect.txuv(j+8):=1.0;
		else
			rect.txuv(j+1):=0.5;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.0;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.0;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.5;  rect.txuv(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elem(j+1):=jj+0;
		rect.elem(j+2):=jj+1;
		rect.elem(j+3):=jj+2;
		rect.elem(j+4):=jj+2;
		rect.elem(j+5):=jj+3;
		rect.elem(j+6):=jj+0;
	end loop;


--############################################################################







--Z+

	-- left -XC
	--================================================================
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zp;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zp;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;

	---------------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;

	k:=k+12;
	--================================================================

	--right +XC
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zp;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zp;
	--------------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
	----------------------------------------------

	k:=k+12;
	--================================================================



--############################################################################


	-- 14dec14 changed so that "nose" of image is always at the same
	--         end of object whether looking from left or right.
	--         See palm trees no longer "shift", and fish always
	--         swim with their nose leading.
	--
	-- texture UV coords for cube:
	for i in 2..3 loop
		j := i*8;

		if( (i=0) or (i=2) or (i=4) ) then
			rect.txuv(j+1):=0.5;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=1.0;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=1.0;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.5;  rect.txuv(j+8):=1.0;
		else
			rect.txuv(j+1):=1.0;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.5;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.5;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=1.0;  rect.txuv(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 2..3 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elem(j+1):=jj+0;
		rect.elem(j+2):=jj+1;
		rect.elem(j+3):=jj+2;
		rect.elem(j+4):=jj+2;
		rect.elem(j+5):=jj+3;
		rect.elem(j+6):=jj+0;
	end loop;


--############################################################################

------------ HALFWAY:  now for the perpendicular X-Y faces ----------------------

--############################################################################





-- X-

	-- from +ZC
	rect.vert(k+ 1):=xm;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xm;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;
-------- begin insert ---------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
-------- end insert ---------------------------------------

	k:=k+12;

	-- back -ZC
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xm;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xm;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;
-------- begin insert ---------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
-------- end insert ---------------------------------------

	k:=k+12;



	-- 14dec14 changed so that "nose" of image is always at the same
	--         end of object whether looking from left or right.
	--         See palm trees no longer "shift", and fish always
	--         swim with their nose leading.
	--
	-- texture UV coords for cube:
	for i in 4..5 loop
		j := i*8;

		if( (i=0) or (i=2) or (i=4) ) then
			rect.txuv(j+1):=0.0;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.5;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.5;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.0;  rect.txuv(j+8):=1.0;
		else
			rect.txuv(j+1):=0.5;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.0;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.0;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.5;  rect.txuv(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 4..5 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elem(j+1):=jj+0;
		rect.elem(j+2):=jj+1;
		rect.elem(j+3):=jj+2;
		rect.elem(j+4):=jj+2;
		rect.elem(j+5):=jj+3;
		rect.elem(j+6):=jj+0;
	end loop;














-- X+


	-- front +ZC
	rect.vert(k+ 1):=xc;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xp;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xp;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xc;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;
-------- begin insert ---------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
-------- end insert ---------------------------------------

	k:=k+12;

	-- back -ZC
	rect.vert(k+ 1):=xp;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zc;
	rect.vert(k+ 4):=xc;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zc;
	rect.vert(k+ 7):=xc;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zc;
	rect.vert(k+10):=xp;  rect.vert(k+11):=yp;  rect.vert(k+12):=zc;
-------- begin insert ---------------------------------------
	ax:=rect.vert(k+1)-rect.vert(k+4);
	ay:=rect.vert(k+2)-rect.vert(k+5);
	az:=rect.vert(k+3)-rect.vert(k+6);
	bx:=rect.vert(k+7)-rect.vert(k+4);
	by:=rect.vert(k+8)-rect.vert(k+5);
	bz:=rect.vert(k+9)-rect.vert(k+6);
	cross(bx,by,bz, ax,ay,az, nx,ny,nz);
	normalize(nx,ny,nz);
	----------------------------------------------------------
	rect.norm(k+ 1):=nx;  rect.norm(k+ 2):=ny;  rect.norm(k+ 3):=nz;
	rect.norm(k+ 4):=nx;  rect.norm(k+ 5):=ny;  rect.norm(k+ 6):=nz;
	rect.norm(k+ 7):=nx;  rect.norm(k+ 8):=ny;  rect.norm(k+ 9):=nz;
	rect.norm(k+10):=nx;  rect.norm(k+11):=ny;  rect.norm(k+12):=nz;
-------- end insert ---------------------------------------

	k:=k+12;



	-- 14dec14 changed so that "nose" of image is always at the same
	--         end of object whether looking from left or right.
	--         See palm trees no longer "shift", and fish always
	--         swim with their nose leading.
	--
	-- texture UV coords for cube:
	for i in 6..7 loop
		j := i*8;

		if( (i=6) or (i=2) or (i=4) ) then
			rect.txuv(j+1):=0.5;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=1.0;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=1.0;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=0.5;  rect.txuv(j+8):=1.0;
		else
			rect.txuv(j+1):=1.0;  rect.txuv(j+2):=0.0;
			rect.txuv(j+3):=0.5;  rect.txuv(j+4):=0.0;
			rect.txuv(j+5):=0.5;  rect.txuv(j+6):=1.0;
			rect.txuv(j+7):=1.0;  rect.txuv(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 6..7 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elem(j+1):=jj+0;
		rect.elem(j+2):=jj+1;
		rect.elem(j+3):=jj+2;
		rect.elem(j+4):=jj+2;
		rect.elem(j+5):=jj+3;
		rect.elem(j+6):=jj+0;
	end loop;



end setrect;




use gl;
use glext;
use glext.binding;
use gl.binding;


procedure drawZm( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
	nve0: constant integer := 0;
	nuv0: constant integer := 0;
	nel0: constant integer := 0;
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vert(nve0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuv(nuv0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elem(nel0+1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawZm;


procedure drawZp( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
	nve0: constant integer := nvert/4;
	nuv0: constant integer := nuv/4;
	nel0: constant integer := nelm/4;
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vert(nve0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuv(nuv0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elem(nel0+1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawZp;












procedure drawXm( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
	nve0: constant integer := 2*nvert/4;
	nuv0: constant integer := 2*nuv/4;
	nel0: constant integer := 2*nelm/4;
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vert(nve0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuv(nuv0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elem(nel0+1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawXm;


procedure drawXp( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
	nve0: constant integer := 3*nvert/4;
	nuv0: constant integer := 3*nuv/4;
	nel0: constant integer := 3*nelm/4;
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vert(nve0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuv(nuv0+1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elem(nel0+1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawXp;









procedure draw( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint;
	xe,ze: float
	) is
begin

if abs(xe)>abs(ze) then

	if xe>=0.0 and ze>=0.0 then
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
	elsif xe>=0.0 and ze<=0.0 then
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
	elsif xe<=0.0 and ze>=0.0 then
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
	elsif xe<=0.0 and ze<=0.0 then
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
	end if;

else -- abs(xe)<abs(ze)

	if xe>=0.0 and ze>=0.0 then
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
	elsif xe>=0.0 and ze<=0.0 then
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
	elsif xe<=0.0 and ze>=0.0 then
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
	elsif xe<=0.0 and ze<=0.0 then
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
	end if;


end if;
end draw;










procedure ldraw( rect: treeangle; vertbuff, uvbuff, normbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rect.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), rect.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- 2nd attribute:  normals
	glBindBuffer(gl_array_buffer, normbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rect.norm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2,3,gl_float,gl_false,0, system.null_address);


	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rect.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);


	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );


	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(2);

end ldraw;








end xtreeobj;

