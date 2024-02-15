
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

with text_io;  use text_io;



package body rectxfineobj is -- for untextured rectangular exterior




procedure initialize( rect: in out rectfine ) is
begin
	rect.vert := new varray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out rectfine ) is
begin
	vfree( rect.vert );
	efree( rect.elem );
end finalize;





procedure setrect( rect: rectfine;
xm,xp,ym,yp,zm,zp : out float
) is

	e,ebase, t, tbase, k : integer := 0;

	jj, jjbase : glushort:=0;

	di0,dj0,di1,dj1,
	xd,yd,zd,
	fn, umax,vmax, umin,vmin : float;

	xc,yc,zc : constant float := 0.0;
	xr,yr,zr : constant float := 1.0;

begin

	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;
	xd := 2.0*xr;
	yd := 2.0*yr;
	zd := 2.0*zr;

	fn:=float(nperedge);


for i in 1..nperedge loop
for j in 1..nperedge loop

	di0:=float(i-1)/fn;
	dj0:=float(j-1)/fn;
	di1:=float(i)/fn;
	dj1:=float(j)/fn;


	umin := di0;
	vmin := dj0;
	umax := di1;
	vmax := dj1;


------------------------------------------------------------------

	-- front
	rect.vert(k+ 1):=xm+xd*di0;  rect.vert(k+ 2):=ym+yd*dj0;  rect.vert(k+ 3):=zp;
	rect.vert(k+ 4):=xm+xd*di1;  rect.vert(k+ 5):=ym+yd*dj0;  rect.vert(k+ 6):=zp;
	rect.vert(k+ 7):=xm+xd*di1;  rect.vert(k+ 8):=ym+yd*dj1;  rect.vert(k+ 9):=zp;
	rect.vert(k+10):=xm+xd*di0;  rect.vert(k+11):=ym+yd*dj1;  rect.vert(k+12):=zp;
	k:=k+12;


	-- back
	rect.vert(k+ 1):=xm+xd*di1;  rect.vert(k+ 2):=ym+yd*dj0;  rect.vert(k+ 3):=zm;
	rect.vert(k+ 4):=xm+xd*di0;  rect.vert(k+ 5):=ym+yd*dj0;  rect.vert(k+ 6):=zm;
	rect.vert(k+ 7):=xm+xd*di0;  rect.vert(k+ 8):=ym+yd*dj1;  rect.vert(k+ 9):=zm;
	rect.vert(k+10):=xm+xd*di1;  rect.vert(k+11):=ym+yd*dj1;  rect.vert(k+12):=zm;
	k:=k+12;

------------------------------------------------------------------


-- u:z, v:x (zfish) different
------------------------------------------------------------------

	-- top
	rect.vert(k+ 1):=xm+xd*di0;  rect.vert(k+ 2):=yp;  rect.vert(k+ 3):=zm+zd*dj1;
	rect.vert(k+ 4):=xm+xd*di1;  rect.vert(k+ 5):=yp;  rect.vert(k+ 6):=zm+zd*dj1;
	rect.vert(k+ 7):=xm+xd*di1;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zm+zd*dj0;
	rect.vert(k+10):=xm+xd*di0;  rect.vert(k+11):=yp;  rect.vert(k+12):=zm+zd*dj0;
	k:=k+12;


	-- bottom
	rect.vert(k+ 1):=xm+xd*di0;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zm+zd*dj0;
	rect.vert(k+ 4):=xm+xd*di1;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zm+zd*dj0;
	rect.vert(k+ 7):=xm+xd*di1;  rect.vert(k+ 8):=ym;  rect.vert(k+ 9):=zm+zd*dj1;
	rect.vert(k+10):=xm+xd*di0;  rect.vert(k+11):=ym;  rect.vert(k+12):=zm+zd*dj1;
	k:=k+12;

------------------------------------------------------------------




-- u:z, v:y (zfish) Ok
------------------------------------------------------------------
	-- left
	rect.vert(k+ 1):=xm;  rect.vert(k+ 2):=ym+yd*di0;  rect.vert(k+ 3):=zm+zd*dj0;
	rect.vert(k+ 4):=xm;  rect.vert(k+ 5):=ym+yd*di0;  rect.vert(k+ 6):=zm+zd*dj1;
	rect.vert(k+ 7):=xm;  rect.vert(k+ 8):=ym+yd*di1;  rect.vert(k+ 9):=zm+zd*dj1;
	rect.vert(k+10):=xm;  rect.vert(k+11):=ym+yd*di1;  rect.vert(k+12):=zm+zd*dj0;
	k:=k+12;

	-- right
	rect.vert(k+ 1):=xp;  rect.vert(k+ 2):=ym+yd*di0;  rect.vert(k+ 3):=zm+zd*dj1;
	rect.vert(k+ 4):=xp;  rect.vert(k+ 5):=ym+yd*di0;  rect.vert(k+ 6):=zm+zd*dj0;
	rect.vert(k+ 7):=xp;  rect.vert(k+ 8):=ym+yd*di1;  rect.vert(k+ 9):=zm+zd*dj0;
	rect.vert(k+10):=xp;  rect.vert(k+11):=ym+yd*di1;  rect.vert(k+12):=zm+zd*dj1;
	k:=k+12;

------------------------------------------------------------------

	tbase:=tbase+48;


	-- element indices:
	for s in 0..5 loop
		jj:=glushort(s*4)+jjbase;
		e := s*6 + ebase;
		rect.elem(e+1):=jj+0;
		rect.elem(e+2):=jj+1;
		rect.elem(e+3):=jj+2;
		rect.elem(e+4):=jj+2;
		rect.elem(e+5):=jj+3;
		rect.elem(e+6):=jj+0;
	end loop;
	ebase:=ebase+36;
	jjbase:=jjbase+24;


end loop;--for j
end loop;--for i


end setrect;



use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rect: rectfine; vertbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rect.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rect.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);

end draw;






end rectxfineobj;

