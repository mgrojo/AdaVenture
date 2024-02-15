
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

with ada.numerics.generic_elementary_functions;


with ada.finalization;
with unchecked_deallocation;


	with text_io;

package body cylobj is -- for textured cylindrical exterior, eg. greek column

-- Intended for objects bigger than 1 unit on a side...
-- maps texture coordinates UV to cover a portion of a side;
-- thus multiple copies of *.png file may be visible


	package cmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use cmath;

	onepi : constant float     := 3.14159_26535_89793;
	twopi : constant float     := onepi*2.0;





procedure initialize( cyl: in out cylinder ) is
begin
	cyl.vert := new varray;
	cyl.txuv := new tarray;
	cyl.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( cyl: in out cylinder ) is
begin
	vfree( cyl.vert );
	tfree( cyl.txuv );
	efree( cyl.elem );
	--text_io.put_line("cyl Free");
end finalize;




-- setup for textured interior of vertical cylinder:
procedure setinterior( cyl: cylinder;
xc,yc,zc, yr,rr : float;
xm,xp,ym,yp,zm,zp : out float
) is

	k, ejj, j : integer := 0;
	jj : glushort;
	ang0,ang1,x0,z0,x1,z1: float;
begin

	xm  := xc-rr;
	xp  := xc+rr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-rr;
	zp  := zc+rr;

	for a in 1..nrads loop

		ang0 := float(a-1)*twopi/float(nrads);
		ang1 := float(a)*twopi/float(nrads);

		x0:=xc+rr*cos(ang0);
		z0:=zc+rr*sin(ang0);

		x1:=xc+rr*cos(ang1);
		z1:=zc+rr*sin(ang1);

		cyl.vert(k+ 1):=x1;  cyl.vert(k+ 2):=ym;  cyl.vert(k+ 3):=z1;
		cyl.vert(k+ 4):=x1;  cyl.vert(k+ 5):=yp;  cyl.vert(k+ 6):=z1;
		cyl.vert(k+ 7):=x0;  cyl.vert(k+ 8):=yp;  cyl.vert(k+ 9):=z0;
		cyl.vert(k+10):=x0;  cyl.vert(k+11):=ym;  cyl.vert(k+12):=z0;
		k:=k+12;

	end loop;



	-- texture UV coords for cube
	-- 6sep16: reworked so texture is conformal @ 0=twopi
	-- ...nfaces must be even now
	for i in 0..nfaces-1 loop --0..15
		j := i*8;
		if i<=nfaces/2-1 then
			ang0:=2.0*float(i)/float(nfaces); --@0=>0
			ang1:=2.0*float(i+1)/float(nfaces); --@7=>1.0
		else
			ang0:=2.0*float(nfaces-i)/float(nfaces); --@8=>1.0
			ang1:=2.0*float(nfaces-i-1)/float(nfaces); --@nfaces-1=>0
		end if;

		cyl.txuv(j+1):=1.0;  cyl.txuv(j+2):=ang1;
		cyl.txuv(j+3):=0.0;  cyl.txuv(j+4):=ang1;
		cyl.txuv(j+5):=0.0;  cyl.txuv(j+6):=ang0;
		cyl.txuv(j+7):=1.0;  cyl.txuv(j+8):=ang0;

	end loop;

	-- element indices:
	for i in 0..nfaces-1 loop
		jj:=glushort(i*4);
		j := i*6;
		cyl.elem(j+1):=jj+0;
		cyl.elem(j+2):=jj+1;
		cyl.elem(j+3):=jj+2;
		cyl.elem(j+4):=jj+2;
		cyl.elem(j+5):=jj+3;
		cyl.elem(j+6):=jj+0;
	end loop;


end setinterior;












-- setup for textured exterior of vertical cylinder:
procedure setcyl( cyl: cylinder;
xc,yc,zc, yr,rr : float;
xm,xp,ym,yp,zm,zp : out float
) is

	k, ejj, j : integer := 0;
	jj : glushort;
	ang0,ang1,x0,z0,x1,z1: float;
begin

	xm  := xc-rr;
	xp  := xc+rr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-rr;
	zp  := zc+rr;

	for a in 1..nrads loop

		ang0 := float(a-1)*twopi/float(nrads);
		ang1 := float(a)*twopi/float(nrads);

		x0:=xc+rr*cos(ang0);
		z0:=zc+rr*sin(ang0);

		x1:=xc+rr*cos(ang1);
		z1:=zc+rr*sin(ang1);

		cyl.vert(k+ 1):=x0;  cyl.vert(k+ 2):=ym;  cyl.vert(k+ 3):=z0;
		cyl.vert(k+ 4):=x0;  cyl.vert(k+ 5):=yp;  cyl.vert(k+ 6):=z0;
		cyl.vert(k+ 7):=x1;  cyl.vert(k+ 8):=yp;  cyl.vert(k+ 9):=z1;
		cyl.vert(k+10):=x1;  cyl.vert(k+11):=ym;  cyl.vert(k+12):=z1;
		k:=k+12;

	end loop;



	-- texture UV coords for cube
	-- 6sep16: reworked so texture is conformal @ 0=twopi
	-- ...nfaces must be even now
	for i in 0..nfaces-1 loop --0..15
		j := i*8;
		if i<=nfaces/2-1 then
			ang0:=2.0*float(i)/float(nfaces); --@0=>0
			ang1:=2.0*float(i+1)/float(nfaces); --@7=>1.0
		else
			ang0:=2.0*float(nfaces-i)/float(nfaces); --@8=>1.0
			ang1:=2.0*float(nfaces-i-1)/float(nfaces); --@nfaces-1=>0
		end if;

		cyl.txuv(j+1):=0.0;  cyl.txuv(j+2):=ang0;
		cyl.txuv(j+3):=1.0;  cyl.txuv(j+4):=ang0;
		cyl.txuv(j+5):=1.0;  cyl.txuv(j+6):=ang1;
		cyl.txuv(j+7):=0.0;  cyl.txuv(j+8):=ang1;

	end loop;

	-- element indices:
	for i in 0..nfaces-1 loop
		jj:=glushort(i*4);
		j := i*6;
		cyl.elem(j+1):=jj+0;
		cyl.elem(j+2):=jj+1;
		cyl.elem(j+3):=jj+2;
		cyl.elem(j+4):=jj+2;
		cyl.elem(j+5):=jj+3;
		cyl.elem(j+6):=jj+0;
	end loop;


end setcyl;



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

procedure draw( cyl: cylinder; vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), cyl.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), cyl.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), cyl.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;






end cylobj;

