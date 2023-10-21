
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

with ada.numerics.generic_elementary_functions;

with ada.finalization;
with unchecked_deallocation;


	with text_io;

package body twictobj is 
-- for twistable textured rectangular exterior without top or bottom

-- maps texture coordinates UV to cover the full extent of a side;
-- thus the whole *.png file is visible


	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use fmath;



procedure initialize( rect: in out twictangle ) is
begin
	rect.vert := new varray;
	rect.txuv := new tarray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out twictangle ) is
begin
	vfree( rect.vert );
	tfree( rect.txuv );
	efree( rect.elem );
	--text_io.put_line("rect Free");
end finalize;



procedure rotate(x,z,ang: float;  x2,z2: out float) is
begin
	x2 :=  fmath.cos(ang)*x + fmath.sin(ang)*z;
	z2 := -fmath.sin(ang)*x + fmath.cos(ang)*z;
end rotate;


-- this initializer is intended for texture images
-- that must not be reversed, EG. when lettering exists.
-- AND it omits top & bottom surfaces
procedure setrect2( rect: twictangle;
xc,yc,zc, xr,yr,zr, angl : float;
xm1,xp1,ym,yp,zm1,zp1 : out float
) is

	k, ejj, j : integer := 0;
	jj : glushort;
	xm,xp,zm,zp, dx,dz: float;
begin

	xm1 := xc-xr;
	xp1 := xc+xr;
	ym := yc-yr;
	yp := yc+yr;
	zm1 := zc-zr;
	zp1 := zc+zr;



	-- north front Z+ ccw exterior
	rotate(xm1-xc,zp1-zc,angl,dx,dz); xm:=xc+dx; zp:=zc+dz;
	rect.vert(k+ 1):=xm;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zp; --LL
	rotate(xp1-xc,zp1-zc,angl,dx,dz); xp:=xc+dx; zp:=zc+dz;
	rect.vert(k+ 4):=xp;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zp;
	rect.vert(k+ 7):=xp;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zp;
	rotate(xm1-xc,zp1-zc,angl,dx,dz); xm:=xc+dx; zp:=zc+dz;
	rect.vert(k+10):=xm;  rect.vert(k+11):=yp;  rect.vert(k+12):=zp;
	k:=k+12;

	-- south back Z- ccw exterior
	rotate(xp1-xc,zm1-zc,angl,dx,dz); xp:=xc+dx; zm:=zc+dz;
	rect.vert(k+ 1):=xp;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zm; --LL
	rotate(xm1-xc,zm1-zc,angl,dx,dz); xm:=xc+dx; zm:=zc+dz;
	rect.vert(k+ 4):=xm;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zm;
	rect.vert(k+ 7):=xm;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zm;
	rotate(xp1-xc,zm1-zc,angl,dx,dz); xp:=xc+dx; zm:=zc+dz;
	rect.vert(k+10):=xp;  rect.vert(k+11):=yp;  rect.vert(k+12):=zm;
	k:=k+12;




	-- east X- ccw exterior
	rotate(xm1-xc,zm1-zc,angl,dx,dz); xm:=xc+dx; zm:=zc+dz;
	rect.vert(k+ 1):=xm;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zm; --LL
	rotate(xm1-xc,zp1-zc,angl,dx,dz); xm:=xc+dx; zp:=zc+dz;
	rect.vert(k+ 4):=xm;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zp;
	rect.vert(k+ 7):=xm;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zp;
	rotate(xm1-xc,zm1-zc,angl,dx,dz); xm:=xc+dx; zm:=zc+dz;
	rect.vert(k+10):=xm;  rect.vert(k+11):=yp;  rect.vert(k+12):=zm;
	k:=k+12;

	-- west X+ ccw exterior
	rotate(xp1-xc,zp1-zc,angl,dx,dz); xp:=xc+dx; zp:=zc+dz;
	rect.vert(k+ 1):=xp;  rect.vert(k+ 2):=ym;  rect.vert(k+ 3):=zp; --LL
	rotate(xp1-xc,zm1-zc,angl,dx,dz); xp:=xc+dx; zm:=zc+dz;
	rect.vert(k+ 4):=xp;  rect.vert(k+ 5):=ym;  rect.vert(k+ 6):=zm;
	rect.vert(k+ 7):=xp;  rect.vert(k+ 8):=yp;  rect.vert(k+ 9):=zm;
	rotate(xp1-xc,zp1-zc,angl,dx,dz); xp:=xc+dx; zp:=zc+dz;
	rect.vert(k+10):=xp;  rect.vert(k+11):=yp;  rect.vert(k+12):=zp;

	-- texture UV coords for cube:
	for i in 0..3 loop
		j := i*8;
		rect.txuv(j+1):=1.0;  rect.txuv(j+2):=0.0;
		rect.txuv(j+3):=0.0;  rect.txuv(j+4):=0.0;
		rect.txuv(j+5):=0.0;  rect.txuv(j+6):=1.0;
		rect.txuv(j+7):=1.0;  rect.txuv(j+8):=1.0;
	end loop;

	-- element indices:
	for i in 0..3 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elem(j+1):=jj+0;
		rect.elem(j+2):=jj+1;
		rect.elem(j+3):=jj+2;
		rect.elem(j+4):=jj+2;
		rect.elem(j+5):=jj+3;
		rect.elem(j+6):=jj+0;
	end loop;


end setrect2;







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

procedure draw( rect: twictangle; vertbuff, uvbuff, elembuff : gluint ) is
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

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rect.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);


	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );


	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;






end twictobj;

