
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

with ada.numerics.float_random;

with ada.finalization;
with unchecked_deallocation;

with ada.numerics.generic_elementary_functions;

with text_io;  use text_io;



package body cloudobj is 
-- for untextured rectangular exteriors, Xformed to spherical shells



	frac: constant float := 1.0; --particle-size:
	-- 0.5 => 50% coverage;  
	-- 1.0 => noGaps; i.e. width of texture = spacing of textures
	-- 2.0 => 100%overlap;



	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );



procedure initialize( rect: in out cloud ) is
begin
	rect.vert := new varray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out cloud ) is
begin
	vfree( rect.vert );
	efree( rect.elem );
end finalize;






procedure myassert( condition : boolean;  flag: integer:=0 ) is
begin
  if condition=false then
  		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put_line( "cloudobj @ " & integer'image(flag) );
		end if;
		new_line;
  		raise program_error;
  end if;
end myassert;






procedure setrect( rect: cloud ) is
--construct a cubical shell where each face
--has n X n vertices,
--then deform into a unit spherical shell
--then replicate to nrad spherical shells

	e, k : integer := 0;
	jj : glushort:=0;

	rr: constant float := 1.0; 
	--radius of cube => [x,y,z] in [-1..+1, -1..+1, -1..+1 ]
	fn: constant float := float(n);
	dt: constant float := 2.0*rr/fn; --spacing of vertices
	dd: constant float := frac*dt/2.0; --radius of quads

	xmin,ymin,zmin : constant float := -1.0;

	xc,yc,zc, xm,xp,ym,yp,zm,zp : float;

begin


for h in 1..n loop --x
	xc := xmin + float(h-1)*dt+dt/2.0;
	xm := xc-dd;
	xp := xc+dd;

for i in 1..n loop --y
	yc := ymin + float(i-1)*dt+dt/2.0;
	ym := yc-dd;
	yp := yc+dd;

------------------------------------------------------------------
	--eye is north (+Z) of quad; eye looking south
	rect.vert(k+1):=xm;  rect.vert(k+2):=ym;
	rect.vert(k+3):=xp;  rect.vert(k+4):=ym;
	rect.vert(k+5):=xp;  rect.vert(k+6):=yp;
	rect.vert(k+7):=xm;  rect.vert(k+8):=yp;
	k:=k+8;

------------------------------------------------------------------
	-- element indices: check this logic!
	rect.elem(e+1):=jj+0;
	rect.elem(e+2):=jj+1;
	rect.elem(e+3):=jj+2;
	rect.elem(e+4):=jj+2;
	rect.elem(e+5):=jj+3;
	rect.elem(e+6):=jj+0;
	e:=e+6;
	jj:=jj+4;

end loop;--for i
end loop;--for h

myassert( k=nvert, 1 );
myassert( e=nelm, 2 );


end setrect;



use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rect: cloud; vertbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rect.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rect.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);

end draw;






end cloudobj;

