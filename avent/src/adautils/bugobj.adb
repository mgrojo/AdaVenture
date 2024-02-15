
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

with Ada.Numerics.generic_elementary_functions;


	with text_io;


--1st try...without lighting
package body bugobj is -- rectangular surface for normal textures
-- note:  needs to be "lightweight" since many bugs will exist



	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use fmath;


procedure initialize( rs: in out bugsurf ) is
begin
	rs.vert := new varray;
	rs.txuv := new tarray;
	rs.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);


procedure finalize( rs: in out bugsurf ) is
begin
	vfree( rs.vert );
	tfree( rs.txuv );
	efree( rs.elem );
end finalize;






procedure rotate( x,z,ang: float;  x2,z2: out float) is
begin
	x2 :=  fmath.cos(ang)*x + fmath.sin(ang)*z;
	z2 := -fmath.sin(ang)*x + fmath.cos(ang)*z;
end rotate;



procedure setrect( rs: bugsurf;
xc,yc,zc, xr,zr, angl : float
) is

	xm : float := -xr;
	xp : float := +xr;
	zm : float := -zr;
	zp : float := +zr;

	x1,z1, x2,z2,
	x3,z3, x4,z4 : float;

begin

	-- top face:

	rotate( xm,zp,angl, x1,z1);
	rotate( xp,zp,angl, x2,z2);
	rotate( xp,zm,angl, x3,z3);
	rotate( xm,zm,angl, x4,z4);

	-- vertex coords:
	rs.vert( 1):=xc+x1;  rs.vert( 2):=yc;  rs.vert( 3):=zc+z1; --v1
	rs.vert( 4):=xc+x2;  rs.vert( 5):=yc;  rs.vert( 6):=zc+z2; --v2
	rs.vert( 7):=xc+x3;  rs.vert( 8):=yc;  rs.vert( 9):=zc+z3; --v3 
	rs.vert(10):=xc+x4;  rs.vert(11):=yc;  rs.vert(12):=zc+z4; --v4
	
	-- CCW => upward (+y) normal

	--texture coords:
	rs.txuv(1):=0.0; rs.txuv(2):=0.0;
	rs.txuv(3):=1.0; rs.txuv(4):=0.0;
	rs.txuv(5):=1.0; rs.txuv(6):=1.0;
	rs.txuv(7):=0.0; rs.txuv(8):=1.0;

	-- element numbering:
	rs.elem(1):=0;
	rs.elem(2):=1;
	rs.elem(3):=2;
	rs.elem(4):=2;
	rs.elem(5):=3;
	rs.elem(6):=0;



end setrect;










-- note:  the shaders for these objects must have one
-- input "layout", as well as whatever uniforms are needed:
--
-- layout(location=0) in vec3 vertPosName
--
-- ...where their actual name can be whatever is convenient
--

use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rs: bugsurf;  vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rs.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), rs.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rs.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);


	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );


	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;


end bugobj;

