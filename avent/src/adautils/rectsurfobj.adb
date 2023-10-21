
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


	with text_io;


package body rectsurfobj is -- rectangular untextured surface for special frag.shaders



procedure initialize( rs: in out rectsurf ) is
begin
	rs.vert := new varray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);

procedure finalize( rs: in out rectsurf ) is
begin
	vfree( rs.vert );
	--text_io.put_line("rectsurf Free");
end finalize;







procedure setrect( rs: rectsurf;
xc,zc, xr,zr : float
) is

	xm : float := xc-xr;
	xp : float := xc+xr;
	zm : float := zc-zr;
	zp : float := zc+zr;

	xmm,xpp, zmm,zpp: float;

	dx : constant float := (xp-xm)/fn;
	dz : constant float := (zp-zm)/fn;
	yy : constant float := 0.0;

	k : integer := 0;
	fi,fj : float;
begin


-- top face is nxn grid:
for i in 0..n-1 loop
fi := float(i);
for j in 0..n-1 loop
fj := float(j);

	xmm:=xm+fi*dx;  xpp:=xmm+dx;
	zmm:=zm+fj*dz;  zpp:=zmm+dz;

	rs.vert(k+ 1):=xmm;  rs.vert(k+ 2):=yy;  rs.vert(k+ 3):=zpp; --v1
	rs.vert(k+ 4):=xpp;  rs.vert(k+ 5):=yy;  rs.vert(k+ 6):=zpp; --v2
	rs.vert(k+ 7):=xpp;  rs.vert(k+ 8):=yy;  rs.vert(k+ 9):=zmm; --v3

	rs.vert(k+10):=xmm;  rs.vert(k+11):=yy;  rs.vert(k+12):=zpp; --v1
	rs.vert(k+13):=xpp;  rs.vert(k+14):=yy;  rs.vert(k+15):=zmm; --v3
	rs.vert(k+16):=xmm;  rs.vert(k+17):=yy;  rs.vert(k+18):=zmm; --v4
	k:=k+18;

end loop; --for j
end loop; --for i


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

procedure draw( rs: rectsurf;  vertbuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rs.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	glDrawArrays( gl_triangles, 0, glint(nvert) );

	glDisableVertexAttribArray(0);

end draw;


end rectsurfobj;

