
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


package body rectxobj is -- untextured, no keepout, for special frag.shader




procedure initialize( rx: in out rectx ) is
begin
	rx.vert := new varray;
	rx.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rx: in out rectx ) is
begin
	vfree( rx.vert );
	efree( rx.elem );
	--text_io.put_line("rectX Free");
end finalize;







procedure setrect( rx: rectx;
xc,yc,zc, xr,yr,zr : float
) is

	xm : float := xc-xr;
	xp : float := xc+xr;
	ym : float := yc-yr;
	yp : float := yc+yr;
	zm : float := zc-zr;
	zp : float := zc+zr;

	j,k : integer := 0;
	jj : glushort;

begin

	-- front
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp; --LL ccw
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;
	k:=k+12;

	-- top
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=yp;  rx.vert(k+ 3):=zp; --NE ccw
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=yp;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;


	-- back
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm; --LL ccw
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- bottom
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm; --SE ccw
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm; --SW
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=ym;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=ym;  rx.vert(k+12):=zp;
	k:=k+12;


	-- right ea
	rx.vert(k+ 1):=xm;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zm; --LL ccw
	rx.vert(k+ 4):=xm;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zp;
	rx.vert(k+ 7):=xm;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zp;
	rx.vert(k+10):=xm;  rx.vert(k+11):=yp;  rx.vert(k+12):=zm;
	k:=k+12;

	-- left we
	rx.vert(k+ 1):=xp;  rx.vert(k+ 2):=ym;  rx.vert(k+ 3):=zp; --LL ccw
	rx.vert(k+ 4):=xp;  rx.vert(k+ 5):=ym;  rx.vert(k+ 6):=zm;
	rx.vert(k+ 7):=xp;  rx.vert(k+ 8):=yp;  rx.vert(k+ 9):=zm;
	rx.vert(k+10):=xp;  rx.vert(k+11):=yp;  rx.vert(k+12):=zp;



	-- element indices:
	for i in 0..5 loop
		jj:=glushort(i*4);
		j := i*6;
		rx.elem(j+1):=jj+0;
		rx.elem(j+2):=jj+1;
		rx.elem(j+3):=jj+2;
		rx.elem(j+4):=jj+2;
		rx.elem(j+5):=jj+3;
		rx.elem(j+6):=jj+0;
	end loop;


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

procedure draw( rx: rectx;  vertbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rx.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), rx.elem(1)'address, gl_static_draw);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);

end draw;



end rectxobj;

