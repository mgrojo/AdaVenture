
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




package body cubemapobj is -- for ocean skybox




procedure initialize( cm: in out cubemap ) is
begin
	cm.vert := new varray;
	cm.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( cm: in out cubemap ) is
begin
	vfree( cm.vert );
	efree( cm.elem );
	--text_io.put_line("cubeMap Free");
end finalize;






procedure setrect( cm: cubemap;  xc,yc,zc,xr,yr,zr : float ) is

	xm : float := xc-xr;
	xp : float := xc+xr;

	ym : float := yc-yr;
	yp : float := yc+yr;

	zm : float := zc-zr;
	zp : float := zc+zr;

	k,j : integer := 0;
	jj : glushort;

begin

	-- +X
	cm.vert(k+ 1):=xp;  cm.vert(k+ 2):=ym;  cm.vert(k+ 3):=zm;
	cm.vert(k+ 4):=xp;  cm.vert(k+ 5):=ym;  cm.vert(k+ 6):=zp;
	cm.vert(k+ 7):=xp;  cm.vert(k+ 8):=yp;  cm.vert(k+ 9):=zp;
	cm.vert(k+10):=xp;  cm.vert(k+11):=yp;  cm.vert(k+12):=zm;
	k:=k+12;

	-- -X
	cm.vert(k+ 1):=xm;  cm.vert(k+ 2):=ym;  cm.vert(k+ 3):=zp;
	cm.vert(k+ 4):=xm;  cm.vert(k+ 5):=ym;  cm.vert(k+ 6):=zm;
	cm.vert(k+ 7):=xm;  cm.vert(k+ 8):=yp;  cm.vert(k+ 9):=zm;
	cm.vert(k+10):=xm;  cm.vert(k+11):=yp;  cm.vert(k+12):=zp;
	k:=k+12;


	-- +Y
	cm.vert(k+ 1):=xm;  cm.vert(k+ 2):=yp;  cm.vert(k+ 3):=zm;
	cm.vert(k+ 4):=xp;  cm.vert(k+ 5):=yp;  cm.vert(k+ 6):=zm;
	cm.vert(k+ 7):=xp;  cm.vert(k+ 8):=yp;  cm.vert(k+ 9):=zp;
	cm.vert(k+10):=xm;  cm.vert(k+11):=yp;  cm.vert(k+12):=zp;
	k:=k+12;

	-- -Y
	cm.vert(k+ 1):=xm;  cm.vert(k+ 2):=ym;  cm.vert(k+ 3):=zp;
	cm.vert(k+ 4):=xp;  cm.vert(k+ 5):=ym;  cm.vert(k+ 6):=zp;
	cm.vert(k+ 7):=xp;  cm.vert(k+ 8):=ym;  cm.vert(k+ 9):=zm;
	cm.vert(k+10):=xm;  cm.vert(k+11):=ym;  cm.vert(k+12):=zm;
	k:=k+12;


	-- +Z
	cm.vert(k+ 1):=xp;  cm.vert(k+ 2):=ym;  cm.vert(k+ 3):=zp;
	cm.vert(k+ 4):=xm;  cm.vert(k+ 5):=ym;  cm.vert(k+ 6):=zp;
	cm.vert(k+ 7):=xm;  cm.vert(k+ 8):=yp;  cm.vert(k+ 9):=zp;
	cm.vert(k+10):=xp;  cm.vert(k+11):=yp;  cm.vert(k+12):=zp;
	k:=k+12;

	-- -Z
	cm.vert(k+ 1):=xm;  cm.vert(k+ 2):=ym;  cm.vert(k+ 3):=zm;
	cm.vert(k+ 4):=xp;  cm.vert(k+ 5):=ym;  cm.vert(k+ 6):=zm;
	cm.vert(k+ 7):=xp;  cm.vert(k+ 8):=yp;  cm.vert(k+ 9):=zm;
	cm.vert(k+10):=xm;  cm.vert(k+11):=yp;  cm.vert(k+12):=zm;

	for i in 0..5 loop

		jj:=glushort(i*4);
		j := i*6;
		cm.elem(j+1):=jj+0;
		cm.elem(j+2):=jj+1;
		cm.elem(j+3):=jj+2;
		cm.elem(j+4):=jj+2;
		cm.elem(j+5):=jj+3;
		cm.elem(j+6):=jj+0;

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
use gl.binding;
use glext;
use glext.binding;


procedure draw( cm: cubemap;  vertbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer,glsizeiptr(4*nvert), cm.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), cm.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);

end draw;



end cubemapobj;

