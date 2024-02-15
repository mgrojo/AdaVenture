
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


	with text_io;




package body itemobj is -- for crab, starfish




procedure initialize( ts: in out texsurf ) is
begin
	ts.vert := new varray;
	ts.txuv := new tarray;
	ts.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( ts: in out texsurf ) is
begin
	vfree( ts.vert );
	tfree( ts.txuv );
	efree( ts.elem );
	--text_io.put_line("texsurf Free");
end finalize;






procedure setrect( ts: texsurf ) is
--;  xc,yc,zc, xr,yr,zr : float ) is

xc,yc,zc : constant float := 0.0;
xr,yr,zr : constant float := 1.0;

xm,xp,ym,yp,zm,zp : float;

	vk,tk, ej : integer := 0;
	dx,dz,du, fi,fj : float;

	ejj : glushort := 0;

	xmm,xpp,zmm,zpp,umm,upp,vmm,vpp : float;

begin
	xm  := xc-xr;
	xp  := xc+xr;
	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;

	dx  := (xp-xm)/fn;
	dz  := (zp-zm)/fn;
	du  := 1.0/fn;



for i in 0..n-1 loop
fi:=float(i);
xmm:=xm+fi*dx;  xpp:=xmm+dx;
vmm:=float(n-1-i)*du;  vpp:=vmm+du;

for j in 0..n-1 loop
fj:=float(j);
zmm:=zm+fj*dz;  zpp:=zmm+dz;
umm:=float(n-1-j)*du;  upp:=umm+du;


	ts.vert(vk+ 1):=xmm;  ts.vert(vk+ 2):=yp;  ts.vert(vk+ 3):=zpp; --NE
	ts.vert(vk+ 4):=xpp;  ts.vert(vk+ 5):=yp;  ts.vert(vk+ 6):=zpp; --NW
	ts.vert(vk+ 7):=xpp;  ts.vert(vk+ 8):=yp;  ts.vert(vk+ 9):=zmm; --SW
	ts.vert(vk+10):=xmm;  ts.vert(vk+11):=yp;  ts.vert(vk+12):=zmm; --SE
	vk:=vk+12;


	ts.txuv(tk+1):=umm;  ts.txuv(tk+2):=vpp;
	ts.txuv(tk+3):=umm;  ts.txuv(tk+4):=vmm;
	ts.txuv(tk+5):=upp;  ts.txuv(tk+6):=vmm;
	ts.txuv(tk+7):=upp;  ts.txuv(tk+8):=vpp;
	tk:=tk+8;

	ts.elem(ej+1):=ejj+0;  ts.elem(ej+2):=ejj+1;  ts.elem(ej+3):=ejj+2;
	ts.elem(ej+4):=ejj+2;  ts.elem(ej+5):=ejj+3;  ts.elem(ej+6):=ejj+0;
	ej:=ej+6;
	ejj:=ejj+4;


end loop; -- for j
end loop; -- for i


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

procedure draw( ts: texsurf;  vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), ts.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), ts.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), ts.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;



end itemobj;

