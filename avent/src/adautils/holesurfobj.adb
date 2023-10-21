
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

with text_io;  use text_io;



-- tiled 2D rectangular textured floor/ground surface 
-- with rectangular XZ-hole for buildings with basements...
-- to be used within a skybox
package body holesurfobj is 


procedure initialize( ct: in out holesurf ) is
begin
	ct.vert := new varray;
	ct.txuv := new tarray;
	ct.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( ct: in out holesurf ) is
begin
	vfree( ct.vert );
	tfree( ct.txuv );
	efree( ct.elem );
	--text_io.put_line("holesurf Free");
end finalize;







	package fmath is new
			ada.Numerics.generic_elementary_functions( float );
	use fmath;


  onepi : constant float     := 3.14159_26535_89793;
  twopi : constant float     := onepi*2.0;









procedure assert( condition : boolean;  flag: integer:=0 ) is
begin
  if condition=false then
  		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put_line( "@ " & integer'image(flag) );
		end if;
		new_line;
  		raise program_error;
  end if;
end assert;






-- hole @ (hx-rx..hx+rx, hz-rz..hz+rz)
procedure setrect( ct: holesurf;  xc,zc, xr,zr : float ) is

	dx : constant float := 2.0*xr/float(nx);
	dz : constant float := 2.0*zr/float(nz);

	z0,z1, x0,x1 : float;
	xi,zj : float;
	ejj : Interfaces.C.unsigned_short;

begin

	nv:=0;
	tk:=0;
	ej:=0;
	ejj:=0;

	for i in -nx..nx-1 loop
	xi := float(i);
	for j in -nz..nz-1 loop
	zj := float(j);

		x0 := xi*dx;
		x1 := x0+dx;

		z0 := zj*dz;
		z1 := z0+dz;

		-- only do this for areas that are not inside hole:
		if 
			(x0<hx-rx) or (z0<hz-rz) or (x1>hx+rx) or (z1>hz+rz)
		then

			-- SE
			ct.vert(nv+1):=x0;
			ct.vert(nv+2):=yht(x0,z0);
			ct.vert(nv+3):=z0;
			nv:=nv+3;

			-- NE
			ct.vert(nv+1):=x0;
			ct.vert(nv+2):=yht(x0,z1);
			ct.vert(nv+3):=z1;
			nv:=nv+3;

			-- NW
			ct.vert(nv+1):=x1;
			ct.vert(nv+2):=yht(x1,z1);
			ct.vert(nv+3):=z1;
			nv:=nv+3;

			-- SW
			ct.vert(nv+1):=x1;
			ct.vert(nv+2):=yht(x1,z0);
			ct.vert(nv+3):=z0;
			nv:=nv+3;


		-- now for the texture coords:
			ct.txuv(tk+1):=x0;  ct.txuv(tk+2):=z0;
			ct.txuv(tk+3):=x0;  ct.txuv(tk+4):=z1;
			ct.txuv(tk+5):=x1;  ct.txuv(tk+6):=z1;
			ct.txuv(tk+7):=x1;  ct.txuv(tk+8):=z0;
			tk := tk+8;


			ct.elem(ej+1):=ejj+0;
			ct.elem(ej+2):=ejj+1;
			ct.elem(ej+3):=ejj+2;
			ct.elem(ej+4):=ejj+2;
			ct.elem(ej+5):=ejj+3;
			ct.elem(ej+6):=ejj+0;
			ej := ej+6;
			ejj := ejj+4;

		end if; --not in hole

	end loop; --j
	end loop; --i

	assert( nv<=nvert );
	assert( tk<=nuv );
	assert( ej<=nelm );


end setrect;


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
procedure draw( ct: holesurf;  vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nv), ct.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*tk), ct.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*ej), ct.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nv), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;



end holesurfobj;

