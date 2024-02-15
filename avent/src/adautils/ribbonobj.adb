
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

with text_io;





package body ribbonobj is 
-- for ribbons with vertically-sliding textures

-- maps texture coordinates UV to cover the full extent of a side;
-- thus the whole *.png file is visible

	qyd: float; --saved height of waterfall

procedure initialize( rect: in out ribbon ) is
begin
	rect.vert := new varray;
	rect.txuv := new tarray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out ribbon ) is
begin
	vfree( rect.vert );
	tfree( rect.txuv );
	efree( rect.elem );
end finalize;


oldet: float := 0.0;


procedure update( rect: in out ribbon; 
	vvel, etsec: float ) is

--Before each draw, this is called to move 
--ribbon-texture downward...
--(by moving vertical texture coord up
--to a higher location in the texture image)

	t: integer := 0;
	dt: float := etsec-oldet;
	dh: constant float := qyd; --height of waterfall (4.4)
	dv: constant float := vvel/dh*dt;
	a,b: float;
begin

	for i in 1..npts loop

		a:=rect.txuv(t+2);
		b:=rect.txuv(t+6);

		a:=a+dv;
		b:=b+dv;

		if a<0.0 and b<0.0 then -- entire quad may be moved up
			a:=a+1.0; b:=b+1.0; 
		end if;

		--north face
		rect.txuv(t+2):=a;
		rect.txuv(t+4):=a;
		rect.txuv(t+6):=b;
		rect.txuv(t+8):=b;
		t:=t+8;

		--south face
		rect.txuv(t+2):=a;
		rect.txuv(t+4):=a;
		rect.txuv(t+6):=b;
		rect.txuv(t+8):=b;
		t:=t+8;

	end loop;


	oldet:=etsec;

end update;


function min( a,b: float ) return float is
begin
	if a<b then return a;
	else return b; end if;
end min;


-- Here we assume a simple vertical waterfall -- on northern wall
-- the values represent its 2-dimensional extent
-- i.e. yc+yr=top, yc-yr=bottom.

procedure setrect( rect: ribbon;
xc,yc, xr,yr : float
) is

	jj: glushort := 0;
	k, t, e : integer := 0;
	ycen, xm,xp, ym,yp, vm,vp: float;

	fnh: constant float := float(npts);
	ddy: constant float := (2.0*yr/fnh);


	xmin: constant float := xc-xr;
	xmax: constant float := xc+xr;
	ymin: constant float := yc-yr;
	ymax: constant float := yc+yr;

	dxmx: constant float := xr*1.1; --10% wider @ bottom
	dx : float;

begin

	qyd:=2.0*yr;

	xm  := xmin;
	xp  := xmax;

for i in 1..npts loop

	ycen:=ymin + float(i-1)*ddy;
	dx := (ymax-ycen)/(ymax-ymin) * dxmx;

	ym  := ycen;
	yp  := ycen+ddy;

	vm := (ym-ymin)/(ymax-ymin);
	vp := (yp-ymin)/(ymax-ymin);

	-- north face quad
	rect.vert(k+1):=xm-dx;  rect.vert(k+2):=ym;
	rect.vert(k+3):=xp+dx;  rect.vert(k+4):=ym;
	rect.vert(k+5):=xp+dx;  rect.vert(k+6):=yp;
	rect.vert(k+7):=xm-dx;  rect.vert(k+8):=yp;

	k:=k+8;

	-- south face quad
	rect.vert(k+1):=xp+dx;  rect.vert(k+2):=ym;
	rect.vert(k+3):=xm-dx;  rect.vert(k+4):=ym;
	rect.vert(k+5):=xm-dx;  rect.vert(k+6):=yp;
	rect.vert(k+7):=xp+dx;  rect.vert(k+8):=yp;

	k:=k+8;






	-- texture UV coords:

	--north face map
	rect.txuv(t+1):=0.0;  rect.txuv(t+2):=vm;
	rect.txuv(t+3):=1.0;  rect.txuv(t+4):=vm;
	rect.txuv(t+5):=1.0;  rect.txuv(t+6):=vp;
	rect.txuv(t+7):=0.0;  rect.txuv(t+8):=vp;
	t:=t+8;

	--south face map
	rect.txuv(t+1):=0.0;  rect.txuv(t+2):=vm;
	rect.txuv(t+3):=1.0;  rect.txuv(t+4):=vm;
	rect.txuv(t+5):=1.0;  rect.txuv(t+6):=vp;
	rect.txuv(t+7):=0.0;  rect.txuv(t+8):=vp;
	t:=t+8;




	-- element indices:
	rect.elem(e+1):=jj+0;
	rect.elem(e+2):=jj+1;
	rect.elem(e+3):=jj+2;
	rect.elem(e+4):=jj+2;
	rect.elem(e+5):=jj+3;
	rect.elem(e+6):=jj+0;
	e:=e+6;
	jj:=jj+4;


	rect.elem(e+1):=jj+0;
	rect.elem(e+2):=jj+1;
	rect.elem(e+3):=jj+2;
	rect.elem(e+4):=jj+2;
	rect.elem(e+5):=jj+3;
	rect.elem(e+6):=jj+0;
	e:=e+6;
	jj:=jj+4;


end loop; -- i


end setrect;









--
-- note:  the shaders for these objects must have two 
-- input "layouts", as well as whatever uniforms are needed:
--
-- layout(location=0) in vec2 vertPosName
-- layout(location=1) in vec2 texUVname
--
-- ...their actual names can be whatever is convenient
--

use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rect: ribbon; vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertex locations
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), rect.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,2,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV coords
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






end ribbonobj;

