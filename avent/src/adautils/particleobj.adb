
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


--	particle-waterfall: tiny 2D quads that always face the camera (billboarding)
-- ...designed for either north or south facing textures at fixed Z coord.



package body particleobj is 
-- for textured particles

-- Fixed parameters:

	hfac: constant float := 0.02;  --governs horizontal particle spread
	vfac: constant float := 0.002; --governs downward speed variation

	frac: constant float := 1.0; --0.8; --particle-size:
	-- 0.5 => 50% coverage;  
	-- 1.0 => noGaps; i.e. width of texture = spacing of textures
	-- 2.0 => 100%overlap;




	qxd, qyd, qymin: float; --saved dimensions


procedure initialize( rect: in out partix ) is
begin
	orig := new varray;
	rect.vert := new varray;
	rect.txuv := new tarray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out partix ) is
begin
	vfree( orig );
	vfree( rect.vert );
	tfree( rect.txuv );
	efree( rect.elem );
end finalize;


oldet: float := 0.0;



procedure update( rect: in out partix; 
	yvel, etsec: float ) is

--Before each draw, this is called to move particles downward

	dh: constant float := qyd; --height of waterfall
	ymin: constant float := qymin;
	k: integer := 0;
	dt: float := etsec-oldet;
	dy: constant float := yvel*dt;
	f01: float;
	gen: ada.numerics.float_random.generator;
	xoff, yoff: float;
begin


for j in 1..nptsh*nptsw loop


	f01:=ada.numerics.float_random.random(gen);
	xoff := hfac*qxd*(f01-0.5); -- hfac governs width of random spread

	f01:=ada.numerics.float_random.random(gen);
	yoff := vfac*qyd*(f01-0.5); -- vfac governs variations in downward speeds
	--yoff := 0.0;



--first side:

	rect.vert(k+2) := rect.vert(k+2) - dy + yoff;
	rect.vert(k+4) := rect.vert(k+4) - dy + yoff;
	rect.vert(k+6) := rect.vert(k+6) - dy + yoff;
	rect.vert(k+8) := rect.vert(k+8) - dy + yoff;

	rect.vert(k+1) := rect.vert(k+1) + xoff;
	rect.vert(k+3) := rect.vert(k+3) + xoff;
	rect.vert(k+5) := rect.vert(k+5) + xoff;
	rect.vert(k+7) := rect.vert(k+7) + xoff;

	if 
		( rect.vert(k+2) < ymin ) and
		( rect.vert(k+4) < ymin ) and
		( rect.vert(k+6) < ymin ) and
		( rect.vert(k+8) < ymin )
	then
		rect.vert(k+2) := rect.vert(k+2) + dh;
		rect.vert(k+4) := rect.vert(k+4) + dh;
		rect.vert(k+6) := rect.vert(k+6) + dh;
		rect.vert(k+8) := rect.vert(k+8) + dh;

		--restore original [non-perturbed] X-locations
		rect.vert(k+1) := orig(k+1);
		rect.vert(k+3) := orig(k+3);
		rect.vert(k+5) := orig(k+5);
		rect.vert(k+7) := orig(k+7);
	end if;

	k:=k+8;


-- now other side:

	rect.vert(k+2) := rect.vert(k+2) - dy + yoff;
	rect.vert(k+4) := rect.vert(k+4) - dy + yoff;
	rect.vert(k+6) := rect.vert(k+6) - dy + yoff;
	rect.vert(k+8) := rect.vert(k+8) - dy + yoff;
	rect.vert(k+1) := rect.vert(k+1) + xoff;
	rect.vert(k+3) := rect.vert(k+3) + xoff;
	rect.vert(k+5) := rect.vert(k+5) + xoff;
	rect.vert(k+7) := rect.vert(k+7) + xoff;

	if 
		( rect.vert(k+2) < ymin ) and
		( rect.vert(k+4) < ymin ) and
		( rect.vert(k+6) < ymin ) and
		( rect.vert(k+8) < ymin )
	then
		rect.vert(k+2) := rect.vert(k+2) + dh;
		rect.vert(k+4) := rect.vert(k+4) + dh;
		rect.vert(k+6) := rect.vert(k+6) + dh;
		rect.vert(k+8) := rect.vert(k+8) + dh;

		--restore original [non-perturbed] X-locations
		rect.vert(k+1) := orig(k+1);
		rect.vert(k+3) := orig(k+3);
		rect.vert(k+5) := orig(k+5);
		rect.vert(k+7) := orig(k+7);
	end if;

	k:=k+8;




end loop;
oldet:=etsec;

end update;


function min( a,b: float ) return float is
begin
	if a<b then return a;
	else return b; end if;
end min;


-- Here we assume a simple vertical waterfall -- on north/south wall
-- the values represent its 2-dimensional extent
-- i.e. yc+yr=top, yc-yr=bottom.

procedure setrect( rect: partix;
xc,yc, xr,yr : float
) is

	jj: glushort := 0;
	k, t, e : integer := 0;
	xm,xp, ym,yp: float;

	fnw : constant float := float(nptsw);
	fnh : constant float := float(nptsh);
	xcen,ycen,yoff,xoff: float;

	--spacing between quads:
	ddx: constant float := (2.0*xr/fnw);
	ddy: constant float := (2.0*yr/fnh);

	-- width of each quad:
	dx: constant float := (frac*ddx);
	dy: constant float := (frac*ddy);
	dd: constant float := min(dx,dy);

	xmin: constant float := xc-xr;
	ymin: constant float := yc-yr;
	ymax: constant float := yc+yr;

	f01: float;
	gen: ada.numerics.float_random.generator;

begin

	qxd:=2.0*xr; --width @ top
	qyd:=2.0*yr; --height
	qymin:=yc-yr;

-- initially distribute (nptsw*nptsh) particles uniformly throughout the area...

for i in 1..nptsh loop --h=height => vertical discretization
	ycen:=ymax - float(i-1)*ddy;

for j in 1..nptsw loop --w=width => horizontal discretization
	xcen:=xmin+float(j)*ddx;

	f01:=ada.numerics.float_random.random(gen);

	yoff := 1.0*ddy*(f01-0.5);
	xoff := 0.5*ddx*(f01-0.5);


	xm  := xcen+xoff-dd;
	xp  := xcen+xoff+dd;
	ym  := ycen+yoff-dd;
	yp  := ycen+yoff+dd;


--north face:

	rect.vert(k+1):=xm;  rect.vert(k+2):=ym;
	rect.vert(k+3):=xp;  rect.vert(k+4):=ym;
	rect.vert(k+5):=xp;  rect.vert(k+6):=yp;
	rect.vert(k+7):=xm;  rect.vert(k+8):=yp;

	--saved values
	orig(k+1):=xm;  orig(k+2):=ym;
	orig(k+3):=xp;  orig(k+4):=ym;
	orig(k+5):=xp;  orig(k+6):=yp;
	orig(k+7):=xm;  orig(k+8):=yp;

	k:=k+8;


--south face

	rect.vert(k+1):=xp;  rect.vert(k+2):=ym;
	rect.vert(k+3):=xm;  rect.vert(k+4):=ym;
	rect.vert(k+5):=xm;  rect.vert(k+6):=yp;
	rect.vert(k+7):=xp;  rect.vert(k+8):=yp;

	--saved values
	orig(k+1):=xp;  orig(k+2):=ym;
	orig(k+3):=xm;  orig(k+4):=ym;
	orig(k+5):=xm;  orig(k+6):=yp;
	orig(k+7):=xp;  orig(k+8):=yp;

	k:=k+8;




	-- texture UV coords for north face:
	rect.txuv(t+1):=0.0;  rect.txuv(t+2):=0.0;
	rect.txuv(t+3):=1.0;  rect.txuv(t+4):=0.0;
	rect.txuv(t+5):=1.0;  rect.txuv(t+6):=1.0;
	rect.txuv(t+7):=0.0;  rect.txuv(t+8):=1.0;
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



	-- texture UV coords for south face:
	rect.txuv(t+1):=0.0;  rect.txuv(t+2):=0.0;
	rect.txuv(t+3):=1.0;  rect.txuv(t+4):=0.0;
	rect.txuv(t+5):=1.0;  rect.txuv(t+6):=1.0;
	rect.txuv(t+7):=0.0;  rect.txuv(t+8):=1.0;
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



end loop; --for j
end loop; --for i



end setrect;









--
-- note:  the shaders for these objects must have two 
-- input "layouts", as well as whatever uniforms are needed:
--
-- layout(location=0) in vec2 vertPosName
-- layout(location=1) in vec2 texUVname
--
-- ...where their actual names can be whatever is convenient
--

use gl;
use glext;
use glext.binding;
use gl.binding;

procedure draw( rect: partix; vertbuff, uvbuff, elembuff : gluint ) is
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






end particleobj;

