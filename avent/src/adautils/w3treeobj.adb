
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

with ada.numerics.generic_elementary_functions;

with ada.finalization;
with unchecked_deallocation;

with matutils; use matutils;


	with text_io;

package body w3treeobj is 
--
-- Triple-plane implementation for a 3D tree graphic...
--
-- 3 planes intersecting along a vertical Y-axis
-- are spaced @ 120 degree increments, each showing an
-- asymmetric tree texture [with transparent background].
-- Direction alternates according to the following 
-- arrangement (top view):
--
--   R    L
--    \  /
--	    \/
--L -------- R
--     /\
--    /  \
--   R    L
--
-- All 3 planes show the entire texture on both sides,
-- where, of course, opposite sides are reversed.
--
-- The draw procedure input requires the X,Z offsets from
-- the tree to the viewpoint (xeye-xtree, zeye-ztree);
-- This enables each of the 6 wings to be drawn in order 
-- from furthest to nearest.  This regimen makes it
-- simple to draw nice looking trees.  As usual, 
-- one still must draw individual trees in 
-- sorted order from furthest to nearest.
--
-- This tree is defined @ the origin with unit radius.
-- The position, height, width and viewpoint location 
-- are set using appropriate uniform values for the 
-- vertex shader;  likewise for wind-sway parameters.  
-- Thus, one tree object suffices for many instances 
-- of trees or grasses.



procedure initialize( rect: in out treeangle ) is
begin
	rect.vertzm := new varray;
	rect.vertzp := new varray;

	rect.vertxm := new varray;
	rect.vertxp := new varray;

	rect.vertwm := new varray;
	rect.vertwp := new varray;

	rect.txuvzm := new tarray;
	rect.txuvzp := new tarray;

	rect.txuvxm := new tarray;
	rect.txuvxp := new tarray;

	rect.txuvwm := new tarray;
	rect.txuvwp := new tarray;

	rect.elemzm := new earray;
	rect.elemzp := new earray;

	rect.elemxm := new earray;
	rect.elemxp := new earray;

	rect.elemwm := new earray;
	rect.elemwp := new earray;

end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out treeangle ) is
begin

	vfree( rect.vertzm );
	vfree( rect.vertzp );
	vfree( rect.vertxm );
	vfree( rect.vertxp );
	vfree( rect.vertwm );
	vfree( rect.vertwp );

	tfree( rect.txuvzm );
	tfree( rect.txuvzp );
	tfree( rect.txuvxm );
	tfree( rect.txuvxp );
	tfree( rect.txuvwm );
	tfree( rect.txuvwp );

	efree( rect.elemzm );
	efree( rect.elemzp );
	efree( rect.elemxm );
	efree( rect.elemxp );
	efree( rect.elemwm );
	efree( rect.elemwp );

end finalize;




	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use fmath;



procedure setrect( rect: treeangle ) is

	xc,yc,zc : constant float := 0.0;
	xr,yr,zr : constant float := 1.0;

	k, ejj, j : integer := 0;
	jj : glushort;

	--ax,ay,az,bx,by,bz, nx,ny,nz,
	wmx,wpx,wmz,wpz, xm,xp,ym,yp,zm,zp : float;

	eps: constant float := 0.001;

	s3 : constant float := fmath.sqrt(3.0);

begin

	xm  := xc-xr;
	xp  := xc+xr;

	-- now define 
	wmz := -0.5;
	wpz := +0.5;
	wmx := -s3/2.0;
	wpx := +s3/2.0;

	ym  := yc-yr;
	yp  := yc+yr;
	zm  := zc-zr;
	zp  := zc+zr;



---------- first we setup the Z-Y faces -------------------------


--############################################################################

--Z-

	k:=0;
	--right -XC
	--================================================================
	rect.vertzm(k+ 1):=xc-eps;  rect.vertzm(k+ 2):=ym;  rect.vertzm(k+ 3):=zm;
	rect.vertzm(k+ 4):=xc-eps;  rect.vertzm(k+ 5):=ym;  rect.vertzm(k+ 6):=zc;
	rect.vertzm(k+ 7):=xc-eps;  rect.vertzm(k+ 8):=yp;  rect.vertzm(k+ 9):=zc;
	rect.vertzm(k+10):=xc-eps;  rect.vertzm(k+11):=yp;  rect.vertzm(k+12):=zm; --CCW

	k:=k+12;
	--================================================================

	--left +XC
	rect.vertzm(k+ 1):=xc+eps;  rect.vertzm(k+ 2):=ym;  rect.vertzm(k+ 3):=zc;
	rect.vertzm(k+ 4):=xc+eps;  rect.vertzm(k+ 5):=ym;  rect.vertzm(k+ 6):=zm;
	rect.vertzm(k+ 7):=xc+eps;  rect.vertzm(k+ 8):=yp;  rect.vertzm(k+ 9):=zm;
	rect.vertzm(k+10):=xc+eps;  rect.vertzm(k+11):=yp;  rect.vertzm(k+12):=zc; --CCW

	k:=k+12;
	--================================================================


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvzm(j+1):=0.0;  rect.txuvzm(j+2):=0.0;
			rect.txuvzm(j+3):=0.5;  rect.txuvzm(j+4):=0.0;
			rect.txuvzm(j+5):=0.5;  rect.txuvzm(j+6):=1.0;
			rect.txuvzm(j+7):=0.0;  rect.txuvzm(j+8):=1.0;
		else
			rect.txuvzm(j+1):=0.5;  rect.txuvzm(j+2):=0.0;
			rect.txuvzm(j+3):=0.0;  rect.txuvzm(j+4):=0.0;
			rect.txuvzm(j+5):=0.0;  rect.txuvzm(j+6):=1.0;
			rect.txuvzm(j+7):=0.5;  rect.txuvzm(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemzm(j+1):=jj+0;
		rect.elemzm(j+2):=jj+1;
		rect.elemzm(j+3):=jj+2;
		rect.elemzm(j+4):=jj+2;
		rect.elemzm(j+5):=jj+3;
		rect.elemzm(j+6):=jj+0;
	end loop;


--############################################################################

--Z+

	k:=0;
	-- right -XC
	--================================================================
	rect.vertzp(k+ 1):=xc-eps;  rect.vertzp(k+ 2):=ym;  rect.vertzp(k+ 3):=zc;
	rect.vertzp(k+ 4):=xc-eps;  rect.vertzp(k+ 5):=ym;  rect.vertzp(k+ 6):=zp;
	rect.vertzp(k+ 7):=xc-eps;  rect.vertzp(k+ 8):=yp;  rect.vertzp(k+ 9):=zp;
	rect.vertzp(k+10):=xc-eps;  rect.vertzp(k+11):=yp;  rect.vertzp(k+12):=zc; --CCW

	k:=k+12;
	--================================================================

	--left +XC
	rect.vertzp(k+ 1):=xc+eps;  rect.vertzp(k+ 2):=ym;  rect.vertzp(k+ 3):=zp;
	rect.vertzp(k+ 4):=xc+eps;  rect.vertzp(k+ 5):=ym;  rect.vertzp(k+ 6):=zc;
	rect.vertzp(k+ 7):=xc+eps;  rect.vertzp(k+ 8):=yp;  rect.vertzp(k+ 9):=zc;
	rect.vertzp(k+10):=xc+eps;  rect.vertzp(k+11):=yp;  rect.vertzp(k+12):=zp; --CCW

	k:=k+12;
	--================================================================


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvzp(j+1):=0.5;  rect.txuvzp(j+2):=0.0;
			rect.txuvzp(j+3):=1.0;  rect.txuvzp(j+4):=0.0;
			rect.txuvzp(j+5):=1.0;  rect.txuvzp(j+6):=1.0;
			rect.txuvzp(j+7):=0.5;  rect.txuvzp(j+8):=1.0;
		else
			rect.txuvzp(j+1):=1.0;  rect.txuvzp(j+2):=0.0;
			rect.txuvzp(j+3):=0.5;  rect.txuvzp(j+4):=0.0;
			rect.txuvzp(j+5):=0.5;  rect.txuvzp(j+6):=1.0;
			rect.txuvzp(j+7):=1.0;  rect.txuvzp(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemzp(j+1):=jj+0;
		rect.elemzp(j+2):=jj+1;
		rect.elemzp(j+3):=jj+2;
		rect.elemzp(j+4):=jj+2;
		rect.elemzp(j+5):=jj+3;
		rect.elemzp(j+6):=jj+0;
	end loop;


--############################################################################

------------ ThirdWAY:  now for the X-Y faces ----------------------

--############################################################################



-- X-

	k:=0;
	-- front +ZC
	rect.vertxm(k+ 1):=wmx;  rect.vertxm(k+ 2):=ym;  rect.vertxm(k+ 3):=wmz+eps;
	rect.vertxm(k+ 4):=0.0;  rect.vertxm(k+ 5):=ym;  rect.vertxm(k+ 6):=0.0+eps;
	rect.vertxm(k+ 7):=0.0;  rect.vertxm(k+ 8):=yp;  rect.vertxm(k+ 9):=0.0+eps;
	rect.vertxm(k+10):=wmx;  rect.vertxm(k+11):=yp;  rect.vertxm(k+12):=wmz+eps;

	k:=k+12;

	-- back -ZC
	rect.vertxm(k+ 1):=0.0;  rect.vertxm(k+ 2):=ym;  rect.vertxm(k+ 3):=0.0-eps;
	rect.vertxm(k+ 4):=wmx;  rect.vertxm(k+ 5):=ym;  rect.vertxm(k+ 6):=wmz-eps;
	rect.vertxm(k+ 7):=wmx;  rect.vertxm(k+ 8):=yp;  rect.vertxm(k+ 9):=wmz-eps;
	rect.vertxm(k+10):=0.0;  rect.vertxm(k+11):=yp;  rect.vertxm(k+12):=0.0-eps;

	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvxm(j+1):=1.0;  rect.txuvxm(j+2):=0.0;
			rect.txuvxm(j+3):=0.5;  rect.txuvxm(j+4):=0.0;
			rect.txuvxm(j+5):=0.5;  rect.txuvxm(j+6):=1.0;
			rect.txuvxm(j+7):=1.0;  rect.txuvxm(j+8):=1.0;
		else
			rect.txuvxm(j+1):=0.5;  rect.txuvxm(j+2):=0.0;
			rect.txuvxm(j+3):=1.0;  rect.txuvxm(j+4):=0.0;
			rect.txuvxm(j+5):=1.0;  rect.txuvxm(j+6):=1.0;
			rect.txuvxm(j+7):=0.5;  rect.txuvxm(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemxm(j+1):=jj+0;
		rect.elemxm(j+2):=jj+1;
		rect.elemxm(j+3):=jj+2;
		rect.elemxm(j+4):=jj+2;
		rect.elemxm(j+5):=jj+3;
		rect.elemxm(j+6):=jj+0;
	end loop;


--############################################################################

-- X+

	k:=0;
	-- front +ZC
	rect.vertxp(k+ 1):=0.0;  rect.vertxp(k+ 2):=ym;  rect.vertxp(k+ 3):=0.0+eps;
	rect.vertxp(k+ 4):=wpx;  rect.vertxp(k+ 5):=ym;  rect.vertxp(k+ 6):=wpz+eps;
	rect.vertxp(k+ 7):=wpx;  rect.vertxp(k+ 8):=yp;  rect.vertxp(k+ 9):=wpz+eps;
	rect.vertxp(k+10):=0.0;  rect.vertxp(k+11):=yp;  rect.vertxp(k+12):=0.0+eps; --CCW

	k:=k+12;

	-- back -ZC
	rect.vertxp(k+ 1):=wpx;  rect.vertxp(k+ 2):=ym;  rect.vertxp(k+ 3):=wpz-eps;
	rect.vertxp(k+ 4):=0.0;  rect.vertxp(k+ 5):=ym;  rect.vertxp(k+ 6):=0.0-eps;
	rect.vertxp(k+ 7):=0.0;  rect.vertxp(k+ 8):=yp;  rect.vertxp(k+ 9):=0.0-eps;
	rect.vertxp(k+10):=wpx;  rect.vertxp(k+11):=yp;  rect.vertxp(k+12):=wpz-eps; --CCW

	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvxp(j+1):=0.5;  rect.txuvxp(j+2):=0.0;
			rect.txuvxp(j+3):=0.0;  rect.txuvxp(j+4):=0.0;
			rect.txuvxp(j+5):=0.0;  rect.txuvxp(j+6):=1.0;
			rect.txuvxp(j+7):=0.5;  rect.txuvxp(j+8):=1.0;
		else
			rect.txuvxp(j+1):=0.0;  rect.txuvxp(j+2):=0.0;
			rect.txuvxp(j+3):=0.5;  rect.txuvxp(j+4):=0.0;
			rect.txuvxp(j+5):=0.5;  rect.txuvxp(j+6):=1.0;
			rect.txuvxp(j+7):=0.0;  rect.txuvxp(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemxp(j+1):=jj+0;
		rect.elemxp(j+2):=jj+1;
		rect.elemxp(j+3):=jj+2;
		rect.elemxp(j+4):=jj+2;
		rect.elemxp(j+5):=jj+3;
		rect.elemxp(j+6):=jj+0;
	end loop;




















--############################################################################

------------ TwoThirdsWAY:  now for the W-Y faces ----------------------

--############################################################################



-- W-

	k:=0;
	-- front +ZC
	rect.vertwm(k+ 1):=wmx;  rect.vertwm(k+ 2):=ym;  rect.vertwm(k+ 3):=wpz+eps;
	rect.vertwm(k+ 4):=0.0;  rect.vertwm(k+ 5):=ym;  rect.vertwm(k+ 6):=0.0+eps;
	rect.vertwm(k+ 7):=0.0;  rect.vertwm(k+ 8):=yp;  rect.vertwm(k+ 9):=0.0+eps;
	rect.vertwm(k+10):=wmx;  rect.vertwm(k+11):=yp;  rect.vertwm(k+12):=wpz+eps;

	k:=k+12;

	-- back -ZC
	rect.vertwm(k+ 1):=0.0;  rect.vertwm(k+ 2):=ym;  rect.vertwm(k+ 3):=0.0-eps;
	rect.vertwm(k+ 4):=wmx;  rect.vertwm(k+ 5):=ym;  rect.vertwm(k+ 6):=wpz-eps;
	rect.vertwm(k+ 7):=wmx;  rect.vertwm(k+ 8):=yp;  rect.vertwm(k+ 9):=wpz-eps;
	rect.vertwm(k+10):=0.0;  rect.vertwm(k+11):=yp;  rect.vertwm(k+12):=0.0-eps;

	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvwm(j+1):=0.0;  rect.txuvwm(j+2):=0.0;
			rect.txuvwm(j+3):=0.5;  rect.txuvwm(j+4):=0.0;
			rect.txuvwm(j+5):=0.5;  rect.txuvwm(j+6):=1.0;
			rect.txuvwm(j+7):=0.0;  rect.txuvwm(j+8):=1.0;
		else
			rect.txuvwm(j+1):=0.5;  rect.txuvwm(j+2):=0.0;
			rect.txuvwm(j+3):=0.0;  rect.txuvwm(j+4):=0.0;
			rect.txuvwm(j+5):=0.0;  rect.txuvwm(j+6):=1.0;
			rect.txuvwm(j+7):=0.5;  rect.txuvwm(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemwm(j+1):=jj+0;
		rect.elemwm(j+2):=jj+1;
		rect.elemwm(j+3):=jj+2;
		rect.elemwm(j+4):=jj+2;
		rect.elemwm(j+5):=jj+3;
		rect.elemwm(j+6):=jj+0;
	end loop;


--############################################################################

-- W+

	k:=0;
	-- front +ZC
	rect.vertwp(k+ 1):=0.0;  rect.vertwp(k+ 2):=ym;  rect.vertwp(k+ 3):=0.0+eps;
	rect.vertwp(k+ 4):=wpx;  rect.vertwp(k+ 5):=ym;  rect.vertwp(k+ 6):=wmz+eps;
	rect.vertwp(k+ 7):=wpx;  rect.vertwp(k+ 8):=yp;  rect.vertwp(k+ 9):=wmz+eps;
	rect.vertwp(k+10):=0.0;  rect.vertwp(k+11):=yp;  rect.vertwp(k+12):=0.0+eps; --CCW

	k:=k+12;

	-- back -ZC
	rect.vertwp(k+ 1):=wpx;  rect.vertwp(k+ 2):=ym;  rect.vertwp(k+ 3):=wmz-eps;
	rect.vertwp(k+ 4):=0.0;  rect.vertwp(k+ 5):=ym;  rect.vertwp(k+ 6):=0.0-eps;
	rect.vertwp(k+ 7):=0.0;  rect.vertwp(k+ 8):=yp;  rect.vertwp(k+ 9):=0.0-eps;
	rect.vertwp(k+10):=wpx;  rect.vertwp(k+11):=yp;  rect.vertwp(k+12):=wmz-eps; --CCW

	k:=k+12;


	-- texture UV coords for cube:
	for i in 0..1 loop
		j := i*8;

		if( i=0 ) then
			rect.txuvwp(j+1):=0.5;  rect.txuvwp(j+2):=0.0;
			rect.txuvwp(j+3):=1.0;  rect.txuvwp(j+4):=0.0;
			rect.txuvwp(j+5):=1.0;  rect.txuvwp(j+6):=1.0;
			rect.txuvwp(j+7):=0.5;  rect.txuvwp(j+8):=1.0;
		else
			rect.txuvwp(j+1):=1.0;  rect.txuvwp(j+2):=0.0;
			rect.txuvwp(j+3):=0.5;  rect.txuvwp(j+4):=0.0;
			rect.txuvwp(j+5):=0.5;  rect.txuvwp(j+6):=1.0;
			rect.txuvwp(j+7):=1.0;  rect.txuvwp(j+8):=1.0;
		end if;
	end loop;

	-- element indices:
	for i in 0..1 loop
		jj:=glushort(i*4);
		j := i*6;
		rect.elemwp(j+1):=jj+0;
		rect.elemwp(j+2):=jj+1;
		rect.elemwp(j+3):=jj+2;
		rect.elemwp(j+4):=jj+2;
		rect.elemwp(j+5):=jj+3;
		rect.elemwp(j+6):=jj+0;
	end loop;





















end setrect;




use gl;
use glext;
use glext.binding;
use gl.binding;


procedure drawZm( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertzm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvzm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemzm(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawZm;


procedure drawZp( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertzp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvzp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemzp(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawZp;












procedure drawXm( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertxm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvxm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemxm(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawXm;


procedure drawXp( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertxp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvxp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemxp(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawXp;



















procedure drawWm( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertwm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvwm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemwm(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawWm;


procedure drawWp( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint 
	) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nvert), rect.vertwp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(nuv), rect.txuvwp(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(nelm/2), rect.elemwp(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert/4), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end drawWp;











procedure draw( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint;
	xe,ze: float
	) is
begin

	--hangl:=fmath.arctan(xe,ze);

	if xe>=0.0 and ze>=0.0 then --xm,zm,wm,wp,zp,xp
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawWm(rect,vertbuff,uvbuff,elembuff);
		drawWp(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
	elsif xe>=0.0 and ze<0.0 then --wm,zp,xm,xp,zm,wp
		drawWm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawWp(rect,vertbuff,uvbuff,elembuff);
	elsif xe<0.0 and ze<0.0 then --xp,zp,wp,wm,zm,xm
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawWp(rect,vertbuff,uvbuff,elembuff);
		drawWm(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
	elsif xe<0.0 and ze>=0.0 then --wp,zm,xp,xm,zp,wm
		drawWp(rect,vertbuff,uvbuff,elembuff);
		drawZm(rect,vertbuff,uvbuff,elembuff);
		drawXp(rect,vertbuff,uvbuff,elembuff);
		drawXm(rect,vertbuff,uvbuff,elembuff);
		drawZp(rect,vertbuff,uvbuff,elembuff);
		drawWm(rect,vertbuff,uvbuff,elembuff);
	end if;


end draw;





end w3treeobj;

