
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

with text_io; use text_io;



package body cyl2texobj is -- intersection of two cylinders, for ZPMs
	package fmath is new
			ada.Numerics.generic_elementary_functions( float );
	use fmath;


------------ begin private procs --------------------------



procedure myassert( condition : boolean;  flag: integer:=0 ) is
begin
  if condition=false then
  		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put_line( "cyl2texobj @ " & integer'image(flag) );
		end if;
		new_line;
  		raise program_error;
  end if;
end myassert;




------------ end private procs --------------------------


procedure initialize( bar: in out ball ) is
begin
	bar.vert := new varray;
	--bar.norm := new varray;
	bar.txuv := new tarray;
	bar.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( bar: in out ball ) is
begin
	vfree( bar.vert );
	--vfree( bar.norm );
	tfree( bar.txuv );
	efree( bar.elem );
end finalize;


procedure recenter( bar: in out ball;  dxc, dyc, dzc : float ) is
	k: integer;
begin

	for j in 1..nvert/3 loop
		k:=(j-1)*3;
		bar.vert(k+1):=bar.vert(k+1)+dxc;
		bar.vert(k+2):=bar.vert(k+2)+dyc;
		bar.vert(k+3):=bar.vert(k+3)+dzc;
	end loop;
	-- last: (nvert/3-1)*3+3 = (nvert-3)+3 = nvert

end recenter;






procedure setcyl2( bar: in out ball; xc,yc,zc, rr : float ) is

	y0,y1,z0,z1,x0,x1 : float;
	t,k,m,j : integer := 0;
	jj : glushort;
	dy : constant float := rr/float(nparts+1);
	ytop : constant float := float(nparts)*dy;
	xtop : constant float := sqrt(rr*rr - ytop*ytop);
	ztop : constant float := xtop;

--// end caps are:
--// top:  p1=(+xtop,ytop,+ztop), p2=(+xtop,ytop,-ztop),
--//       p3=(-xtop,ytop,-ztop), p4=(-xtop,ytop,+ztop)
--//
--// bot:  p1=(+xtop,-ytop,+ztop), p2(-xtop,-ytop,+ztop),
--//       p3=(-xtop,-ytop,-ztop), p4(+xtop,-ytop,-ztop);

begin

--NOTE:  for convenience, UV coords are initially in [-1,+1]
--       ...so that later we must Xlate to [0,1]

	for p in 0..nparts-1 loop

		y0:= float(p) * dy;
		y1:= y0 + dy;
		z0:= sqrt( rr*rr - y0*y0 );
		z1:= sqrt( rr*rr - y1*y1 );
		x0:= z0;
		x1:= z1;
		m := k;


-------------- first time thru we prep exterior using CCW sense ------------------

		--top front ccw
		bar.vert(k+ 1):=-x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=+z0;
		bar.vert(k+ 4):=+x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=+z0;
		bar.vert(k+ 7):=+x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=+z1;
		bar.vert(k+10):=-x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=+z1;

		bar.txuv(t+ 1):=-x0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=+x0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=+x1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=-x1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot front ccw
		bar.vert(k+ 1):=-x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=+z1;
		bar.vert(k+ 4):=+x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=+z1;
		bar.vert(k+ 7):=+x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=+z0;
		bar.vert(k+10):=-x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=+z0;  

		bar.txuv(t+ 1):=-x1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=+x1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=+x0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=-x0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



		--top rear ccw
		bar.vert(k+ 1):=+x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=-z0;
		bar.vert(k+ 4):=-x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=-z0;
		bar.vert(k+ 7):=-x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=-z1;
		bar.vert(k+10):=+x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=-z1;

		bar.txuv(t+ 1):=+x0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=-x0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=-x1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=+x1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot rear ccw
		bar.vert(k+ 1):=+x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=-z1;
		bar.vert(k+ 4):=-x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=-z1;
		bar.vert(k+ 7):=-x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=-z0;
		bar.vert(k+10):=+x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=-z0;  

		bar.txuv(t+ 1):=+x1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=-x1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=-x0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=+x0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



---------------------------- 4 rectangles of 8 -------------------------

		--top left ccw
		bar.vert(k+ 1):=-x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=-z0;
		bar.vert(k+ 4):=-x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=+z0;
		bar.vert(k+ 7):=-x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=+z1;
		bar.vert(k+10):=-x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=-z1;  

		bar.txuv(t+ 1):=-z0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=+z0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=+z1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=-z1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot left ccw
		bar.vert(k+ 1):=-x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=-z1;
		bar.vert(k+ 4):=-x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=+z1;
		bar.vert(k+ 7):=-x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=+z0;
		bar.vert(k+10):=-x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=-z0;  

		bar.txuv(t+ 1):=-z1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=+z1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=+z0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=-z0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



		--top right ccw
		bar.vert(k+ 1):=+x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=+z0;
		bar.vert(k+ 4):=+x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=-z0;
		bar.vert(k+ 7):=+x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=-z1;
		bar.vert(k+10):=+x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=+z1;  

		bar.txuv(t+ 1):=+z0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=-z0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=-z1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=+z1/rr;  bar.txuv(t+ 8):=+y1/rr;


		k:=k+12;
		t:=t+8;



		--bot right ccw
		bar.vert(k+ 1):=+x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=+z1;
		bar.vert(k+ 4):=+x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=-z1;
		bar.vert(k+ 7):=+x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=-z0;
		bar.vert(k+10):=+x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=+z0;  

		bar.txuv(t+ 1):=+z1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=-z1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=-z0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=+z0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;

----------------- now do again using CW sense (inside surface) ----------------------

--first try...only change sign on Xcoords (vert only)

		--top front cw
		bar.vert(k+ 1):=+x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=+z0;
		bar.vert(k+ 4):=-x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=+z0;
		bar.vert(k+ 7):=-x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=+z1;
		bar.vert(k+10):=+x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=+z1;

		bar.txuv(t+ 1):=-x0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=+x0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=+x1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=-x1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot front cw
		bar.vert(k+ 1):=+x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=+z1;
		bar.vert(k+ 4):=-x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=+z1;
		bar.vert(k+ 7):=-x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=+z0;
		bar.vert(k+10):=+x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=+z0;  

		bar.txuv(t+ 1):=-x1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=+x1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=+x0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=-x0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



		--top rear cw
		bar.vert(k+ 1):=-x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=-z0;
		bar.vert(k+ 4):=+x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=-z0;
		bar.vert(k+ 7):=+x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=-z1;
		bar.vert(k+10):=-x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=-z1;

		bar.txuv(t+ 1):=+x0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=-x0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=-x1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=+x1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot rear cw
		bar.vert(k+ 1):=-x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=-z1;
		bar.vert(k+ 4):=+x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=-z1;
		bar.vert(k+ 7):=+x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=-z0;
		bar.vert(k+10):=-x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=-z0;  

		bar.txuv(t+ 1):=+x1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=-x1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=-x0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=+x0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



---------------------------- 4 rectangles of 8 -------------------------
--chs Z only:

		--top left cw
		bar.vert(k+ 1):=-x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=+z0;
		bar.vert(k+ 4):=-x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=-z0;
		bar.vert(k+ 7):=-x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=-z1;
		bar.vert(k+10):=-x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=+z1;  

		bar.txuv(t+ 1):=-z0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=+z0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=+z1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=-z1/rr;  bar.txuv(t+ 8):=+y1/rr;

		k:=k+12;
		t:=t+8;



		--bot left cw
		bar.vert(k+ 1):=-x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=+z1;
		bar.vert(k+ 4):=-x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=-z1;
		bar.vert(k+ 7):=-x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=-z0;
		bar.vert(k+10):=-x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=+z0;  

		bar.txuv(t+ 1):=-z1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=+z1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=+z0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=-z0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;



		--top right cw
		bar.vert(k+ 1):=+x0;  bar.vert(k+ 2):=+y0;  bar.vert(k+ 3):=-z0;
		bar.vert(k+ 4):=+x0;  bar.vert(k+ 5):=+y0;  bar.vert(k+ 6):=+z0;
		bar.vert(k+ 7):=+x1;  bar.vert(k+ 8):=+y1;  bar.vert(k+ 9):=+z1;
		bar.vert(k+10):=+x1;  bar.vert(k+11):=+y1;  bar.vert(k+12):=-z1;  

		bar.txuv(t+ 1):=+z0/rr;  bar.txuv(t+ 2):=+y0/rr;
		bar.txuv(t+ 3):=-z0/rr;  bar.txuv(t+ 4):=+y0/rr;
		bar.txuv(t+ 5):=-z1/rr;  bar.txuv(t+ 6):=+y1/rr;
		bar.txuv(t+ 7):=+z1/rr;  bar.txuv(t+ 8):=+y1/rr;


		k:=k+12;
		t:=t+8;



		--bot right cw
		bar.vert(k+ 1):=+x1;  bar.vert(k+ 2):=-y1;  bar.vert(k+ 3):=-z1;
		bar.vert(k+ 4):=+x1;  bar.vert(k+ 5):=-y1;  bar.vert(k+ 6):=+z1;
		bar.vert(k+ 7):=+x0;  bar.vert(k+ 8):=-y0;  bar.vert(k+ 9):=+z0;
		bar.vert(k+10):=+x0;  bar.vert(k+11):=-y0;  bar.vert(k+12):=-z0;  

		bar.txuv(t+ 1):=+z1/rr;  bar.txuv(t+ 2):=-y1/rr;
		bar.txuv(t+ 3):=-z1/rr;  bar.txuv(t+ 4):=-y1/rr;
		bar.txuv(t+ 5):=-z0/rr;  bar.txuv(t+ 6):=-y0/rr;
		bar.txuv(t+ 7):=+z0/rr;  bar.txuv(t+ 8):=-y0/rr;

		k:=k+12;
		t:=t+8;




	end loop; --for p

myassert(k=nvert, 1001);



	--Xlate UV from [-1,+1] to [0,+1]:
	for i in 1..t loop
		bar.txuv(i) := 0.5*( 1.0 + bar.txuv(i) );
	end loop;





	-- now offset object vertices from (0,0,0) to new center @ (xc,yc,zc)
	bar.oxc:=xc;
	bar.oyc:=yc;
	bar.ozc:=zc;
	bar.orr:=rr;
	recenter(bar, xc,yc,zc);



	-- finally set all of the element indices:
	for i in 0..nparts*8*2-1 loop --  np*8*2 rectangles
		jj := glushort(i*4);
		j := i*6;
		bar.elem(j+1):=jj+0;  bar.elem(j+2):=jj+1;  bar.elem(j+3):=jj+2;
		bar.elem(j+4):=jj+2;  bar.elem(j+5):=jj+3;  bar.elem(j+6):=jj+0;
	end loop;
	-- 1st: 1
	-- last: (np*8*2)*6 = 96*np = nelm

end setcyl2;




-- note:  the shaders for these objects must have 3
-- input "layouts", as well as whatever uniforms are needed:
--
-- layout(location=0) in vec3 vertPos
-- layout(location=1) in vec3 vertUV
-- layout(location=2) in vec3 vertNorm
--
-- ...where their actual names can be whatever is convenient
--
use gl;
use glext;
use glext.binding;
use gl.binding;
procedure draw( bar: ball; 
	--vertbuff, uvbuff, normbuff, elembuff : gluint ) is
	vertbuff, uvbuff,  elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), bar.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);


	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), bar.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- 2nd attribute:  normals
	--glBindBuffer(gl_array_buffer, normbuff);
	--glBufferData(gl_array_buffer, glsizeiptr(4*nvert), bar.norm(1)'address, gl_static_draw);
	--glEnableVertexAttribArray(2);
	--glVertexAttribPointer(2,3,gl_float,gl_false,0, system.null_address);


	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), bar.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);
	--glDisableVertexAttribArray(2);

end draw;

end cyl2texobj;

