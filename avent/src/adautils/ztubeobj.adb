
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


with ada.numerics;
with ada.numerics.generic_elementary_functions;
with ada.finalization;
with unchecked_deallocation;

with text_io;  use text_io;


package body ztubeobj is 
-- this package is currently specialized for a snake object...
-- for textured cylinder with multiple Z-sections (snake)




procedure initialize( rect: in out ztube ) is
begin
	rect.vert := new varray;
	rect.txuv := new tarray;
	rect.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( rect: in out ztube ) is
begin
	vfree( rect.vert );
	tfree( rect.txuv );
	efree( rect.elem );
end finalize;





procedure myassert( condition : boolean;  flag: integer:=0 ) is
begin
  if condition=false then
  		put("ASSERTION Failed!  ");
		if flag /= 0 then
			put_line( "@ " & integer'image(flag) );
		end if;
		new_line;
  		raise program_error;
  end if;
end myassert;



	package zmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use zmath;

	onepi : constant float     := 3.14159_26535_89793;
	twopi : constant float     := onepi*2.0;
	halfpi : constant float    := onepi*0.5;
	fourthpi : constant float  := onepi*0.25;







procedure setrect( rect: ztube;
xr,yr,zr : float ) is

	e,ebase, t, tbase, k : integer := 0;

	jj, jjbase : glushort:=0;

	dj0,dj1,
	theta0, theta1,
	ff,fn, umax,vmax, umin,vmin : float;

	x0,x1,y0,y1, z0, z1 : float;
	tty,ttx, tt0, tt1 : float := 1.0;

begin

	fn:=float(nperedge);
	ff:=float(nFaces);

	umin := 0.0;
	umax := 1.0;


for j in 1..nperedge loop -- (32) "j" subdivides Z-direction only

	dj0:=float(j-1)/fn;
	dj1:=float(j)/fn;
	vmin := dj0;
	vmax := dj1;
	z0 := -zr + 2.0*zr*dj0;
	z1 := -zr + 2.0*zr*dj1;

	-- Close up the tail end of the tube because we need to hide
	-- the inside surface which is not visible.
	-- note:  this method works only when (xc,yc)=(0,0)
	tt0:=1.0;
	tt1:=1.0;
	tty:=1.0;
	ttx:=1.0;
	if j=1 then --dj0=0 => z=zmin (tail)
		tt0:=0.2;
		tt1:=0.4;
	elsif j=2 then
		tt0:=0.4;
		tt1:=0.6;
	elsif j=3 then
		tt0:=0.6;
		tt1:=0.8;
	elsif j=4 then
		tt0:=0.8;
	elsif j=nperedge-1 then --dj1=1 => z=zmax (head)
		tty:=0.7;
	elsif j=nperedge then --dj1=1 => z=zmax (head)
		tty:=0.3;
		ttx:=0.7;
	end if; 


	for i in 1..nFaces loop -- (10)

		theta0 := -halfpi + float(i-1)*twopi/ff;
		theta1 := -halfpi + float(i-0)*twopi/ff;

		x0:=xr*cos(theta0);
		x1:=xr*cos(theta1); 
		y0:=yr*sin(theta0);
		y1:=yr*sin(theta1); 

		umin :=  float(i-1)/ff; -- 0 @ 0 and @ nFaces/2
		umax :=  float(i-0)/ff; -- 1 @ nFaces/2 and @ nFaces


		rect.vert(k+ 1):=x1*tt1*ttx;  rect.vert(k+ 2):=y1*tt1*tty;  rect.vert(k+ 3):=z1;
		rect.vert(k+ 4):=x0*tt1*ttx;  rect.vert(k+ 5):=y0*tt1*tty;  rect.vert(k+ 6):=z1;
		rect.vert(k+ 7):=x0*tt0;      rect.vert(k+ 8):=y0*tt0;      rect.vert(k+ 9):=z0;
		rect.vert(k+10):=x1*tt0;      rect.vert(k+11):=y1*tt0;      rect.vert(k+12):=z0;

		k:=k+12;




		t := tbase;
		rect.txuv(t+1):=vmax;  rect.txuv(t+2):=umax;
		rect.txuv(t+3):=vmax;  rect.txuv(t+4):=umin;
		rect.txuv(t+5):=vmin;  rect.txuv(t+6):=umin;
		rect.txuv(t+7):=vmin;  rect.txuv(t+8):=umax;
		tbase:=tbase+8;


		-- element indices:
		jj:=jjbase;
		e :=ebase;
		rect.elem(e+1):=jj+0;
		rect.elem(e+2):=jj+1;
		rect.elem(e+3):=jj+2;
		rect.elem(e+4):=jj+2;
		rect.elem(e+5):=jj+3;
		rect.elem(e+6):=jj+0;
		jjbase:=jjbase+4;
		ebase :=ebase+6;

	end loop; -- i

end loop;--for j



nu:=tbase;
ne:=ebase;
nv:=k;



myassert( nu <= nuv );
myassert( ne <= nelm );
myassert( nv <= nvert );


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

procedure draw( rect: ztube; vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nv), rect.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nu), rect.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*ne), rect.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;






end ztubeobj;

