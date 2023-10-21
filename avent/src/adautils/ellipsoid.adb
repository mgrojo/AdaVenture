
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

with text_io;  use text_io;
with ada.numerics.generic_elementary_functions;

with matutils;
use matutils;


package body ellipsoid is



	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );
	use fmath;



procedure initialize( eli: in out eloid ) is
begin
	eli.vert := new varray;
	eli.norm := new varray;
	eli.txuv := new tarray;
	eli.elem := new earray;
end initialize;

procedure vfree is new unchecked_deallocation(varray,vap);
procedure tfree is new unchecked_deallocation(tarray,tap);
procedure efree is new unchecked_deallocation(earray,eap);

procedure finalize( eli: in out eloid ) is
begin
	vfree( eli.vert );
	vfree( eli.norm );
	tfree( eli.txuv );
	efree( eli.elem );
end finalize;








procedure setPos( eli: in out eloid; x,y,z: float) is
	k: integer:=0;
begin
	k:=0;
	for i in 1..nfaces*4 loop
		eli.vert(k+1) := (x-eli.oldx) + eli.vert(k+1);
		eli.vert(k+2) := (y-eli.oldy) + eli.vert(k+2);
		eli.vert(k+3) := (z-eli.oldz) + eli.vert(k+3);
		k:=k+3;
	end loop;
	eli.oldx:=x;
	eli.oldy:=y;
	eli.oldz:=z;

end setPos;




procedure getPos( eli: eloid; x,y,z: out float) is
begin
	x:=eli.oldx;
	y:=eli.oldy;
	z:=eli.oldz;
end getPos;










-- Here we assume Y+ is up...
-- maps face texture (or picture) to a spheroid, with
-- nose => (az,el)=(0,0)
procedure fseteli( eli: eloid; rx,ry,rz: float ) is


	t, k, ejj, tj : integer := 0;
	jj : glushort := 0;


	u,v,

	nx00,ny00,nz00,
	nx01,ny01,nz01,
	nx11,ny11,nz11,
	nx10,ny10,nz10,

	x00,y00,z00,
	x01,y01,z01,
	x10,y10,z10,
	x11,y11,z11,
	u0,u1,v0,v1 : float;

	onepi : constant float := ada.numerics.pi;
	twopi : constant float := ada.numerics.pi * 2.0;
	fsides:   constant float := float(nsides);
	fradials: constant float := float(nradials);

-- theta = U in [0, twopi] = azim
-- phi   = V in [0, onepi] = elev+halfpi
-- elev  = phi-halfpi;
--
-- theta = arctan(z,x)
-- phi = arccos(y/r)
-- r = sqrt( x*x + y*y + z*z )
--
-- x = r sin(phi) cos(theta)
-- z = r sin(phi) sin(theta)
-- y = r cos(phi)
--
-- NOTE:  the normal vector at a point (x0,y0,z0) on the
-- ellipsoid is (x0/sqr(rx), y0/sqr(ry), z0/sqr(rz)) !!!

	-- Y+ is up
	procedure surfac( theta, phi: float;  x,y,z: out float ) is
	begin
		x:=rx*fmath.sin(phi)*fmath.cos(theta);
		y:=rz*fmath.sin(phi)*fmath.sin(theta);
		z:=ry*fmath.cos(phi);
	end surfac;

	function sqr(x:float) return float is
	begin
		return x*x;
	end sqr;

-- perfect except for normals @ poles

	eps: constant float := 0.0001;

	x,y,z,r,theta:  float;

begin


	for slice in 1..nradials loop -- U

		u0:=float(slice-1)/fradials;
		u1:=float(slice-0)/fradials;

		for side in 1..nsides loop -- V
			v0:=float(side-1)/fsides;
			v1:=float(side-0)/fsides;

			surfac(twopi*u0,onepi*v0,x00,y00,z00);
			surfac(twopi*u0,onepi*v1,x01,y01,z01);
			surfac(twopi*u1,onepi*v0,x10,y10,z10);
			surfac(twopi*u1,onepi*v1,x11,y11,z11);

			eli.vert(k+ 1):=x00;  eli.vert(k+ 2):=y00;  eli.vert(k+ 3):=z00; --A
			eli.vert(k+ 4):=x01;  eli.vert(k+ 5):=y01;  eli.vert(k+ 6):=z01; --B
			eli.vert(k+ 7):=x11;  eli.vert(k+ 8):=y11;  eli.vert(k+ 9):=z11; --C
			eli.vert(k+10):=x10;  eli.vert(k+11):=y10;  eli.vert(k+12):=z10; --D



-------- begin normal insert ---------------------------------------
---------------------------------------------------------------------------
-- simple [analytic] normals:

			nx00:=x00/rx/rx;
			ny00:=y00/ry/ry;
			nz00:=z00/rz/rz;
			normalize(nx00,ny00,nz00);

			nx01:=x01/rx/rx;
			ny01:=y01/ry/ry;
			nz01:=z01/rz/rz;
			normalize(nx01,ny01,nz01);

			nx11:=x11/rx/rx;
			ny11:=y11/ry/ry;
			nz11:=z11/rz/rz;
			normalize(nx11,ny11,nz11);

			nx10:=x10/rx/rx;
			ny10:=y10/ry/ry;
			nz10:=z10/rz/rz;
			normalize(nx10,ny10,nz10);

---------------------------------------------------------------------------

			eli.norm(k+ 1):=nx00;  eli.norm(k+ 2):=ny00;  eli.norm(k+ 3):=nz00; --00
			eli.norm(k+ 4):=nx01;  eli.norm(k+ 5):=ny01;  eli.norm(k+ 6):=nz01; --01
			eli.norm(k+ 7):=nx11;  eli.norm(k+ 8):=ny11;  eli.norm(k+ 9):=nz11; --11
			eli.norm(k+10):=nx10;  eli.norm(k+11):=ny10;  eli.norm(k+12):=nz10; --10

-------- end normal insert -------------------------------------------



------------ begin singularity-free texture insert -------------------------------

	-- This technique maps a rectangular [face] texture onto the sphere
	-- with the texture-center @ (0.5.0.5) mapping to Npole
	-- Here, we choose the X-axis as the polar axis

	t:=tj;
	for j in 1..4 loop
		-- in this case, we choose X as polar axis:
		x:=eli.vert(k+1)/rx; y:=eli.vert(k+2)/ry; z:=eli.vert(k+3)/rz;
		r:=0.7*sqrt(y*y+z*z); -- 0 <= r <= 1
		if abs(r)<eps then
			theta:=0.0;
		else
			theta:=arctan(z,y);
		end if;
		r:=r+(1.0-abs(x));    -- 0 <= r <= 2
		u:=0.5+0.25*r*cos(theta);
		v:=0.5+0.25*r*sin(theta);


		eli.txuv(t+1):=u; eli.txuv(t+2):=-v;
		t:=t+2;
		k:=k+3;
	end loop; -- FSETELI

------------ end singularity-free texture insert ---------------------------------


			tj:=tj+8;




			-- element indices:
			eli.elem(ejj+1):=jj+0;
			eli.elem(ejj+2):=jj+1;
			eli.elem(ejj+3):=jj+2;
			eli.elem(ejj+4):=jj+2;
			eli.elem(ejj+5):=jj+3;
			eli.elem(ejj+6):=jj+0;
			ejj:=ejj+6;
			jj:=jj+4;


		end loop; --side

	end loop; --slice



end fseteli;


















-- maps equirectangular texture (2x1) to a spheroid
-- as you would for NASA earth maps;  the entire top edge
-- maps to the Npole(0,0,1),  bottom->Spole(0,0,-1):
procedure mseteli( eli: eloid; rx,ry,rz: float ) is


	t, k, ejj, tj : integer := 0;
	jj : glushort := 0;


	u,v,

	nx00,ny00,nz00,
	nx01,ny01,nz01,
	nx11,ny11,nz11,
	nx10,ny10,nz10,

	x00,y00,z00,
	x01,y01,z01,
	x10,y10,z10,
	x11,y11,z11,
	u0,u1,v0,v1 : float;

	onepi : constant float := ada.numerics.pi;
	twopi : constant float := ada.numerics.pi * 2.0;
	fsides:   constant float := float(nsides);
	fradials: constant float := float(nradials);

-- theta = U in [0, twopi]
-- phi   = V in [0, onepi]
--
-- theta = arctan(y,x)
-- phi = arccos(z/r)
-- r = sqrt( x*x + y*y + z*z )
--
-- x = r sin(phi) cos(theta)
-- y = r sin(phi) sin(theta)
-- z = r cos(phi)
--
-- NOTE:  the normal vector at a point (x0,y0,z0) on the
-- ellipsoid is (x0/sqr(rx), y0/sqr(ry), z0/sqr(rz)) !!!


	procedure surfac( theta, phi: float;  x,y,z: out float ) is
	begin
		x:=rx*fmath.sin(phi)*fmath.cos(theta);
		y:=ry*fmath.sin(phi)*fmath.sin(theta);
		z:=rz*fmath.cos(phi);
	end surfac;

	function sqr(x:float) return float is
	begin
		return x*x;
	end sqr;

-- perfect except for normals @ poles

	eps: constant float := 0.0001;

begin


	for slice in 1..nradials loop -- U

		u0:=float(slice-1)/fradials;
		u1:=float(slice-0)/fradials;

		for side in 1..nsides loop -- V
			v0:=float(side-1)/fsides;
			v1:=float(side-0)/fsides;

			surfac(twopi*u0,onepi*v0,x00,y00,z00);
			surfac(twopi*u0,onepi*v1,x01,y01,z01);
			surfac(twopi*u1,onepi*v0,x10,y10,z10);
			surfac(twopi*u1,onepi*v1,x11,y11,z11);

			eli.vert(k+ 1):=x00;  eli.vert(k+ 2):=y00;  eli.vert(k+ 3):=z00; --A
			eli.vert(k+ 4):=x01;  eli.vert(k+ 5):=y01;  eli.vert(k+ 6):=z01; --B
			eli.vert(k+ 7):=x11;  eli.vert(k+ 8):=y11;  eli.vert(k+ 9):=z11; --C
			eli.vert(k+10):=x10;  eli.vert(k+11):=y10;  eli.vert(k+12):=z10; --D



-------- begin normal insert ---------------------------------------
---------------------------------------------------------------------------
-- simple [analytic] normals:

			nx00:=x00/rx/rx;
			ny00:=y00/ry/ry;
			nz00:=z00/rz/rz;
			normalize(nx00,ny00,nz00);

			nx01:=x01/rx/rx;
			ny01:=y01/ry/ry;
			nz01:=z01/rz/rz;
			normalize(nx01,ny01,nz01);

			nx11:=x11/rx/rx;
			ny11:=y11/ry/ry;
			nz11:=z11/rz/rz;
			normalize(nx11,ny11,nz11);

			nx10:=x10/rx/rx;
			ny10:=y10/ry/ry;
			nz10:=z10/rz/rz;
			normalize(nx10,ny10,nz10);

---------------------------------------------------------------------------

			eli.norm(k+ 1):=nx00;  eli.norm(k+ 2):=ny00;  eli.norm(k+ 3):=nz00; --00
			eli.norm(k+ 4):=nx01;  eli.norm(k+ 5):=ny01;  eli.norm(k+ 6):=nz01; --01
			eli.norm(k+ 7):=nx11;  eli.norm(k+ 8):=ny11;  eli.norm(k+ 9):=nz11; --11
			eli.norm(k+10):=nx10;  eli.norm(k+11):=ny10;  eli.norm(k+12):=nz10; --10

-------- end normal insert -------------------------------------------



------------ begin singularity-free texture insert -----------------------------------------

	-- This technique maps a EquiRectangular texture onto the sphere
	-- (1st v0=Npole= +Z, last v0=Spole= -Z)
	t:=tj;
	for j in 1..4 loop
		if    j=1 then u:=u0; v:=v0; --A
		elsif j=2 then u:=u0; v:=v1; --B
		elsif j=3 then u:=u1; v:=v1; --C
		else           u:=u1; v:=v0; --D
		end if;

		eli.txuv(t+1):=u; eli.txuv(t+2):=v;
		t:=t+2;
		k:=k+3;
	end loop;

------------ end singularity-free texture insert -------------------------------------------


			tj:=tj+8;




			-- element indices:
			eli.elem(ejj+1):=jj+0;
			eli.elem(ejj+2):=jj+1;
			eli.elem(ejj+3):=jj+2;
			eli.elem(ejj+4):=jj+2;
			eli.elem(ejj+5):=jj+3;
			eli.elem(ejj+6):=jj+0;
			ejj:=ejj+6;
			jj:=jj+4;


		end loop; --side

	end loop; --slice



end mseteli;











-- This version assumes Y=Npole
-- maps equirectangular texture (2x1) to a spheroid
-- as you would for NASA earth maps;  the entire top edge
-- maps to the Npole(0,1,0),  bottom->Spole(0,-1,0):
procedure yseteli( eli: eloid; rx,ry,rz: float ) is


	t, k, ejj, tj : integer := 0;
	jj : glushort := 0;


	u,v,

	nx00,ny00,nz00,
	nx01,ny01,nz01,
	nx11,ny11,nz11,
	nx10,ny10,nz10,

	x00,y00,z00,
	x01,y01,z01,
	x10,y10,z10,
	x11,y11,z11,
	u0,u1,v0,v1 : float;

	onepi : constant float := ada.numerics.pi;
	twopi : constant float := ada.numerics.pi * 2.0;
	fsides:   constant float := float(nsides);
	fradials: constant float := float(nradials);

-- theta = U in [0, twopi]
-- phi   = V in [0, onepi]
--
-- theta = arctan(y,x)
-- phi = arccos(z/r)
-- r = sqrt( x*x + y*y + z*z )
--
-- x = r sin(phi) cos(theta)
-- y = r sin(phi) sin(theta)
-- z = r cos(phi)
--
-- NOTE:  the normal vector at a point (x0,y0,z0) on the
-- ellipsoid is (x0/sqr(rx), y0/sqr(ry), z0/sqr(rz)) !!!


	procedure surfac( theta, phi: float;  x,y,z: out float ) is
	begin
		x:=rx*fmath.sin(phi)*fmath.cos(theta);
		z:=rz*fmath.sin(phi)*fmath.sin(theta);
		y:=ry*fmath.cos(phi);
	end surfac;

	function sqr(x:float) return float is
	begin
		return x*x;
	end sqr;

-- perfect except for normals @ poles

	eps: constant float := 0.0001;

begin


	for slice in 1..nradials loop -- U

		u0:=float(slice-1)/fradials;
		u1:=float(slice-0)/fradials;

		for side in 1..nsides loop -- V
			v0:=float(side-1)/fsides;
			v1:=float(side-0)/fsides;

			surfac(twopi*u0,onepi*v0,x00,y00,z00);
			surfac(twopi*u0,onepi*v1,x01,y01,z01);
			surfac(twopi*u1,onepi*v0,x10,y10,z10);
			surfac(twopi*u1,onepi*v1,x11,y11,z11);

			--eli.vert(k+ 1):=x00;  eli.vert(k+ 2):=y00;  eli.vert(k+ 3):=z00; --A
			--eli.vert(k+ 4):=x01;  eli.vert(k+ 5):=y01;  eli.vert(k+ 6):=z01; --B
			--eli.vert(k+ 7):=x11;  eli.vert(k+ 8):=y11;  eli.vert(k+ 9):=z11; --C
			--eli.vert(k+10):=x10;  eli.vert(k+11):=y10;  eli.vert(k+12):=z10; --D

			eli.vert(k+ 1):=x10;  eli.vert(k+ 2):=y10;  eli.vert(k+ 3):=z10; --A
			eli.vert(k+ 4):=x11;  eli.vert(k+ 5):=y11;  eli.vert(k+ 6):=z11; --B
			eli.vert(k+ 7):=x01;  eli.vert(k+ 8):=y01;  eli.vert(k+ 9):=z01; --C
			eli.vert(k+10):=x00;  eli.vert(k+11):=y00;  eli.vert(k+12):=z00; --D



-------- begin normal insert ---------------------------------------
---------------------------------------------------------------------------
-- simple [analytic] normals:

			nx00:=x00/rx/rx;
			ny00:=y00/ry/ry;
			nz00:=z00/rz/rz;
			normalize(nx00,ny00,nz00); --D

			nx01:=x01/rx/rx;
			ny01:=y01/ry/ry;
			nz01:=z01/rz/rz;
			normalize(nx01,ny01,nz01); --C

			nx11:=x11/rx/rx;
			ny11:=y11/ry/ry;
			nz11:=z11/rz/rz;
			normalize(nx11,ny11,nz11); --B

			nx10:=x10/rx/rx;
			ny10:=y10/ry/ry;
			nz10:=z10/rz/rz;
			normalize(nx10,ny10,nz10); --A

---------------------------------------------------------------------------

			--eli.norm(k+ 1):=nx00;  eli.norm(k+ 2):=ny00;  eli.norm(k+ 3):=nz00; --00
			--eli.norm(k+ 4):=nx01;  eli.norm(k+ 5):=ny01;  eli.norm(k+ 6):=nz01; --01
			--eli.norm(k+ 7):=nx11;  eli.norm(k+ 8):=ny11;  eli.norm(k+ 9):=nz11; --11
			--eli.norm(k+10):=nx10;  eli.norm(k+11):=ny10;  eli.norm(k+12):=nz10; --10

			eli.norm(k+ 1):=nx10;  eli.norm(k+ 2):=ny10;  eli.norm(k+ 3):=nz10; --A
			eli.norm(k+ 4):=nx11;  eli.norm(k+ 5):=ny11;  eli.norm(k+ 6):=nz11; --B
			eli.norm(k+ 7):=nx01;  eli.norm(k+ 8):=ny01;  eli.norm(k+ 9):=nz01; --C
			eli.norm(k+10):=nx00;  eli.norm(k+11):=ny00;  eli.norm(k+12):=nz00; --D

-------- end normal insert -------------------------------------------



------------ begin singularity-free texture insert -----------------------------------------

	-- This technique maps a EquiRectangular texture onto the sphere
	-- (1st v0=Npole= +Y, last v1=Spole= -Y)
	t:=tj;
	for j in 1..4 loop
		if    j=1 then u:=u1; v:=v0; --A
		elsif j=2 then u:=u1; v:=v1; --B
		elsif j=3 then u:=u0; v:=v1; --C
		else           u:=u0; v:=v0; --D
		end if;

		eli.txuv(t+1):=-u; eli.txuv(t+2):=v;
		t:=t+2;
		k:=k+3;
	end loop;

------------ end singularity-free texture insert -------------------------------------------


			tj:=tj+8;




			-- element indices:
			eli.elem(ejj+1):=jj+0;
			eli.elem(ejj+2):=jj+1;
			eli.elem(ejj+3):=jj+2;
			eli.elem(ejj+4):=jj+2;
			eli.elem(ejj+5):=jj+3;
			eli.elem(ejj+6):=jj+0;
			ejj:=ejj+6;
			jj:=jj+4;


		end loop; --side

	end loop; --slice



end yseteli;


















-- maps rectangular texture to northern hemisphere
-- where texture-center is mapped to the Npole;
-- Southern hemisphere is reflection of northern.
procedure seteli( eli: eloid; rx,ry,rz: float ) is


	t, k, ejj, tj : integer := 0;
	jj : glushort := 0;


	x,y,z,r,u,v,theta,

	nx00,ny00,nz00,
	nx01,ny01,nz01,
	nx11,ny11,nz11,
	nx10,ny10,nz10,

	x00,y00,z00,
	x01,y01,z01,
	x10,y10,z10,
	x11,y11,z11,
	u0,u1,v0,v1 : float;

	onepi : constant float := ada.numerics.pi;
	twopi : constant float := ada.numerics.pi * 2.0;
	fsides:   constant float := float(nsides);
	fradials: constant float := float(nradials);

-- theta = U in [0, twopi]
-- phi   = V in [0, onepi]
--
-- theta = arctan(y,x)
-- phi = arccos(z/r)
-- r = sqrt( x*x + y*y + z*z )
--
-- x = r sin(phi) cos(theta)
-- y = r sin(phi) sin(theta)
-- z = r cos(phi)
--
-- NOTE:  the normal vector at a point (x0,y0,z0) on the
-- ellipsoid is (x0/sqr(rx), y0/sqr(ry), z0/sqr(rz)) !!!


	procedure surfac( theta, phi: float;  x,y,z: out float ) is
	begin
		x:=rx*fmath.sin(phi)*fmath.cos(theta);
		y:=ry*fmath.sin(phi)*fmath.sin(theta);
		z:=rz*fmath.cos(phi);
	end surfac;

	function sqr(x:float) return float is
	begin
		return x*x;
	end sqr;

-- perfect except for normals @ poles

	eps: constant float := 0.0001;

begin


	for slice in 1..nradials loop -- U

		u0:=float(slice-1)/fradials;
		u1:=float(slice-0)/fradials;

		for side in 1..nsides loop -- V
			v0:=float(side-1)/fsides;
			v1:=float(side-0)/fsides;

			surfac(twopi*u0,onepi*v0,x00,y00,z00);
			surfac(twopi*u0,onepi*v1,x01,y01,z01);
			surfac(twopi*u1,onepi*v0,x10,y10,z10);
			surfac(twopi*u1,onepi*v1,x11,y11,z11);

			eli.vert(k+ 1):=x00;  eli.vert(k+ 2):=y00;  eli.vert(k+ 3):=z00; --A
			eli.vert(k+ 4):=x01;  eli.vert(k+ 5):=y01;  eli.vert(k+ 6):=z01; --B
			eli.vert(k+ 7):=x11;  eli.vert(k+ 8):=y11;  eli.vert(k+ 9):=z11; --C
			eli.vert(k+10):=x10;  eli.vert(k+11):=y10;  eli.vert(k+12):=z10; --D



-------- begin normal insert ---------------------------------------
---------------------------------------------------------------------------
-- simple [analytic] normals:

			nx00:=x00/rx/rx;
			ny00:=y00/ry/ry;
			nz00:=z00/rz/rz;
			normalize(nx00,ny00,nz00);

			nx01:=x01/rx/rx;
			ny01:=y01/ry/ry;
			nz01:=z01/rz/rz;
			normalize(nx01,ny01,nz01);

			nx11:=x11/rx/rx;
			ny11:=y11/ry/ry;
			nz11:=z11/rz/rz;
			normalize(nx11,ny11,nz11);

			nx10:=x10/rx/rx;
			ny10:=y10/ry/ry;
			nz10:=z10/rz/rz;
			normalize(nx10,ny10,nz10);

---------------------------------------------------------------------------

			eli.norm(k+ 1):=nx00;  eli.norm(k+ 2):=ny00;  eli.norm(k+ 3):=nz00; --00
			eli.norm(k+ 4):=nx01;  eli.norm(k+ 5):=ny01;  eli.norm(k+ 6):=nz01; --01
			eli.norm(k+ 7):=nx11;  eli.norm(k+ 8):=ny11;  eli.norm(k+ 9):=nz11; --11
			eli.norm(k+10):=nx10;  eli.norm(k+11):=ny10;  eli.norm(k+12):=nz10; --10

-------- end normal insert -------------------------------------------





------------ begin singularity-free texture insert -----------------------------------------

	-- This technique [from rectfineobj.adb] conformally maps the
	-- poles ( in this case we choose the Y-axis as the polar axis )
	-- to the texture center @ (0.5,0.5), then maps the
	-- circular interior of the texture to the upper & lower halves
	-- of the ellipsiod:
	t:=tj;
	for j in 1..4 loop
		x:=eli.vert(k+1)/rx; y:=eli.vert(k+2)/ry; z:=eli.vert(k+3)/rz;
		r:=0.7*sqrt(x*x+z*z); -- 0 <= r <= 1
		if abs(r)<eps then
			theta:=0.0;
		else
			theta:=arctan(z,x);
		end if;
		r:=r+(1.0-abs(y));    -- 0 <= r <= 2
		u:=0.5+0.25*r*cos(theta);
		v:=0.5+0.25*r*sin(theta);
		--note:  Npole maps to texture @ (u,v)=(0.5,0.5)
		eli.txuv(t+1):=u; eli.txuv(t+2):=v;
		t:=t+2;
		k:=k+3;
	end loop;

------------ end singularity-free texture insert -------------------------------------------


			-- For the usual texture UV coords, do this instead:
			--k:=k+12;
			--eli.txuv(tj+1):=u0;  eli.txuv(tj+2):=v0;
			--eli.txuv(tj+3):=u0;  eli.txuv(tj+4):=v1;
			--eli.txuv(tj+5):=u1;  eli.txuv(tj+6):=v1;
			--eli.txuv(tj+7):=u1;  eli.txuv(tj+8):=v0;

			tj:=tj+8;




			-- element indices:
			eli.elem(ejj+1):=jj+0;
			eli.elem(ejj+2):=jj+1;
			eli.elem(ejj+3):=jj+2;
			eli.elem(ejj+4):=jj+2;
			eli.elem(ejj+5):=jj+3;
			eli.elem(ejj+6):=jj+0;
			ejj:=ejj+6;
			jj:=jj+4;


		end loop; --side

	end loop; --slice



end seteli;



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

procedure draw( eli: eloid; vertbuff, uvbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), eli.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), eli.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), eli.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);

end draw;




procedure ldraw( eli: eloid; vertbuff, uvbuff, normbuff, elembuff : gluint ) is
begin

	-- 0th attribute:  vertices
	glBindBuffer(gl_array_buffer, vertbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), eli.vert(1)'address, gl_static_draw);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0,3,gl_float,gl_false,0, system.null_address);

	-- 1st attribute:  texture UV
	glBindBuffer(gl_array_buffer, uvbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nuv), eli.txuv(1)'address, gl_static_draw);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1,2,gl_float,gl_false,0, system.null_address);

	-- 2nd attribute:  normals
	glBindBuffer(gl_array_buffer, normbuff);
	glBufferData(gl_array_buffer, glsizeiptr(4*nvert), eli.norm(1)'address, gl_static_draw);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2,3,gl_float,gl_false,0, system.null_address);

	-- element indices:
	glBindBuffer(gl_element_array_buffer, elembuff);
	glBufferData(gl_element_array_buffer, glsizeiptr(2*nelm), eli.elem(1)'address, gl_static_draw);

	glEnable(gl_blend);
	glBlendFunc(gl_src_alpha, gl_one_minus_src_alpha);

	glDrawElements( gl_triangles, glint(nvert), gl_unsigned_short, system.null_address );

	glDisableVertexAttribArray(0);
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(2);

end ldraw;





end ellipsoid;

