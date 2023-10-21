
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


with gl;  use gl;
with ada.finalization;
with unchecked_deallocation;


generic
	nx, nz : integer; -- #Xgrid, #Zgrid

	-- this defines the terrain elevation:
	with function yht( x,z : float ) return float;

	hx,hz,rx,rz: float; 
	--optional rectangular hole..set radii to zero if none



-- tiled 2D rectangular textured floor/ground surface 
-- with rectangular XZ-hole for buildings with basements...
-- to be used within a skybox
package holesurfobj is

type holesurf is tagged private;

procedure setrect( ct: holesurf;  xc,zc, xr,zr : float );

procedure draw( ct: holesurf;  vertbuff, uvbuff, elembuff : gluint );


private

nsq : constant integer := 4*nx*nz;
nvert : constant integer := 12*nsq;
nuv : constant integer := 8*nsq;
nelm : constant integer := 6*nsq; -- 30aug15 corrections

nv, tk, ej : integer := 0; -- to hold actual values

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;


type holesurf is new ada.finalization.controlled with record
	vert: vap;
	txuv: tap;
	elem: eap;
end record;



procedure initialize(ct: in out holesurf);
procedure finalize(ct: in out holesurf);


end holesurfobj;
