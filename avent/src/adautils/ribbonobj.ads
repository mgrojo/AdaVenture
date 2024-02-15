
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

with gl;  use gl;
with ada.finalization;
with unchecked_deallocation;



generic
	npts: integer; --#vertical divisions

package ribbonobj is 
-- for ribbons with sliding textures

-- maps texture coordinates UV to cover the full extent of a side;
-- thus the whole *.png file is visible


type ribbon is tagged private;

procedure setrect( rect: ribbon;
xc,yc, xr,yr : float );

procedure update( 
	rect: in out ribbon; 
	vvel, etsec: float );


procedure draw( rect: ribbon; vertbuff, uvbuff, elembuff : gluint );

private

nvert : constant integer :=  8*npts*2; 
nuv   : constant integer :=  8*npts*2; 
nelm  : constant integer :=  6*npts*2; 

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type ribbon is new ada.finalization.controlled with record
	vert : vap;
	txuv : tap;
	elem : eap;
end record;


procedure initialize(rect: in out ribbon);
procedure finalize(rect: in out ribbon);

end ribbonobj;

