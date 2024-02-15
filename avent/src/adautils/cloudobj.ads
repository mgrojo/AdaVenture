
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
	n : integer;

-- a unit square of n*n points defines the centers
-- of tiny billboarded quads.
-- The transparency diminishes linearly with radius
-- so that only a sphere is visible.

package cloudobj is 

type cloud is tagged private;

procedure setrect( rect: cloud );

procedure draw( rect: cloud; vertbuff, elembuff : gluint );

private

ncoord: constant integer := 2;
nvPerFace: constant integer := 4;
nvert : constant integer :=  nvPerFace*n*n*ncoord; 
nelm  : constant integer :=  nvert*3/4; 

type varray is array(1..nvert) of float;
type vap is access varray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type cloud is new ada.finalization.controlled with record
	vert : vap;
	elem : eap;
end record;


procedure initialize(rect: in out cloud);
procedure finalize(rect: in out cloud);

end cloudobj;

