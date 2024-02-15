
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


--1st try...without lighting
package bugobj is -- rectangular surface for normal textures
-- note:  needs to be "lightweight" since many bugs will exist


type bugsurf is tagged private;


-- on floor:
procedure setrect( rs: bugsurf;
xc,yc,zc, xr,zr, angl : float );


procedure draw( rs: bugsurf;  vertbuff, uvbuff, elembuff : gluint );



private

----------- begin private procs --------------------------

nvert : constant integer := 12;
nuv :   constant integer := 8;
nelm :  constant integer := 6;

--type varray is array(1..nvert) of float;
--type vap is access varray;
--type bugsurf is new ada.finalization.controlled with record
--	vert : vap;
--end record;

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type bugsurf is new ada.finalization.controlled with record
	vert, norm : vap;
	txuv : tap;
	elem : eap;
end record;




procedure initialize(rs: in out bugsurf);
procedure finalize(rs: in out bugsurf);


end bugobj;

