
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



package cylobj is 
-- for textured vertical cylindrical exterior surface, eg. greek column
-- (ends are not drawn)

-- objects bigger than 1 unit on a side show repeating texture


type cylinder is tagged private;

-- setup for textured exterior of vertical cylinder:
procedure setcyl( cyl: cylinder;
xc,yc,zc, yr,rr : float;
xm,xp, ym,yp, zm,zp : out float
);


-- setup for textured interior of vertical cylinder:
procedure setinterior( cyl: cylinder;
xc,yc,zc, yr,rr : float;
xm,xp, ym,yp, zm,zp : out float
);


procedure draw( cyl: cylinder; vertbuff, uvbuff, elembuff : gluint );

private

nrads : constant integer := 16;
nsegs : constant integer := 1;
----------------------------------------------------
nfaces : constant integer := nrads*nsegs;
nvert : constant integer := nfaces*12; -- 16*3*4=#faces*(#coords*#corners)
nuv : constant integer := nfaces*8;
nelm : constant integer := nfaces*6;

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type cylinder is new ada.finalization.controlled with record
	vert : vap;
	txuv : tap;
	elem : eap;
end record;


procedure initialize(cyl: in out cylinder);
procedure finalize(cyl: in out cylinder);

end cylobj;

