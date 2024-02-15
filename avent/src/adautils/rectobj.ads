
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



package rectobj is -- for textured rectangular exterior with 1 rect per face, eg. partition wall

-- Intended for objects bigger than 1 unit on a side...
-- maps texture coordinates UV to cover a portion of a side;
-- thus multiple copies of *.png file may be visible


type rectangle is tagged private;

procedure setrect( rect: rectangle;
xc,yc,zc, xr,yr,zr : float;
xm,xp, ym,yp, zm,zp : out float
);

procedure draw( rect: rectangle; vertbuff, uvbuff, elembuff : gluint );
procedure ldraw( rect: rectangle; vertbuff, uvbuff, normbuff, elembuff : gluint );

private

nvert : constant integer := 72;
nuv : constant integer := 48;
nelm : constant integer := 36;

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type rectangle is new ada.finalization.controlled with record
	vert, norm : vap;
	txuv : tap;
	elem : eap;
end record;


procedure initialize(rect: in out rectangle);
procedure finalize(rect: in out rectangle);

end rectobj;

