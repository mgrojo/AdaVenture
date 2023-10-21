
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



package twictobj is 
-- for twistable textured rectangular exterior without top or bottom

-- maps texture coordinates UV to cover the full extent of a side;
-- thus the whole *.png file is visible

type twictangle is tagged private;


-- this initializer is intended for texture images that
-- must not be reversed, EG. when lettering exists.
-- AND it omits top & bottom surfaces
procedure setrect2( rect: twictangle;
xc,yc,zc, xr,yr,zr, angl : float;
xm1,xp1, ym,yp, zm1,zp1 : out float
);



procedure draw( rect: twictangle; vertbuff, uvbuff, elembuff : gluint );

private

nvert : constant integer := 48;
nuv : constant integer := 32;
nelm : constant integer := 24;


type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type twictangle is new ada.finalization.controlled with record
	vert : vap;
	txuv : tap;
	elem : eap;
end record;


procedure initialize(rect: in out twictangle);
procedure finalize(rect: in out twictangle);

end twictobj;

