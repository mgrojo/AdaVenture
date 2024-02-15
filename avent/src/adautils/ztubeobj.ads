
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
	nperedge : integer; -- default about 3

package ztubeobj is 
-- this package is currently specialized for a snake object...
-- for textured cylinder with multiple sections in Z direction (snake)
-- made up using rectangular sections that are rounded out with
-- an appropriate shader.

type ztube is tagged private;

-- here (xr,yr,zr)  are all nonnegligible and approximately equal
procedure setrect( rect: ztube; xr,yr,zr : float );

procedure draw( rect: ztube; vertbuff, uvbuff, elembuff : gluint );

private

-- nperedge=7

nFaces : constant integer := 10; --9; -- # secant approximations to circle
nptsPerFace : constant integer := 4;
cpp : constant integer := 3; -- coords per point

nvert : constant integer := nFaces*nptsPerFace*cpp*nperedge; --504
nuv : constant integer := (nvert*2)/3; --336
nelm : constant integer := nvert/2; --252

nv,nu,ne : integer; --actual values

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type ztube is new ada.finalization.controlled with record
	vert : vap;
	txuv : tap;
	elem : eap;
end record;


procedure initialize(rect: in out ztube);
procedure finalize(rect: in out ztube);

end ztubeobj;

