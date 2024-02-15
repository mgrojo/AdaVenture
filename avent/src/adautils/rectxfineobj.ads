
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

package rectxfineobj is -- for untextured rectangular exterior with >1 squares per face

type rectfine is tagged private;

procedure setrect( rect: rectfine;
xm,xp, ym,yp, zm,zp : out float
);

procedure draw( rect: rectfine; vertbuff, elembuff : gluint );

private

--nperedge : constant integer := 3;

nfaces : constant integer := 6;
cpp : constant integer := 3; -- coords per point

nsq : constant integer := nperedge*nperedge; -- 1
npts : constant integer := 4*nsq; -- 4
cpf : constant integer := npts * cpp; -- coords per face (12)

nvert : constant integer := nfaces*cpf; --72;
nelm : constant integer := nvert/2; --36;

type varray is array(1..nvert) of float;
type vap is access varray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type rectfine is new ada.finalization.controlled with record
	vert : vap;
	elem : eap;
end record;


procedure initialize(rect: in out rectfine);
procedure finalize(rect: in out rectfine);

end rectxfineobj;

