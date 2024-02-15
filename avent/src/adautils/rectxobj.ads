
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




package rectxobj is -- rectangular object, no texture, no keepout, for special frag.shader


type rectx is tagged private;

procedure setrect( rx: rectx;
xc,yc,zc, xr,yr,zr : float
);


procedure draw( rx: rectx;  vertbuff, elembuff : gluint );



private

nvert : constant integer := 72;
nelm : constant integer := 36;


type varray is array(1..nvert) of float;
type vap is access varray;


type earray is array(1..nelm) of glushort;
type eap is access earray;


type rectx is new ada.finalization.controlled with record
vert : vap;
elem : eap;
end record;

procedure initialize( rx: in out rectx );
procedure finalize( rx: in out rectx );


end rectxobj;

