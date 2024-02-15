
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
	n : integer; -- 10(water@lev1,4), 33(lava@lev3), 120(water@lev2)

package rectsurfobj is -- rectangular surface [no texture] for special frag.shaders


type rectsurf is tagged private;


procedure setrect( rs: rectsurf;
xc,zc, xr,zr : float
);


procedure draw( rs: rectsurf;  vertbuff : gluint );



private

----------- begin private procs --------------------------

--n : constant integer := 10;
fn : constant float := float(n);
nsq : constant integer := n*n;
nvert : constant integer := 18*nsq;

type varray is array(1..nvert) of float;
type vap is access varray;

type rectsurf is new ada.finalization.controlled with record
	vert : vap;
end record;


procedure initialize(rs: in out rectsurf);
procedure finalize(rs: in out rectsurf);


end rectsurfobj;

