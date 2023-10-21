
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


generic
	n : integer; -- crab=4, star=4

package itemobj is -- for crab, starfish

type texsurf is tagged private;

procedure setrect( ts: texsurf );

procedure draw( ts: texsurf;  vertbuff, uvbuff, elembuff : gluint );


private

--n : constant integer := 4; --7;
fn : constant float := float(n);
nsq : constant integer := n*n;
nvert : constant integer := 3*4*nsq;
nuv : constant integer := 2*4*nsq;
nelm : constant integer := 3*2*nsq;


type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;


type texsurf is new ada.finalization.controlled with record
vert : vap;
txuv : tap;
elem : eap;
end record;

procedure initialize(ts: in out texsurf);
procedure finalize(ts: in out texsurf);

end itemobj;
