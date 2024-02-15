
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




package avatarolay is
-- textured object made up of 6 cubelets that will
-- map to minecraft character bodies.
-- This version now allows for HeadOverlays
-- to allow turbans, crowns, hats, etc.
-------------------------------------
-- y>0.5 => head
-- y>0.0 => torso
----------------------- y<0 :
-- x>+0.5 => leg
-- x<-0.5 => leg
----------------------- abs(x)<0.5 :
-- x>0 => arm
-- x<0 => arm



type avatar is tagged private;

procedure setrect( rx: avatar );


procedure draw( rx: avatar;  vertbuff, uvbuff, elembuff : gluint );



private

-- 7 = 6 cubelets + headOverlay
nvert : constant integer := 72*7;
nelm : constant integer := 36*7;
nuv : constant integer := 48*7;


type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;


type avatar is new ada.finalization.controlled with record
vert : vap;
elem : eap;
txuv : tap;
end record;

procedure initialize( rx: in out avatar );
procedure finalize( rx: in out avatar );


end avatarolay;

