
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




package troomobj is
-- for rectangular textured interiors
-- optionally supports fixed floor-gaps (for pools) 
-- or fixed Z-door-gaps for see-thru doors


type room is tagged private;


procedure setroomwithZMdoor( 
	rm: in out room;  
	dx,dy,  xc,yc,zc, xr,yr,zr, sx,sy,sz : float );

procedure setroomwithZPdoor( 
	rm: in out room;  
	dx,dy,  xc,yc,zc, xr,yr,zr, sx,sy,sz : float );



procedure setrect( rm: in out room;  xc,yc,zc, xr,yr,zr, sx,sy,sz : float );

procedure draw( rm: room;  vertbuff, uvbuff, elembuff : gluint );
procedure ldraw( rm: room;  vertbuff, uvbuff, normbuff, elembuff : gluint );


private

nvert: constant integer := 84; --5*12=60 <=7*12 ...was 72..96; <=8*12
nuv  : constant integer := 56; --5*8 =40 <=7*8  ...was 48..64; <=8*8
nelm : constant integer := 42; --5*6 =30 <=7*6  ...was 36..48; <=8*6



type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;



type room is new ada.finalization.controlled with record
vert, norm : vap;
txuv : tap;
elem : eap;
nv,nt,ne: integer; --actuals
end record;


procedure initialize(rm: in out room);
procedure finalize(rm: in out room);


end troomobj;
