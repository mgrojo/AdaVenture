
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


package w3treeobj is
--
-- Triple-plane implementation for a 3D tree graphic...
--
-- 3 planes intersecting along a vertical Y-axis
-- are spaced @ 120 degree increments, each showing an
-- asymmetric tree texture [with transparent background].
-- Direction alternates according to the following 
-- arrangement (top view):
--
--   R    L
--    \  /
--	    \/
--L -------- R
--     /\
--    /  \
--   R    L
--
-- All 3 planes show the entire texture on both sides,
-- where, of course, opposite sides are reversed.
--
-- The draw procedure input requires the X,Z offsets from
-- the tree to the viewpoint (xeye-xtree, zeye-ztree);
-- This enables each of the 6 wings to be drawn in order 
-- from furthest to nearest.  This regimen makes it
-- simple to draw nice looking trees.  As usual, 
-- one still must draw individual trees in 
-- sorted order from furthest to nearest.
--
-- This tree is defined @ the origin with unit radius.
-- The position, height, width and viewpoint location 
-- are set using appropriate uniform values for the 
-- vertex shader;  likewise for wind-sway parameters.  
-- Thus, one tree object suffices for many instances 
-- of trees or grasses.


type treeangle is tagged private;

procedure setrect( rect: treeangle );

-- xe=xeye-xtree
-- ze=zeye-ztree
procedure draw( 
	rect: treeangle; 
	vertbuff, uvbuff, elembuff : gluint;
	xe,ze: float
	);



private

-- Z- : 1..24
-- Z+ : 25..48
-- X- : 49..72
-- X+ : 73..96
-- W- : 97..120
-- W+ :121..144
nvert : constant integer :=144; --48;
nuv   : constant integer := 96; --32;
nelm  : constant integer := 72; --24;

type varray is array(1..nvert/4) of float;
type vap is access varray;

type tarray is array(1..nuv/4) of float;
type tap is access tarray;

type earray is array(1..nelm/4) of glushort;
type eap is access earray;

type treeangle is new ada.finalization.controlled with record
	vertzm,vertzp,vertxm,vertxp,vertwm,vertwp : vap;
	txuvzm,txuvzp,txuvxm,txuvxp,txuvwm,txuvwp : tap;
	elemzm,elemzp,elemxm,elemxp,elemwm,elemwp : eap;
end record;


procedure initialize(rect: in out treeangle);
procedure finalize(rect: in out treeangle);

end w3treeobj;

