
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


package cyl2texobj is -- intersection of two cylinders, textured

	type ball is tagged private;


procedure setcyl2( bar: in out ball;  xc,yc,zc, rr : float );

procedure draw( bar: ball;  vertbuff, uvbuff, elembuff : gluint );


private

	-- the magic number 96 is due to the fact that this intersection of
	-- two cylinders is drawn using many [11] thin rectangles in each of
	-- 8 symmetric octants...
	-- i.e. 8*4 vertices, each with 3 coordinates => 8*4*3 = 96

	-- note:  cpp version had nasty memory problem on OSX when nparts=21
	--        and Ada had a stack overflow on OSX...
	--        but declaring inside gametypes.ads seemed to fix
	--        (I assume by putting on heap)
	nparts: constant integer := 7;
	nvert : constant integer := nparts*96*2;
	nuv   : constant integer := nparts*64*2;
	nelm  : constant integer := nparts*48*2;

	type varray is array(1..nvert) of aliased float;
	type vap is access varray;

	type tarray is array(1..nuv) of float;
	type tap is access tarray;

	type earray is array(1..nelm) of aliased glushort;
	type eap is access earray;



	type ball is new ada.finalization.controlled with record
		vert : vap;
		txuv : tap;
		elem : eap;
		oxc,oyc,ozc,orr : float;
	end record;

	procedure initialize(bar: in out ball);
	procedure finalize(bar: in out ball);

end cyl2texobj;

