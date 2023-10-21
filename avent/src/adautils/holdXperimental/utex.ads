
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


with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

-------------------------------------------------------------

with System;
with Interfaces.C;
use  type interfaces.c.unsigned;
with Interfaces.C.Pointers;
with interfaces.c.strings;

----------------------------------------------------------------






package utex is

use gl;

procedure inittext2d( fname: string;  swid, shit : integer );



-- optional mode affects 1st parm (src) of blendfunc thusly:
-- 0 gl_src_alpha (default)
-- 1 gl_one (white)
-- 2 gl_zero (black)
-- 3 gl_one_minus_dst_color (constrast w/bkgd)
-- 4 gl_constant_color ?
-- 5 gl_one_minus_constant_color ??

-- original baseline
procedure printex( text: string; x,y,size: integer; mode: integer:=0);


procedure print3d( 
	text: string; 
	ccx,ccy,ccz,ccw: float; 
	isize: integer; 
	distance: float;
	mode: integer := 0
	);


procedure print2d(
text: string;	xcen,ycen: float;	
size: integer;
mode: integer:=0
);

procedure cleanuptext;


end utex;
