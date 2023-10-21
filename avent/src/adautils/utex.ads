
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


-- This package renders fonts from a homemade dictionary.
-- Its main use now is to draw special glyphs, other than
-- regular text, to the screen in order to cue the user
-- as to which item the avatar is carrying.


package utex is


procedure inittext2d( fname: string;  swid, shit : integer );




--procedure print3d( 
--	text: string; 
--	ccx,ccy,ccz,ccw: float; 
--	isize: integer; 
--	distance: float);


procedure print1a( 
	ascii: integer;
	ccx,ccy,ccz,ccw: float; 
	isize: integer; 
	distance: float); -- median Z-distance



procedure print2d(text: string;	xcen,ycen: float;	size: integer );

procedure print2d(ascii: integer;  xcen,ycen: float;	size: integer );


procedure cleanuptext;


end utex;
