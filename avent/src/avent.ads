
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


package avent is

	-- jump: 3=>castle, 4=>temple, 
	--			5=>maze5, 6=>maze6, 7=>maze7, 
	--			8=>lab8, 9=>maze9
	--			any other number => normal start
	--
	-- note that non-normal starts are NOT playable...
	--			they only allow observing effects of changes.
	--
	-- note also:  chapter must be "in out" if jump is nonzero
	--      because jump will determine what the chapter must be.
	--
	procedure aventure( 
		inchapter: integer; 
		jump: integer:=0;
		resume: boolean:=false;
		HiRes: boolean:=false
		);

end avent;

