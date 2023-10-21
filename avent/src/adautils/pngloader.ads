
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

with gl;


package pngloader is

	-- these loaders always use RGBA-mode:

	type wraptype is (mirror, clamp, repeat);


	-- Note that OpenGL semantics assume origin @ LowerLeft;
	-- Thus, we have an inversion by default, since
	-- PNG graphics have origin @ UpperLeft.
	function loadpng( 
		wrap: wraptype; 
		pngfilename: string;
		invert : boolean := true  --set false for avatar, glyph-dictionary
		) return gl.gluint;

	function loadpng( 
		wrap: wraptype; 
		pngfilename: string; 
		wid,hit: out gl.glint;
		invert : boolean := true;  --set false for avatar, glyph-dictionary
		debug : in boolean := false
		) return gl.gluint;


	-- cubemap texture loader
	-- Note that OpenGL uses perverse semantics for cubemaps
	-- by assuming the origin @ UpperLeft;  thus no inversion.
	function loadCubePng( f1,f2,f3,f4,f5,f6 : string ) return gl.gluint;

end pngloader;
