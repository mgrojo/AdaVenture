
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

-- This package renders TTF fonts using FreeType
-- libraries, Felix Krause's FreeTypeAda binding [MIT]
-- and Dmitry Kazakov's Strings_Edit [gnuGPL2]


with gl; use gl;
with matutils; use matutils;


package ftex is

	procedure CloseFont;
   procedure InitFont (
		wid, hit : glint;
		Font_File : String );


   procedure print2d (
      Text   : String; 
		X, Y, Scale : float;
      Colour : vec4 );

   procedure print3d (
      Text   : String; 
		ccx,ccy,ccz,ccw, meanZ : float;
		Scale : float;
      Colour : vec4 );


end ftex;
