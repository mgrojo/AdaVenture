
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


with system;
with interfaces.c;
with glfw3;

package zoomwheel is

use glfw3;
use interfaces.c;
use type interfaces.c.double;

	zdefault: constant double := 1.0; --default
	zdist: double := zdefault;
	zmin: constant double := 0.1;
	zmax: constant double := 100.0;


procedure scroll(
	addr: access GLFWwindow;
	dx  : double;
	dy  : double );
pragma Convention(C,scroll);

procedure enable( win: access GLFWwindow );

end zoomwheel;
