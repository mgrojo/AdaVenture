
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

with gametypes;

with system;
with text_io;


package body zoomwheel is

	null_window: exception;

procedure scroll(
	addr: access GLFWwindow;
	dx  : double;
	dy  : double ) is
begin

	if abs(dy)>0.01 then
		if dy<0.0 then
			zdist:=0.99*zdist;
			if zdist<zmin then zdist:=zmin; end if;
		else
			zdist:=1.01*zdist;
			if zdist>zmax then zdist:=zmax; end if;
		end if;
	end if;

	gametypes.camdist := float(zdist); --9feb22: coordinate w/camera

end scroll;


procedure enable( win: access GLFWwindow ) is
	use text_io;
	use system;
	mySFaccess : GLFWscrollfun; -- glfw3_h.ads:1239
begin

	-- must be called AFTER creating window to activate
	-- mouse scroll wheel.
	mySFaccess := glfwSetScrollCallback(win, zoomwheel.scroll'access);

end enable;

end zoomwheel;
