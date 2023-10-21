/*
--
-- Copyright (C) 2022  <fastrgv@gmail.com>
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
*/

#include <realWinTime.hpp>

#include <windows.h>
#include <tchar.h>

//#include <iostream>
//using std::cout;
//using std::endl;



int realPriority( void )
{
	if (!SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS)) {
		return 1;
	} else {
		return 0;
   }
}



int hiPriority( void )
{
	if (!SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS)) {
		return 1;
	} else {
		return 0;
   }

}



