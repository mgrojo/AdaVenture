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



with Ada.Exceptions;
with Interfaces.C;
with Text_io; use Text_io;

package body SysUtils is

	-- This method generally works if the
	-- command sent to the "shell" proc is valid.

	-- This is a mini-binding to call the C-function "system"
	-- from stdlib.h.  It is used as a blocking spawn.


	use interfaces.c;

   procedure bShell ( Str: in string;  Success: out boolean ) is
     stat: integer;

     --returns zero if Ok:
     function Sys
       ( cmd: char_array ) return integer;

     pragma Import ( convention    => C,
                     entity        => Sys,
                     external_name => "system" ); --stdlib.h
   begin --shell

     stat := Sys ( to_c(str) );

     success := (stat=0);

   exception
      when the_error: others =>
         put_line("SysUtils: " &
           Ada.Exceptions.Exception_Information(the_error)
         );

   end bshell;



	-- It is a nonblocking spawn with the "&"


	use interfaces.c;

   procedure Shell ( Str: in string;  Success: out boolean ) is
     stat: integer;

     --returns zero if Ok:
     function Sys
       ( cmd: char_array ) return integer;

     pragma Import ( convention    => C,
                     entity        => Sys,
                     external_name => "system" ); --stdlib.h
   begin --shell

     stat := Sys ( to_c(str&"&") );

     success := (stat=0);

   exception
      when the_error: others =>
         put_line("SysUtils: " &
           Ada.Exceptions.Exception_Information(the_error)
         );

   end shell;



end SysUtils;
