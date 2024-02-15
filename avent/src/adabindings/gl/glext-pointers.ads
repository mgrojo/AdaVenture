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


package GLext.Pointers is

   -- GLchar_Pointer
   --
   type GLchar_Pointer is access all glext.GLchar;

   type GLchar_Pointer_array is
     array (C.size_t range <>)
            of aliased GLchar_Pointer;

   -- GLintptr_Pointer
   --
   type GLintptr_Pointer is access all glext.GLintptr;

   type GLintptr_Pointer_array is
     array (C.size_t range <>)
            of aliased GLintptr_Pointer;

   -- GLsizeiptr_Pointer
   --
   type GLsizeiptr_Pointer is access all glext.GLsizeiptr;

   type GLsizeiptr_Pointer_array is
     array (C.size_t range <>)
            of aliased GLsizeiptr_Pointer;


   -- GLint64_Pointer
   --
   type GLint64_Pointer is access all glext.GLint64;

   type GLint64_Pointer_array is
     array (C.size_t range <>)
            of aliased GLint64_Pointer;


   -- GLuint64_Pointer
   --
   type GLuint64_Pointer is access all glext.GLuint64;

   type GLuint64_Pointer_array is
     array (C.size_t range <>)
            of aliased GLuint64_Pointer;

end GLext.Pointers;
