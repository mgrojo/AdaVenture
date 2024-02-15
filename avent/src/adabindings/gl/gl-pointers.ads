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


with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;



package GL.Pointers is

   use interfaces.C;


   -- GLenum_Pointer
   --
   type GLenum_Pointer is access all GLenum;

   type GLenum_Pointer_array is
     array (C.size_t range <>)
   of aliased GLenum_Pointer;


   -- GLboolean_Pointer
   --
   type GLboolean_Pointer is access all GLboolean;

   type GLboolean_Pointer_array is
     array (C.size_t range <>)
   of aliased GLboolean_Pointer;


   -- GLbitfield_Pointer
   --
   type GLbitfield_Pointer is access all GLbitfield;

   type GLbitfield_Pointer_array is
     array (C.size_t range <>)
   of aliased GLbitfield_Pointer;



   -- GLbyte_Pointer
   --
   type GLbyte_Pointer is access all GLbyte;

   type GLbyte_Pointer_array is
     array (C.size_t range <>)
   of aliased GLbyte_Pointer;


   -- GLshort_Pointer
   --
   type GLshort_Pointer is access all GLshort;

   type GLshort_Pointer_array is
     array (C.size_t range <>)
   of aliased GLshort_Pointer;


   -- GLint_Pointer
   --
   type GLint_Pointer is access all GLint;

   type GLint_Pointer_array is
     array (C.size_t range <>)
   of aliased GLint_Pointer;


   -- GLubyte_Pointer
   --
   type GLubyte_Pointer is access all GLubyte;

   type GLubyte_Pointer_array is
     array (C.size_t range <>)
   of aliased GLubyte_Pointer;


   -- GLushort_Pointer
   --
   type GLushort_Pointer is access all GLushort;

   type GLushort_Pointer_array is
     array (C.size_t range <>)
   of aliased GLushort_Pointer;


   -- GLuint_Pointer
   --
   type GLuint_Pointer is access all GLuint;

   type GLuint_Pointer_array is
     array (C.size_t range <>)
   of aliased GLuint_Pointer;


   -- GLsizei_Pointer
   --
   type GLsizei_Pointer is access all GLsizei;

   type GLsizei_Pointer_array is
     array (C.size_t range <>)
   of aliased GLsizei_Pointer;


   -- GLfloat_Pointer
   --
   type GLfloat_Pointer is access all GLfloat;

   type GLfloat_Pointer_array is
     array (C.size_t range <>)
   of aliased GLfloat_Pointer;


   -- GLclampf_Pointer
   --
   type GLclampf_Pointer is access all GLclampf;

   type GLclampf_Pointer_array is
     array (C.size_t range <>)
   of aliased GLclampf_Pointer;


   -- GLdouble_Pointer
   --
   type GLdouble_Pointer is access all GLdouble;

   type GLdouble_Pointer_array is
     array (C.size_t range <>)
   of aliased GLdouble_Pointer;


   -- GLclampd_Pointer
   --
   type GLclampd_Pointer is access all GLclampd;

   type GLclampd_Pointer_array is
     array (C.size_t range <>)
   of aliased GLclampd_Pointer;

end GL.Pointers;
