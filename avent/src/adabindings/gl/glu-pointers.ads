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


package GLU.Pointers is

   -- GLUnurbs_Pointer
   --
   type GLUnurbs_Pointer is access all GLUnurbs;

   type GLUnurbs_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUnurbs_Pointer;

   -- GLUquadric_Pointer
   --
   type GLUquadric_Pointer is access all GLUquadric;

   type GLUquadric_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUquadric_Pointer;

   -- GLUtesselator_Pointer
   --
   type GLUtesselator_Pointer is access all GLUtesselator;

   type GLUtesselator_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUtesselator_Pointer;

   -- GLUnurbsObj_Pointer
   --
   type GLUnurbsObj_Pointer is access all GLUnurbsObj;

   type GLUnurbsObj_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUnurbsObj_Pointer;

   -- GLUquadricObj_Pointer
   --
   type GLUquadricObj_Pointer is access all GLUquadricObj;

   type GLUquadricObj_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUquadricObj_Pointer;

   -- GLUtesselatorObj_Pointer
   --
   type GLUtesselatorObj_Pointer is access all GLUtesselatorObj;

   type GLUtesselatorObj_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUtesselatorObj_Pointer;

   -- GLUtriangulatorObj_Pointer
   --
   type GLUtriangulatorObj_Pointer is access all GLUtriangulatorObj;

   type GLUtriangulatorObj_Pointer_array is
     array (C.size_t range <>)
            of aliased GLUtriangulatorObj_Pointer;

   -- a_GLUfuncptr_Pointer
   --
   type a_GLUfuncptr_Pointer is access all a_GLUfuncptr;

   type a_GLUfuncptr_Pointer_array is
     array (C.size_t range <>)
            of aliased a_GLUfuncptr_Pointer;

end GLU.Pointers;
