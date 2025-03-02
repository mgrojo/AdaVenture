
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


pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
--limited 
with alc_h;
with al_h;
with System;
with Interfaces.C.Extensions;

package oal_hpp is

   function opendevice return access alc_h.ALCdevice  -- oal.hpp:8
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10opendevicev";

   function createcontext (dev : access alc_h.ALCdevice) return access alc_h.ALCcontext  -- oal.hpp:10
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z13createcontextP16ALCdevice_struct";

   procedure makecontextcurrent (con : access alc_h.ALCcontext)  -- oal.hpp:12
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z18makecontextcurrentP17ALCcontext_struct";

   procedure genbuffers (buf : access al_h.ALuint)  -- oal.hpp:14
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10genbuffersPj";

   procedure bufferdata
     (buf : al_h.ALuint;
      fmt : al_h.ALuint;
      dat : System.Address;
      sz : int;
      rt : int)  -- oal.hpp:16
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10bufferdatajjPKvii";

   procedure gensources (sid : access al_h.ALuint)  -- oal.hpp:23
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10gensourcesPj";

   procedure sourcei (sid : al_h.ALuint; buf : al_h.ALuint)  -- oal.hpp:25
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z7sourceijj";

   procedure cleanup1 (sid : access al_h.ALuint; buf : access al_h.ALuint)  -- oal.hpp:27
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z8cleanup1PjS_";

   procedure cleanup2 (con : access alc_h.ALCcontext; dev : access alc_h.ALCdevice)  -- oal.hpp:33
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z8cleanup2P17ALCcontext_structP16ALCdevice_struct";

   procedure sourceplay (sid : al_h.ALuint)  -- oal.hpp:40
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10sourceplayj";

   procedure sourcestop (sid : al_h.ALuint)  -- oal.hpp:41
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10sourcestopj";

   function is_stopped (sid : al_h.ALuint) return Extensions.bool  -- oal.hpp:43
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10is_stoppedj";

  -- 18oct21 fastrgv
   function is_true return Extensions.bool  -- oal.hpp:45
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z7is_truev";

end oal_hpp;
