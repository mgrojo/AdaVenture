pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package cls_h is

   procedure clear  -- cls.h:4
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z5clearv";

   procedure cursorHome  -- cls.h:5
   with Import => True, 
        Convention => CPP, 
        External_Name => "_Z10cursorHomev";

end cls_h;
