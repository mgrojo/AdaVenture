-- $Id: png_dump.adb,v 1.3 2016/01/10 20:53:32 sangwine Exp $
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO  - Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
--                http://png-io.sourceforge.net/                   --
--                                                                 --
--        Copyright (©) 1999-2009 Dr Stephen J. Sangwine           --
--                                sangwine@users.sourceforge.net   --
--                                                                 --
-- This software was created by Stephen J. Sangwine. He hereby     --
-- asserts his Moral Right to be identified as author of this      --
-- software.                                                       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO is free software; you can redistribute it and/or modify  --
-- it under the terms of the GNU General Public License as         --
-- published by the Free Software Foundation; either version 3 of  --
-- the License, or (at your option) any later version.             --
--                                                                 --
-- PNG_IO is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY; without even the implied warranty of      --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    --
-- GNU General Public License for more details.                    --
--                                                                 --
-- You should have received a copy of the GNU General Public       --
-- License along with this software (in the file gpl.txt).         --
-- If not access  http://www.gnu.org/licenses/                     --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Created:  30 March 2001                                         --
-- Modified:  4   May 2009 for release as part of PNG_IO.          --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Outputs a printable dump of the pixel values from a PNG file.  --
-- The values stored in a PNG file are filtered and compressed by  --
-- Zlib and are thus not very useful, if dumped using a standard   --
-- file dump utility. This program defilters and decompresses the  --
-- pixel values before dumping, thus producing something useful    --
-- for checking what is stored in a PNG. See also the companion    --
-- program PNG_Properties, which outputs information about a PNG,  --
-- but not the pixel data. The output is to standard output.       --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO, Ada.Exceptions;
use  Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO, Ada.Exceptions;

with PNG_IO;
use  PNG_IO;

with PNG_IO.Generic_Image_Dump,
     PNG_IO.Base;

procedure PNG_Dump is

  Version : constant String := "V1.3";
  Date    : constant String := "2009";

  U : constant String := "Usage: png_dump png_file";

  Page_Width : constant := 100; -- The number of columns in the output file.
begin

  if Argument_Count > 1 then
    Put_Line(U); return;
  end if;

  declare
    PNG_Filename : constant String := Argument(1);

    F : PNG_File;
  begin

    begin
      Open(F, PNG_Filename);
    exception
      when E : others =>
        Put_Line("Failed to open file: " & PNG_Filename);
        Put_Line(Exception_Information(E));
        return;
    end;

    declare
      W : constant Dimension        := Width(F);
      H : constant Dimension        := Height(F);
      B : constant Depth            := Bit_Depth(F);
      T : constant Colour_Type_Code := Colour_Type(F);
    begin

      Put_Line("PNG file: " & PNG_Filename);
      New_Line;
      Put("Width       "); Put(W);              New_Line;
      Put("Height      "); Put(H);              New_Line;
      Put("Bit Depth   "); Put(Depth'Image(B)); New_Line;
      Put("Colour type "); Put(Colour_Type_Code'Image(T) & " = " &
                               PNG_IO.Base.Colour_Type_String(T));
      New_Line(2);

      declare
        procedure Dump_Line(S : String) renames Ada.Text_IO.Put_Line;

        subtype Row is Coordinate range 0 .. H - 1;
        subtype Col is Coordinate range 0 .. W - 1;

        N : constant Positive := Positive(PNG_IO.Base.Bit_Depth_Table(B));
        
        subtype Sample is Natural range 0 .. 2 ** N;
        
        function Sample_Image(P : Sample) return String is
          W : constant Positive := PNG_IO.Base.Hex_Width_Table(B);
        begin
          return PNG_IO.Base.To_Hex_String(P, W);
        end Sample_Image;

      begin

        -- What follows has to be tailored to each colour type.

        case T is
          when Zero | Three =>

            -- Greyscale images with no alpha, or a palette image. In the
            -- latter case, what we output is the pixel value, which is an
            -- index into the palette.

            declare
              function Pixel_Image(R, C : Coordinate) return String is
              begin
                return Sample_Image(PNG_IO.Pixel_Value(F, R, C));
              end Pixel_Image;

              procedure Dump is new PNG_IO.Generic_Image_Dump(Row, Col,
                                                              Pixel_Image,
                                                              Dump_Line,
                                                              Page_Width);
            begin
              Dump;
            end;
          when Two =>

            -- RGB images with no alpha information.

            declare
              function Pixel_Image(R, C : Coordinate) return String is
              begin
                return Sample_Image(  Red_Value(F, R, C)) & ' '
                     & Sample_Image(Green_Value(F, R, C)) & ' '
                     & Sample_Image( Blue_Value(F, R, C));
              end Pixel_Image;

              procedure Dump is new PNG_IO.Generic_Image_Dump(Row, Col,
                                                              Pixel_Image,
                                                              Dump_Line,
                                                              Page_Width,
                                                              "  ");
            begin
              Dump;
            end;
          when Four =>

            -- Greyscale with alpha information.

            declare
              function Pixel_Image(R, C : Coordinate) return String is
              begin
                return Sample_Image(Pixel_Value(F, R, C)) & ' ' &
                       Sample_Image(Alpha_Value(F, R, C));
              end Pixel_Image;

              procedure Dump is new PNG_IO.Generic_Image_Dump(Row, Col,
                                                              Pixel_Image,
                                                              Dump_Line,
                                                              Page_Width,
                                                              "  ");
            begin
              Dump;
            end;

          when Six =>

            -- RGB with alpha information.

            declare
              function Pixel_Image(R, C : Coordinate) return String is
              begin
                return Sample_Image(  Red_Value(F, R, C)) & ' '
                     & Sample_Image(Green_Value(F, R, C)) & ' '
                     & Sample_Image( Blue_Value(F, R, C)) & ' '
                     & Sample_Image(Alpha_Value(F, R, C));
              end Pixel_Image;

              procedure Dump is new PNG_IO.Generic_Image_Dump(Row, Col,
                                                              Pixel_Image,
                                                              Dump_Line,
                                                              Page_Width,
                                                              "  ");
            begin
              Dump;
            end;
        end case;

        -- If the image has a palette, output a dump of the palette values.
        -- This will always happen for images of colour type 3, but if an
        -- optional palette is present, it will be dumped. Palette values
        -- are always 24-bits, that is 8-bits per sample, output in the
        -- order R, G, B. Each new line of the palette output is preceded
        -- by the index of the first palette value on that line in decimal.

        if Palette(F) then
          New_Line;
          Put("Palette size : ");
          Ada.Integer_Text_IO.Put(Palette_Size(F), Width => 3);
          New_Line;
          Put_Line("Palette values");
          declare
            function To_Hex_String(Value : Natural;
                                   Width : Natural) return String
                 renames PNG_IO.Base.To_Hex_String;
          begin
            for I in 0 .. Palette_Size(F) - 1 loop
              if I = 0 or else Ada.Text_IO.Col > Page_Width - 8 then
                New_Line;
                Ada.Integer_Text_IO.Put(I, Width => 3); Put("  ");    
              else
                Put("  ");
              end if;
              Put(To_Hex_String(Palette_R_Value(F, I), 2) & ' ');
              Put(To_Hex_String(Palette_G_Value(F, I), 2) & ' ');
              Put(To_Hex_String(Palette_B_Value(F, I), 2)      );
            end loop;
          end;
          New_Line;
        end if;
        Close(F);
      end;
    end;
    New_Line;
    Put_Line("Dumped by PNG_Dump " & Version &
             " (PNG_IO V" & PNG_IO.Version & ") © " & Date);
    Put_Line(" using Zlib version " & Zlib_Version);
  end;

exception
  when E : others =>
    Put_Line("Program terminated by exception.");
    Put_Line(Exception_Information(E));
end PNG_Dump;
