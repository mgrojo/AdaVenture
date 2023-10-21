-- $Id: png_properties.adb,v 1.4 2016/01/10 20:53:32 sangwine Exp $
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
-- Created:   22 October 1999                                      --
-- Modified:   6     May 2009 for release as part of PNG_IO.       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Reads a PNG image and outputs properties of the file and image  --
-- to standard output.                                             --
--                                                                 --
-- See also the companion programs png_chunks, which outputs a     --
-- list of the chunks in a PNG file, and png_dump which outputs    --
-- the decompressed pixel data.                                    --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO, Ada.Exceptions;
use  Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO, Ada.Exceptions;

with Ada.Streams;
use  Ada.Streams;

with PNG_IO, PNG_IO.Base;
use  PNG_IO, PNG_IO.Base;

procedure PNG_Properties is

  Version : constant String := "V1.1.1";
  Date    : constant String := "2009";

  U : constant String :=
      "Usage: png_properties png_file ";
      
  None : constant String := "NONE";
begin

  if Argument_Count /= 1 then
    Put_Line(U); return;
  end if;

  declare
    PNG_Filename : constant String  := Argument(1);
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
      D : constant Depth            := Bit_Depth(F);
      T : constant Colour_Type_Code := Colour_Type(F);
      I : constant Boolean          := Interlaced(F);
    begin
      Put_Line("Image information from file: " & PNG_Filename);
      Put("Width:                 "); Put(W); New_Line;
      Put("Height:                "); Put(H); New_Line;
      declare
        package Depth_IO is new Enumeration_IO(Depth);
        use     Depth_IO;
        package Colour_Type_IO is new Enumeration_IO(Colour_Type_Code);
        use     Colour_Type_IO;
        package Boolean_IO is new Enumeration_IO(Boolean);
        use     Boolean_IO;
      begin
        Put("Bit Depth:             "); Put(D); New_Line;
        Put("Colour type:           "); Put(T);
                                        Put(" = " & Colour_Type_String(T));
        New_Line;
        Put("Interlaced:            "); Put(I); New_Line;
        Put("Palette:               ");
          if Palette(F) then
            Put(Palette_Size(F), Width => 4); Put_Line(" entries.");
          else
            Put_Line(None);
          end if;
        if Standard_RGB(F) then
          Put_Line("Standard RGB");
          Put_Line("Rendering intent:      " & Rendering_Intent'Image(SRGB_Rendering(F)));
        end if;
        Put("Gamma:                 "); if Gamma(F) then
                                          Put(Gamma_Value(F));
                                        else
                                          Put(None);
                                        end if; New_Line;
        Put("Chroma:                "); 
          if Chromaticity(F) then
            declare
              C : constant Positive_Count := Col(Standard_Output);
            begin
                          Put(  White_Point(F).X); Put(  White_Point(F).Y);
              Set_Col(C); Put(  Red_Primary(F).X); Put(  Red_Primary(F).Y);
              Set_Col(C); Put(Green_Primary(F).X); Put(Green_Primary(F).Y);
              Set_Col(C); Put( Blue_Primary(F).X); Put( Blue_Primary(F).Y);
            end;
          else
            Put(None);
          end if; New_Line;
        Put("Physical:              "); if Physical(F) then
                                          declare
                                            C : constant Positive_Count := Col(Standard_Output);
                                          begin
                                            Put("Unit unknown: ");
                                            Put(Unit_Unknown(F));
                                            Set_Col(C);
                                            Put("Unit meter  : ");
                                            Put(Unit_Meter(F));
                                            Set_Col(C);
                                            Put(Physical_Value(F).X);
                                            Put(Physical_Value(F).Y);
                                            if Unit_Meter(F) then
                                              Set_Col(C);
                                              Put("=> Image width  = ");
                                              Put(Width(F)*1000/Physical_Value(F).X);
                                              Put("mm");
                                              Set_Col(C);
                                              Put("=> Image height = ");
                                              Put(Height(F)*1000/Physical_Value(F).Y);
                                              Put("mm");
                                            end if;
                                          end;
                                        else
                                          Put(None);
                                        end if; New_Line(2);
        declare
          N : constant Natural := NText(F);
        begin
          Put("Number of text chunks: "); Put(N); New_Line;
          if N > 0 then New_Line; end if;
          for I in 1 .. N loop
            Put("Keyword:" & Text_Keyword(F, I)); New_Line;
            Put("Text   :" & Text_String (F, I)); New_Line;
          end loop;
        end;

        declare
          N : constant Natural := Ancillary_Chunk_Count(F);
        begin
          if N > 0 then
            Put_Line("Ancillary chunks: " & Natural'Image(N)); New_Line;
            for I in 1 .. N loop
              declare
                C : constant Chunk := Ancillary_Chunk(F, I);
              begin
                Put_Line("Name: " & Name(C));
                declare
                  D : constant Stream_Element_Array := Data(C);
                begin
                  Put_Line("Size: " & Natural'Image(D'Length));
                  Put("Data: ");
                  for J in D'range loop
                    Put(To_Hex_String(Natural(D(J)), 2) & ' ');
                    if J mod 16 = 0 then
                      New_Line; Put("      ");
                    end if;
                  end loop;
                  New_Line(2);
                end;
              end;
            end loop;
          end if;
        end;
      end;
    end;
    Close(F);
  end;
  
  New_Line;
  Put_Line("Dumped by PNG_Properties " & Version &
           " (PNG_IO V" & PNG_IO.Version & ") © " & Date);
exception
  when E : others =>
    Put_Line("Program terminated by exception.");
    Put_Line(Exception_Information(E));
end PNG_Properties;
