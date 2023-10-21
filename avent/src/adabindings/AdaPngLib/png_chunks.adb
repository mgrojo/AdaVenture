-- $Id: png_chunks.adb,v 1.4 2016/01/10 20:53:32 sangwine Exp $
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
-- Created:  15 December 2002                                      --
-- Modified:  3      May 2009 for release as part of PNG_IO.       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Outputs to standard output, a dump of the chunk offsets, names  --
-- and lengths within a PNG file, plus the total file size and the --
-- total IDAT size. For each chunk, the information output is the  --
-- address or offset from the start of the file (in hex), the name --
-- of the chunk, and the chunk length (in decimal). This program   --
-- does not read the chunk data, nor verify the chunk CRC values.  --
-- Therefore if there are errors in the chunk length the output of --
-- subsequent chunks may be meaningless.                           --
--                                                                 --
-- See also the companion programs png_properties, which outputs   --
-- information about a PNG file, but not the pixel data, and       --
-- png_dump which outputs the decompressed pixel data.             --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Command_Line, Ada.Text_IO, Ada.Exceptions;
use  Ada.Command_Line, Ada.Text_IO, Ada.Exceptions;

with Ada.Streams, Ada.Streams.Stream_IO;
use  Ada.Streams, Ada.Streams.Stream_IO;

with Interfaces;
use  Interfaces;

with PNG_IO.Base;
use  PNG_IO.Base;

procedure PNG_Chunks is

  Version : constant String := "V1.1";
  Date    : constant String := "2009";

  U : constant String := "Usage: png_chunks png_file";

begin

  if Argument_Count /= 1 then
    Put_Line(U); return;
  end if;

  declare
    PNG_Filename : constant String := Argument(1);

    F : Ada.Streams.Stream_IO.File_Type;
  begin

    begin
      Open(F, In_File, PNG_Filename);
    exception
      when E : others =>
        Put_Line("Failed to open file: " & PNG_Filename);
        Put_Line(Exception_Information(E));
        return;
    end;

    -- Verify that the file really is a PNG file. If the signature in the
    -- first 8 bytes is OK, assume that it is a PNG file. The code below
    -- handles the case where the file has less than 8 bytes, because Read
    -- will return even if there are less than 8 bytes read. Because we
    -- initialise Signature to be zeros, any bytes that are not read from
    -- the file will remain as zeros and will fail to match with the true
    -- signature.
    
    declare
      Signature : Stream_Element_Array(1 .. 8) := (others => 0);
      Last      : Stream_Element_Offset;
    begin
      Read(F, Signature, Last);
      if Signature /= PNG_Signature then
        Put_Line(PNG_Filename & " is not a PNG file: incorrect signature");
        Close(F);
        return;
      end if;
    end;

    declare

      SF : constant Stream_Access := Stream(F);
      
      function Next_Long return Unsigned_32 is
        -- Reads the next four bytes from the current index position in
        -- file F and returns them as an unsigned 32-bit value.
      begin
        return Shift_Left(Unsigned_32(Unsigned_8'Input(SF)), 24) +
               Shift_Left(Unsigned_32(Unsigned_8'Input(SF)), 16) +
               Shift_Left(Unsigned_32(Unsigned_8'Input(SF)),  8) +
                          Unsigned_32(Unsigned_8'Input(SF));
      end Next_Long;

      File_Size : constant Ada.Streams.Stream_IO.Count := Size(F);

      IDAT_Total_Size : Unsigned_32 := 0;

      function Number_of_Hex_Digits(H : Ada.Streams.Stream_IO.Count)
                                 return Ada.Streams.Stream_IO.Count is
      begin
        if H < 16 then
          return 1;
        else
          return Number_of_Hex_Digits(H/16) + 1;
        end if;
      end Number_of_Hex_Digits;

      -- The address field width (minimum 4 hex characters).
      W : constant Ada.Streams.Stream_IO.Count :=
                   Ada.Streams.Stream_IO.Count'Max(4,
                                          Number_of_Hex_Digits(File_Size));
    begin

      -- Output the header information for the dump.

      Put_Line("PNG file: " & PNG_Filename);
      New_Line;

      Put("Addr"); Set_Col(Ada.Text_IO.Count(W + 2)); Put_Line("Name Length");

      loop -- For each chunk in the file.

        declare
          Chunk_Index  : constant Ada.Streams.Stream_IO.Count :=
                                  Ada.Streams.Stream_IO.Index(F);
          Chunk_Length : constant Unsigned_32 := Next_Long;
          Chunk_Type   : constant Unsigned_32 := Next_Long;
        begin

          if Chunk_Type = IDAT then
            IDAT_Total_Size := IDAT_Total_Size + Chunk_Length;
          end if;

          -- Notice that there is no space between the name and the
          -- image of the chunk length, because the Image attribute
          -- supplies a space (LRM95 3.5(31)) even for an unsigned type.
            
          Put_Line(To_Hex_String(Natural(Chunk_Index), Natural(W)) & ' ' &
                   To_Chunk_Name(Chunk_Type) &
                   Unsigned_32'Image(Chunk_Length));

          -- The index when Chunk_Index was computed pointed to the first
          -- byte of the chunk length. We now want to skip to the first
          -- byte of the next chunk, so we add the chunk length (which
          -- counts only the data) plus 12 (4 for the chunk length, 4 for
          -- the chunk type and 4 for the CRC). Note that if the current
          -- chunk is the IEND chunk, the result will be one byte past the
          -- end of the IEND chunk, which should be past the end of the
          -- file.

          Set_Index(F, Chunk_Index +
                       Ada.Streams.Stream_IO.Count(Chunk_Length) + 12);

          exit when Chunk_Type = IEND;
        end;
      end loop;

      if not End_of_File(F) then
        Put_Line("Warning: File contains bytes beyond IEND chunk.");
      end if;
      Close(F);

      New_Line;
      Put_Line("File size: " & Ada.Streams.Stream_IO.Count'Image(File_Size)
                             & " bytes");
      Put_Line("IDAT size: " & Unsigned_32'Image(IDAT_Total_Size)
                             & " bytes");
      New_Line;
      Put_Line("Dumped by PNG_Chunks " & Version &
               " (PNG_IO V" & PNG_IO.Version & ") © " & Date);
    end;
  end;

exception
  when E : others =>
    Put_Line("Program terminated by exception.");
    Put_Line(Exception_Information(E));
end PNG_Chunks;
