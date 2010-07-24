-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Interfaces.C;

package body DB.Maps.Test_Utils is

   procedure Unlink (File_Name : in String)
   is
      use Interfaces.C;

      procedure Unlink (Pathname : char_array);
      pragma Import (C, Unlink, "unlink");

      C_File_Name : constant char_array := To_C (File_Name);
   begin
      Unlink (C_File_Name);
   end Unlink;

end DB.Maps.Test_Utils;

