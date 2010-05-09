-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Interfaces.C;

procedure DB.Utils.Print (S : String)
is
   use Interfaces;
   function Internal (S : C.char_array) return C.int;
   pragma Import (C, Internal, "puts");
   I : constant C.int := Internal(C.To_C(S & ASCII.LF));
   pragma Unreferenced (I);
begin
   null;
end DB.Utils.Print;

