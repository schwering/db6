-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

procedure DB.Utils.Print (S : String)
is
   procedure Internal (S : String);
   pragma Import (C, Internal, "printf");
begin
   Internal(S & ASCII.LF & ASCII.NUL);
end DB.Utils.Print;

