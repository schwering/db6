with Ada.Text_IO;

with DB.Utils.Binary_Search;

procedure Search
is
   FROM : constant := -101;
   TO   : constant := 1000;

   type Integer_Array_Type is array (Integer range <>) of Integer;
   Integers : Integer_Array_Type(FROM .. TO);

   procedure Search_Integer0 is
      new DB.Utils.Binary_Search.Find_Best(Integer,
                                          Integer,
                                          Integer_Array_Type);
   procedure Search_Integer1 is
      new DB.Utils.Binary_Search.Find_Exact(Integer,
                                           Integer,
                                           Integer_Array_Type);

begin

   for I in Integers'Range loop
      Integers(I) := I;
   end loop;

   for I in Integers'Range loop
      declare
         S : Boolean;
         J : Integer;
      begin
         Search_Integer0(Integers, Integers(I), S, J);
         if not S or I /= J then
            Ada.Text_IO.Put_Line("Failure of best search");
            Ada.Text_IO.Put_Line("S = "& Boolean'Image(S));
            Ada.Text_IO.Put_Line("I ="& Integer'Image(I));
            Ada.Text_IO.Put_Line("J ="& Integer'Image(J));
            Ada.Text_IO.Put_Line("Searched ="& Integer'Image(Integers(I)));
            Ada.Text_IO.Put_Line("Found    ="& Integer'Image(Integers(J)));
         end if;
         Search_Integer1(Integers, Integers(I), S, J);
         if not S or I /= J then
            Ada.Text_IO.Put_Line("Failure of exact search");
            Ada.Text_IO.Put_Line("S = "& Boolean'Image(S));
            Ada.Text_IO.Put_Line("I ="& Integer'Image(I));
            Ada.Text_IO.Put_Line("J ="& Integer'Image(J));
            Ada.Text_IO.Put_Line("Searched ="& Integer'Image(Integers(I)));
            Ada.Text_IO.Put_Line("Found    ="& Integer'Image(Integers(J)));
         end if;
      end;
   end loop;

end Search;

