with Ada.Text_IO;

with DB.Utils.Binary_Search;
with DB.Utils;

procedure Search
is
   FROM : constant := 1;---101;
   TO   : constant := 1000;

   type Integer_Array_Type is array (Integer range <>) of Integer;
   type Def_Integer_Array_Type is array (Integer range From .. To) of Integer;
   Integers     : Integer_Array_Type(FROM .. TO);
   Def_Integers : Def_Integer_Array_Type;

   function Get (C : Def_Integer_Array_Type; I : Integer) return Integer is
   begin
      return C(I);
   end Get;

   function Compare (I, J : Integer) return DB.Utils.Comparison_Result_Type is
   begin
      if I - J < 0 then
         return DB.Utils.Less;
      elsif I = J then
         return DB.Utils.Equal;
      else
         return DB.Utils.Greater;
      end if;
   end Compare;

   procedure U is
      new DB.Utils.Binary_Search.Uniform_Find_Best_In_Container
            (Def_Integer_Array_Type, Integer, Integers'First - 1, Integer,
             Get, Compare);
   procedure Search_Integer0 is
      new DB.Utils.Binary_Search.Find_Best(Integer,
                                          Integer,
                                          Integer_Array_Type);
   procedure Search_Integer1 is
      new DB.Utils.Binary_Search.Find_Exact(Integer,
                                           Integer,
                                           Integer_Array_Type);

begin
   for I in Def_Integers'Range loop
      Def_Integers(I) := I/2;
   end loop;

   for I in Def_Integers'Range loop
      declare
         S : Boolean;
         J : Integer;
      begin
         U(Def_Integers, Def_Integers'First, Def_Integers'Last,
           Def_Integers(I), J);
         if J = Integers'First - 1 or else
            Def_Integers(I) /= Def_Integers(J) then
            Ada.Text_IO.Put_Line("Failure of best search");
            Ada.Text_IO.Put_Line("S = "& Boolean'Image(S));
            Ada.Text_IO.Put_Line("I ="& Integer'Image(I));
            Ada.Text_IO.Put_Line("J ="& Integer'Image(J));
            Ada.Text_IO.Put_Line("Searched ="& Integer'Image(Integers(I)));
            --Ada.Text_IO.Put_Line("Found    ="& Integer'Image(Integers(J)));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line("Alle"&
                        Integer'Image(Def_Integers'Last-Def_Integers'First+1) &
                        " Dinger gefunden");

   if True then
      return;
   end if;

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

