-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Utils.Binary_Search is

   procedure Find_Equal
     (Arr   : in  Array_Type;
      Item  : in  Item_Type;
      Found : out Boolean;
      Index : out Index_Type)
   is
      From : Index_Type := Arr'First;
      To   : Index_Type := Arr'Last;
   begin
      while From <= To loop
         declare
            I : constant Index_Type := (From + To) / 2;
         begin
            if Item <= Arr(I) then -- Left half or found.
               if Item = Arr(I) then -- Found.
                  Index := I;
                  Found := True;
                  return;
               else -- Left half.
                  To := I - 1;
               end if;
            else -- Right half.
               From := I + 1;
            end if;
         end;
      end loop;
      Found := False;
   end Find_Equal;


   procedure Find_Less_Or_Equal
     (Container   : in  Container_Type;
      First_Index : in  Index_Type;
      Last_Index  : in  Index_Type;
      Item        : in  Item_Type;
      Found       : out Boolean;
      Index       : out Index_Type)
   is
      From : Index_Type;
      To   : Index_Type;
   begin
      From := First_Index;
      To   := Last_Index;
      while From <= To loop
         declare
            I : constant Index_Type := (From + To) / 2;
         begin
            if Item <= Get(Container, I) then -- Left half or found.
               if (I = First_Index) or else
                  not (Item <= Get(Container, I - 1)) then -- Found.
                  Index := I;
                  Found := True;
                  return;
               else -- Left half.
                  To := I - 1;
               end if;
            else -- Right half.
               From := I + 1;
            end if;
         end;
      end loop;
      Found := False;
   end Find_Less_Or_Equal;


   procedure Uniform_Find_Less_Or_Equal
     (Container   : in  Container_Type;
      First_Index : in  Extended_Index_Type;
      Last_Index  : in  Extended_Index_Type;
      Item        : in  Item_Type;
      Index       : out Extended_Index_Type)
   is
-- Triggers compiler crash:
--pragma Postcondition
--   ((Index = Invalid_Index and then
--     (First_Index > Last_Index or else
--      Compare(Item, Get(Container, Last_Index)) = Greater))
--    or else
--    (Compare(Item, Get(Container, Index)) in Less .. Equal and then
--     (Index = First_Index or else
--      Compare(Item, Get(Container, Index - 1)) = Greater)));

      One : constant Integer := 1;
      N   : constant Natural := Natural(Last_Index - First_Index + 1);

      function Delta_Table
        (J : Positive)
         return Natural
      is
         pragma Inline (Delta_Table);
      begin
         return (N + 2**(J-1)) / 2**J;
      end Delta_Table;

      function "+"
        (I : Extended_Index_Type;
         X : Natural)
         return Extended_Index_Type
      is
         pragma Inline ("+");
      begin
         return Extended_Index_Type(Integer(I) + X);
      end "+";

      function "-"
        (I : Extended_Index_Type;
         X : Natural)
         return Extended_Index_Type
      is
         pragma Inline ("-");
      begin
         return Extended_Index_Type(Integer(I) - X);
      end "-";

      function Compare
        (Item : Item_Type;
         I    : Extended_Index_Type)
         return Comparison_Result_Type is
      begin
         if I = Invalid_Index then
            return Greater;
         else
            declare
               C : constant Comparison_Result_Type
                 := Compare(Item, Get(Container, I));
            begin
               if C = Less or C = Equal then
                  if I = First_Index or else
                     Compare(Get(Container, I - One), Item) = Less then
                     return Equal;
                  else
                     return Less;
                  end if;
               else
                  return C;
               end if;
            end;
         end if;
      end Compare;

      I : Extended_Index_Type := First_Index + Delta_Table(1) - One;
      J : Positive            := 2;
      C : Comparison_Result_Type;
   begin
      loop
         C := Compare(Item, I);
         case C is
            when Equal =>
               Index := I;
               return;
            when Less =>
               declare
                  DJ : constant Natural := Delta_Table(J);
               begin
                  if DJ = 0 then
                     Index := Invalid_Index;
                     return;
                  end if;
                  I := I - DJ;
                  J := J + 1;
               end;
            when Greater =>
               declare
                  DJ : constant Natural := Delta_Table(J);
               begin
                  if DJ = 0 then
                     Index := Invalid_Index;
                     return;
                  end if;
                  I := I + DJ;
                  J := J + 1;
               end;
         end case;
      end loop;
   end Uniform_Find_Less_Or_Equal;


   procedure Uniform_Find_Less_Or_Equal2
     (Container   : in  Container_Type;
      First_Index : in  Extended_Index_Type;
      Last_Index  : in  Extended_Index_Type;
      Item        : in  Item_Type;
      Index       : out Extended_Index_Type)
   is
      One : constant Integer := 1;
      N   : constant Natural := Natural(Last_Index - First_Index + 1);

      function Delta_Table
        (J : Positive)
         return Natural
      is
         pragma Inline (Delta_Table);
      begin
         return (N + 2**(J-1)) / 2**J;
      end Delta_Table;

      function Compare
        (Item : Item_Type;
         I    : Extended_Index_Type)
         return Comparison_Result_Type is
      begin
         if I = Invalid_Index then
            return Greater;
         else
            return Compare(Item, Get(Container, I));
         end if;
      end Compare;

      function "+"
        (I : Extended_Index_Type;
         X : Natural)
         return Extended_Index_Type
      is
         pragma Inline ("+");
      begin
         return Extended_Index_Type(Integer(I) + X);
      end "+";

      function "-"
        (I : Extended_Index_Type;
         X : Natural)
         return Extended_Index_Type
      is
         pragma Inline ("-");
      begin
         return Extended_Index_Type(Integer(I) - X);
      end "-";

      I : Extended_Index_Type := First_Index + Delta_Table(1) - One;
      J : Positive            := 2;
      C : Comparison_Result_Type;
   begin
      loop
         C := Compare(Item, I);
         case C is
            when Equal =>
               declare
                  DJ : constant Natural := Delta_Table(J);
               begin
                  if DJ = 0 then
                     Index := I;
                     return;
                  elsif I > First_Index and then
                        Compare(Item, Get(Container, I - One)) = Greater
                  then
                     Index := I;
                     return;
                  end if;
                  I := I - DJ;
                  J := J + 1;
               end;
            when Less =>
               declare
                  DJ : constant Natural := Delta_Table(J);
               begin
                  if DJ = 0 then
                     Index := I;
                     return;
                  elsif I > First_Index and then
                        Compare(Item, Get(Container, I - One)) = Greater
                  then
                     Index := I;
                     return;
                  end if;
                  I := I - DJ;
                  J := J + 1;
               end;
            when Greater =>
               declare
                  DJ : constant Natural := Delta_Table(J);
                  C  : Comparison_Result_Type := Greater;
               begin
                  if DJ = 0 then
                     if I < Last_Index and C = Greater then
                        I := I + One;
                        C := Compare(Item, Get(Container, I));
                     end if;
                     if C /= Greater then
                        Index := I;
                     else
                        Index := Invalid_Index;
                     end if;
                     return;
                  end if;
                  I := I + DJ;
                  J := J + 1;
               end;
         end case;
      end loop;
   end Uniform_Find_Less_Or_Equal2;

end DB.Utils.Binary_Search;

