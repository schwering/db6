-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Utils.Binary_Search is

   procedure Find_Exact
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
   end Find_Exact;


   procedure Find_Best
     (Arr   : in  Array_Type;
      Item  : in  Item_Type;
      Found : out Boolean;
      Index : out Index_Type)
   is
      From : Index_Type;
      To   : Index_Type;
   begin
      if Arr'Length = 0 then
         Found := False;
         return;
      end if;
      From := Arr'First;
      To   := Arr'Last;
      while From <= To loop
         declare
            I : constant Index_Type := (From + To) / 2;
         begin
            if Item <= Arr(I) then -- Left half or found.
               if (I = Arr'First) or else not (Item <= Arr(I-1)) then -- Found.
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
   end Find_Best;


   procedure Find_Best_In_Container
     (Container : in  Container_Type;
      Item      : in  Item_Type;
      Found     : out Boolean;
      Index     : out Index_Type)
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
                  not (Item <= Get(Container, I-1)) then -- Found.
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
   end Find_Best_In_Container;

end DB.Utils.Binary_Search;

