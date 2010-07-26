-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Maps.Tag_Map is

   function New_Item (Tag : Ada.Tags.Tag) return Item_Type
   is
      S : constant String := Ada.Tags.External_Tag (Tag);
      I : Item_Type;
   begin
      if S'Length > Max_Tag_Length then
         raise Overflow_Error;
      end if;
      I.Valid               := True;
      I.Str (1 .. S'Length) := S;
      I.Len                 := S'Length;
      return I;
   end New_Item;


   function "<" (I, J : Item_Type) return Boolean is
   begin
      return I.Str (1 .. I.Len) < J.Str (1 .. J.Len);
   end "<";


   function "=" (I, J : Item_Type) return Boolean is
   begin
      return I.Str (1 .. I.Len) = J.Str (1 .. J.Len);
   end "=";


   function "=" (I : Item_Type; S : String) return Boolean is
   begin
      return I.Str (1 .. I.Len) = S;
   end "=";


   procedure Register_Tag (Tag : in Ada.Tags.Tag)
   is
      pragma Precondition ((Head = null) = (Tail = null));
      Node : constant Node_Ref_Type := new Node_Type'(Item => New_Item (Tag),
                                                      Prev => Tail,
                                                      Next => null);
   begin
      if Sealed then
         raise Tag_Error;
      end if;
      if Head = null then
         Head := Node;
         Tail := Node;
      else
         Tail.Next := Node;
         Tail      := Node;
      end if;
   end Register_Tag;


   procedure Free_All
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Type, Node_Ref_Type);
      Cur : Node_Ref_Type := Head;
   begin
      while Cur /= null loop
         declare
            Next : constant Node_Ref_Type := Cur.Next;
         begin
            Head := Next;
            if Tail = Cur then
               Tail := Next;
            end if;
            Free (Cur);
            Cur := Next;
         end;
      end loop;
      Head := null;
      Tail := null;
   end Free_All;


   procedure Seal
   is
      pragma Precondition (Head = null or else Head.Prev = null);
      pragma Precondition (Tail = null or else Tail.Next = null);

      procedure Sort -- insertion sort (O(n^2))
      is
         procedure Swap_Payload (M, N : in Node_Ref_Type) is
         begin
            if M /= N then
               declare
                  Tmp : constant Item_Type := M.Item;
               begin
                  M.Item := N.Item;
                  N.Item := Tmp;
               end;
            end if;
         end Swap_Payload;

         Cur : Node_Ref_Type := Head;

         function Next_Min return Node_Ref_Type
         is
            Min  : Node_Ref_Type := Cur;
            Cand : Node_Ref_Type := Cur.Next;
         begin
            while Cand /= null loop
               if Cand.Item < Min.Item then
                  Min := Cand;
               end if;
               Cand := Cand.Next;
            end loop;
            return Min;
         end Next_Min;

      begin
         while Cur /= null loop
            Swap_Payload (Cur, Next_Min);
            Cur := Cur.Next;
         end loop;
      end Sort;
   begin
      if Sealed then
         raise Tag_Error;
      end if;

      Sort;

      declare
         Cur : Node_Ref_Type := Head;
         TID : TID_Type      := Map'First;
         Len : Natural       := 0;
      begin
         while Cur /= null loop
            if Len >= Integer (Map'Last) - Integer (Map'First) + 1 then
               -- Map is full.
               raise Overflow_Error;
            end if;
            if Cur.Prev = null or else Cur.Prev.Item /= Cur.Item then
               -- Don't store duplicates.
               Map (TID) := Cur.Item;
               TID := TID + 1;
               Len := Len + 1;
            end if;
            Cur := Cur.Next;
         end loop;

         for T in TID .. Map'Last loop
            Map (T).Valid := False;
         end loop;
      end;

      Sealed := True;

      Free_All;
   exception
      when Tag_Error | Overflow_Error =>
         Free_All;
         raise;
      when others =>
         raise;
   end Seal;


   function To_TID (Tag : Ada.Tags.Tag) return TID_Type is
      S : constant String := Ada.Tags.External_Tag (Tag);
   begin
      if not Sealed then
         raise Tag_Error;
      end if;
      for TID in Map'Range loop
         exit when not Map (TID).Valid;
         if Map (TID) = S then
            return TID;
         end if;
      end loop;
      raise Tag_Error;
   end To_TID;


   function To_Tag (TID : TID_Type) return Ada.Tags.Tag is
   begin
      if not Sealed then
         raise Tag_Error;
      end if;
      pragma Warnings (Off); -- TID not in Map'Range could be optimized away
      if TID not in Map'Range or else not Map (TID).Valid then
         raise Tag_Error;
      end if;
      pragma Warnings (Off);
      declare
         S : constant String := Map (TID).Str (1 .. Map (TID).Len);
      begin
         return Ada.Tags.Internal_Tag (S);
      end;
   end To_Tag;

end DB.Maps.Tag_Map;

