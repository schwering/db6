-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.DSA.Utils.Gen_Small_LRU_Caches is

   procedure Move_To_Top
     (Cache : in out Cache_Type;
      Pred  : in     Node_Ref_Type;
      Node  : in     Node_Ref_Type) is
   begin
      if Pred /= null then
         Pred.Next := Node.Next;
         if Cache.Tail = Node then
            Cache.Tail := Pred;
         end if;
      end if;
      if Cache.Head /= Node then
         Node.Next := Cache.Head;
         Cache.Head := Node;
      end if;
   end Move_To_Top;


   procedure Put
     (Cache : in out Cache_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type)
   is
      pragma Precondition ((Cache.Head = null) = (Cache.Tail = null));
      pragma Precondition ((Cache.Head = null) = (Cache.Size = 0));
      pragma Precondition ((Cache.Head = Cache.Tail) = (Cache.Size = 0) or
                           Cache.Head /= null);
      pragma Precondition ((Cache.Head = Cache.Tail) = (Cache.Size = 1) or
                           Cache.Head = null);
      H : constant Hash_Type := Hash (Key);
   begin
      if Cache.Head = null then
         Cache.Head := new Node_Type'(Key, Value, H, null);
         Cache.Tail := Cache.Head;
         Cache.Size := 1;
      else
         declare
            Pred : Node_Ref_Type := null;
            Node : Node_Ref_Type := Cache.Head;
         begin
            while Node /= null loop
               if Node.Hash = H and then Node.Key = Key then
                  Move_To_Top (Cache, Pred, Node);
                  return;
               end if;
               -- to preserve Pred if Tail is replaced, we exit early
               exit when Node.Next = null;
               Pred := Node;
               Node := Node.Next;
            end loop;

            if Cache.Size < Slot_Count then
               Pred            := Cache.Tail;
               Cache.Tail.Next := new Node_Type'(Key, Value, H, null);
               Cache.Tail      := Cache.Tail.Next;
               Cache.Size      := Cache.Size + 1;
            else
               Cache.Tail.all := Node_Type'(Key, Value, H, null);
            end if;
            Move_To_Top (Cache, Pred, Cache.Tail);
         end;
      end if;
   end Put;


   procedure Get
     (Cache : in out Cache_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type;
      Found :    out Boolean)
   is
      pragma Precondition ((Cache.Head = null) = (Cache.Tail = null));
      pragma Precondition ((Cache.Head = null) = (Cache.Size = 0));
      pragma Precondition ((Cache.Head = Cache.Tail) = (Cache.Size <= 1));
      H    : constant Hash_Type := Hash (Key);
      Pred : Node_Ref_Type := null;
      Node : Node_Ref_Type := Cache.Head;
   begin
      while Node /= null loop
         if Node.Hash = H and then Node.Key = Key then
            Move_To_Top (Cache, Pred, Node);
            Value := Node.Value;
            Found := True;
            return;
         end if;
         Pred := Node;
         Node := Node.Next;
      end loop;
      Found := False;
   end Get;


   procedure Initialize (Cache : in out Cache_Type) is
   begin
      Cache.Head := null;
      Cache.Tail := null;
      Cache.Size := 0;
   end Initialize;


   procedure Finalize (Cache : in out Cache_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Type, Node_Ref_Type);
      Node : Node_Ref_Type := Cache.Head;
   begin
      while Node /= null loop
         declare
            Next : constant Node_Ref_Type := Node.Next;
         begin
            Free (Node);
            Node := Next;
         end;
      end loop;
      Cache.Head := null;
      Cache.Tail := null;
      Cache.Size := 0;
   end Finalize;

end DB.DSA.Utils.Gen_Small_LRU_Caches;

