-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Linked_Lists is

   procedure Free is new Ada.Unchecked_Deallocation(Node_Type, Node_Ref_Type);


   function Is_Empty
     (List : List_Type)
      return Boolean is
   begin
      pragma Assert ((List.Head = null) = (List.Tail = null));
      return List.Head = null;
   end Is_Empty;


   function Size
     (List : List_Type)
      return Natural is
   begin
      return List.Size;
   end Size;


   procedure Deep_Copy
     (List : in     List_Type;
      Head :    out Node_Ref_Type;
      Tail :    out Node_Ref_Type) is
   begin
      if List.Head = null then
         Head := null;
         Tail := null;
      else
         declare
            function Copy (N : Node_Ref_Type) return Node_Ref_Type is
            begin
               return new Node_Type'(Item => N.Item,
                                     Prev => N.Prev,
                                     Next => N.Next);
            end Copy;

            Target_Head : constant Node_Ref_Type := Copy(List.Head);
            Target_Node : Node_Ref_Type := Target_Head;
         begin
            while Target_Node.Next /= null loop
               Target_Node.Next := Copy(Target_Node.Next);
               Target_Node      := Target_Node.Next;
            end loop;
            Head := Target_Head;
            Tail := Target_Node;
         end;
      end if;
   end Deep_Copy;


   overriding
   procedure Finalize
     (List : in out List_Type)
   is
      Node : Node_Ref_Type renames List.Head;
   begin
      while Node /= null loop
         Node := Node.Next;
         Free(Node);
      end loop;
   end Finalize;


   function Deep_Copy
     (List : List_Type)
      return List_Type
   is
      Head : Node_Ref_Type;
      Tail : Node_Ref_Type;
   begin
      Deep_Copy(List, Head, Tail);
      return (Ada.Finalization.Limited_Controlled with
              Head   => Head,
              Tail   => Tail,
              others => <>);
   end Deep_Copy;


   procedure Prepend
     (List : in out List_Type;
      Item : in     Item_Type) is
   begin
      List.MCtr := List.MCtr + 1;
      List.Size := List.Size + 1;
      if List.Head = null then
         List.Head := new Node_Type'(Item => Item,
                                     Prev => null,
                                     Next => null);
         List.Tail := List.Head;
      else
         pragma Assert (List.Tail /= null);
         List.Head := new Node_Type'(Item => Item,
                                     Prev => null,
                                     Next => List.Head);
      end if;
   end Prepend;


   procedure Append
     (List : in out List_Type;
      Item : in     Item_Type) is
   begin
      List.MCtr := List.MCtr + 1;
      List.Size := List.Size + 1;
      if List.Head = null then
         List.Head := new Node_Type'(Item => Item,
                                     Prev => null,
                                     Next => null);
         List.Tail := List.Head;
      else
         pragma Assert (List.Tail /= null);
         List.Tail.Next := new Node_Type'(Item => Item,
                                          Prev => List.Tail,
                                          Next => null);
         List.Tail      := List.Tail.Next;
      end if;
   end Append;


   procedure Pop_Head
     (List  : in out List_Type;
      Item  :    out Item_Type;
      Found :    out Boolean) is
   begin
      List.MCtr := List.MCtr + 1;
      if List.Head /= null then
         declare
            Node : Node_Ref_Type := List.Head;
         begin
            Item           := List.Head.Item;
            List.Head      := List.Head.Next;
            List.Head.Prev := null;
            List.Size      := List.Size - 1;
            Free(Node);
            Found := True;
         end;
      else
         Found := False;
      end if;
   end Pop_Head;


   procedure Pop_Tail
     (List  : in out List_Type;
      Item  :    out Item_Type;
      Found :    out Boolean) is
   begin
      List.MCtr := List.MCtr + 1;
      if List.Tail /= null then
         declare
            Node : Node_Ref_Type := List.Tail;
         begin
            Item           := List.Tail.Item;
            List.Tail      := List.Tail.Prev;
            List.Tail.Next := null;
            List.Size      := List.Size - 1;
            Free(Node);
            Found := True;
         end;
      else
         Found := False;
      end if;
   end Pop_Tail;


   procedure Search
     (List    : in     List_Type;
      Matches : access function (Item : Item_Type) return Boolean;
      Item    :    out Item_Type;
      Found   :    out Boolean)
   is
      Node : Node_Ref_Type := List.Head;
   begin
      while Node /= null loop
         if Matches(Node.Item) then
            Item  := Node.Item;
            Found := True;
         end if;
         Node := Node.Next;
      end loop;
      Found := False;
   end Search;


   procedure For_Each
     (List  : in List_Type;
      Visit : not null access procedure (Item : in out Item_Type))
   is
      Iter : Iterator_Type := Iterator(List);
   begin
      while Has_Next(Iter) loop
         declare
            Item : Item_Type;
         begin
            Get_Next(Iter, Item);
            Visit(Item);
         end;
      end loop;
   end For_Each;


   function Iterator
     (List : List_Type)
      return Iterator_Type is
   begin
      return Iterator_Type'(List => List.Self, MCtr => List.MCtr, others => <>);
   end Iterator;


   function Has_Next
     (Iterator : Iterator_Type)
      return Boolean is
   begin
      if Iterator.MCtr /= Iterator.List.MCtr then
         raise List_Error;
      end if;
      return Iterator.Curr /= Iterator.List.Tail;
   end Has_Next;


   procedure Get_Next
     (Iterator : in out Iterator_Type;
      Item     :    out Item_Type) is
   begin
      pragma Assert (Has_Next(Iterator));
      if Iterator.MCtr /= Iterator.List.MCtr then
         raise List_Error;
      end if;
      if Iterator.Curr = null then
         Iterator.Curr := Iterator.List.Head;
      else
         Iterator.Curr := Iterator.Curr.Next;
      end if;
      Iterator.Deleted := False;
      Item             := Iterator.Curr.Item;
   end Get_Next;


   procedure Delete
     (Iterator : in out Iterator_Type)
   is
      List : List_Type renames Iterator.List.all;
      Node : Node_Ref_Type := Iterator.Curr;
   begin
      if Iterator.Curr = null then
         raise List_Error;
      end if;
      if Iterator.MCtr /= Iterator.List.MCtr then
         raise List_Error;
      end if;
      if Iterator.Deleted then
         raise List_Error;
      end if;
      if List.Head = Node then
         List.Head := Node.Next;
      end if;
      if List.Tail = Node then
         List.Tail := Node.Prev;
      end if;
      if Node.Prev /= null then
         Node.Prev.Next := Node.Next;
      end if;
      if Node.Next /= null then
         Node.Next.Prev := Node.Prev;
      end if;
      Iterator.List.Size := Iterator.List.Size - 1;
      Iterator.Deleted   := True;
      Iterator.Curr      := Iterator.Curr.Prev;
      Free(Node);
   end Delete;

end DB.Utils.Gen_Linked_Lists;

