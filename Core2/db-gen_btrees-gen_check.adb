-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Text_IO;

with DB.Gen_BTrees.Gen_Draw;
with DB.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;

procedure DB.Gen_BTrees.Gen_Check
  (Tree : in out Tree_Type)
is
   use Ada.Text_IO;
   use Nodes;

   function Image (N_A : Valid_Address_Type) return String is
   begin
      return Block_IO.Image(Block_IO.Valid_Address_Type(N_A));
   end Image;

   function Image (N_A : Address_Type) return String is
   begin
      if Is_Valid(N_A) then
         return Image(To_Valid_Address(N_A));
      else
         return "invalid";
      end if;
   end Image;

   package Stacks is new Utils.Gen_Stacks
     (Item_Type    => Valid_Address_Type,
      Initial_Size => 7,
      Storage_Pool => Utils.Global_Pool.Global'Storage_Pool);


   -- The stack contains the outermost left nodes of each level.
   Stack : Stacks.Stack_Type := Stacks.New_Stack;
   procedure Init_Stack
   is
      N_A : Valid_Address_Type := Root_Address;
      N   : RO_Node_Type;
   begin
      loop
         Stacks.Push(Stack, N_A);
         Read_Node(Tree, N_A, N);
         exit when Is_Leaf(N);
         N_A := Child(N, 1);
      end loop;
   end Init_Stack;

   procedure Check_Local_Order (N : in Node_Type; N_A : in Valid_Address_Type) is
   begin
      for I in 2 .. Degree(N) loop
         if not (Key(N, I-1) <= Key(N, I)) then
            Put_Line("Wrong order in"&
                     Address_To_String(Block_IO.Address_Type(
                        To_Address(N_A))) &" "&
                     Boolean'Image(Is_Leaf(N)));
            raise Tree_Error;
         end if;
      end loop;
   end Check_Local_Order;

   procedure Check_Link_Order (N : in Node_Type; N_A : in Valid_Address_Type)
   is
      L : RO_Node_Type;
   begin
      Read_Node(Tree, Valid_Link(N), L);
      if Degree(L) = 0 then
         return;
      end if;
      if not (Key(N, Degree(N)) <= Key(L, 1)) then
         Put_Line("Wrong link order in"&
                  Address_To_String(Block_IO.Address_Type(
                     To_Address(N_A))) &" "&
                  Address_To_String(Block_IO.Address_Type(Link(N))) &" "&
                  Boolean'Image(Is_Leaf(N)));
         raise Tree_Error;
      end if;
   end Check_Link_Order;

   procedure Check_Child_Order (N : in Node_Type)
   is
      C : RO_Node_Type;
   begin
      Read_Node(Tree, Child(N, Degree(N)), C);
      if Degree(C) = 0 then
         return;
      end if;
      if not (Key(C, Degree(C)) <= Key(N, Degree(N))) then
         raise Tree_Error;
      end if;
   end Check_Child_Order;

   procedure Check_LinkChild_ChildLink_Equality (N : in Node_Type)
   is
      LC_A : Valid_Address_Type;
      CL_A : Valid_Address_Type;
   begin
      declare
         L : RO_Node_Type;
      begin
         Read_Node(Tree, Valid_Link(N), L);
         LC_A := Child(L, 1);
      end;
      declare
         C : RO_Node_Type;
      begin
         Read_Node(Tree, Child(N, Degree(N)), C);
         CL_A := Valid_Link(C);
      end;
      if LC_A /= CL_A then
         raise Tree_Error;
      end if;
   end Check_LinkChild_ChildLink_Equality;

   procedure Draw is new Gen_Draw(Address_To_String);

   N_A : Valid_Address_Type;
   N   : RO_Node_Type;
begin
   Init_Stack;
   --Draw(Tree);
   loop
      exit when Stacks.Is_Empty(Stack);
      Stacks.Pop(Stack, N_A);
      loop
         Read_Node(Tree, N_A, N);
         Check_Local_Order(N, N_A);
         if Is_Inner(N) then
            Check_Child_Order(N);
            if Is_Valid(Link(N)) then
               Check_Link_Order(N, N_A);
               Check_LinkChild_ChildLink_Equality(N);
            end if;
         end if;
         exit when not Is_Valid(Link(N));
         N_A := Nodes.Valid_Link(N);
      end loop;
   end loop;
end DB.Gen_BTrees.Gen_Check;

