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
            raise Tree_Error with "wrong node-internal order";
         end if;
      end loop;
   end Check_Local_Order;

   procedure Check_Link_Order (N : in Node_Type; N_A : in Valid_Address_Type)
   is
      L : RO_Node_Type;
   begin
      Read_Node(Tree, Valid_Link(N), L);
      if Level(N) /= Level(L) then
         raise Tree_Error with "node and link have inequal level";
      end if;
      if Degree(L) = 0 then
         return;
      end if;
      if not (Key(N, Degree(N)) <= Key(L, 1)) then
         Put_Line("Wrong link order in"&
                  Address_To_String(Block_IO.Address_Type(
                     To_Address(N_A))) &" "&
                  Address_To_String(Block_IO.Address_Type(Link(N))) &" "&
                  Boolean'Image(Is_Leaf(N)));
         raise Tree_Error with "link's keys are less than node's";
      end if;
   end Check_Link_Order;

   procedure Check_High_Key_Order (N : in Node_Type; N_A : in Valid_Address_Type)
   is
      L : RO_Node_Type;
      NHK : Key_Type;
      LHK : Key_Type;
      Has_NHK : Boolean;
      Has_LHK : Boolean;
   begin
      Read_Node(Tree, Valid_Link(N), L);
      Get_High_Key(N, NHK, Has_NHK);
      Get_High_Key(L, LHK, Has_LHK);
      if Has_NHK /= Has_LHK then
         Put_Line("Difference about existence of high keys: "&
                  Has_NHK'Img &" "& Has_LHK'Img);
         raise Tree_Error with "node and link differ in having a high key";
      end if;
      if not Has_NHK then
         return;
      end if;
      if not (NHK <= LHK) then
         Put_Line("Wrong high key order in"&
                  Address_To_String(Block_IO.Address_Type(
                     To_Address(N_A))) &" "&
                  Address_To_String(Block_IO.Address_Type(Link(N))) &" "&
                  Boolean'Image(Is_Leaf(N)));
         raise Tree_Error with "link's high key is less than node's";
      end if;
   end Check_High_Key_Order;

   procedure Check_Child_Order (N : in Node_Type)
   is
      C : RO_Node_Type;
   begin
      Read_Node(Tree, Child(N, Degree(N)), C);
      if Level(N) - 1 /= Level(C) then
         raise Tree_Error with "child's level is not one less than node's";
      end if;
      if Degree(C) = 0 then
         return;
      end if;
      declare
         NHK  : Key_Type;
         NHHK : Boolean;
         CHK  : Key_Type;
         CHHK : Boolean;
      begin
         Get_High_Key(N, NHK, NHHK);
         Get_High_Key(C, CHK, CHHK);
         if not NHHK then
            raise Tree_Error with "node has no high key";
         end if;
         if not CHHK then
            raise Tree_Error with "child has no high key";
         end if;
         if not (CHK <= NHK) then
            raise Tree_Error with "child's high key is greater than node's";
         end if;
      end;
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
         raise Tree_Error with "parallelogram equality not satisfied";
      end if;
   end Check_LinkChild_ChildLink_Equality;

   procedure Check_Leaf_Level (N : in Node_Type) is
   begin
      if Level(N) /= 0 then
         raise Tree_Error with "leaf has not level zero";
      end if;
   end Check_Leaf_Level;

   procedure Check_Keys (N : in Node_Type)
   is
      Key_Context : Key_Context_Type := New_Key_Context;
      Key         : Key_Type;
   begin
      for I in 1 .. Degree(N) loop
         Get_Key(N, I, Key, Key_Context);
      end loop;
   end Check_Keys;

   procedure Check_Children (N : in Node_Type)
   is
      Key_Context : Key_Context_Type := New_Key_Context;
      Child       : Valid_Address_Type;
   begin
      for I in 1 .. Degree(N) loop
         Get_Child(N, I, Child, Key_Context);
      end loop;
   end Check_Children;

   procedure Check_Values (N : in Node_Type)
   is
      Key_Context   : Key_Context_Type := New_Key_Context;
      Value_Context : Value_Context_Type := New_Value_Context;
      Value         : Value_Type;
   begin
      for I in 1 .. Degree(N) loop
         Get_Value(N, I, Value, Key_Context, Value_Context);
      end loop;
   end Check_Values;

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
         case Is_Inner(N) is
            when True =>
               Check_Child_Order(N);
               if Is_Valid(Link(N)) then
                  Check_Link_Order(N, N_A);
                  Check_LinkChild_ChildLink_Equality(N);
               end if;
               Check_Keys(N);
               Check_Children(N);
            when False =>
               Check_Leaf_Level(N);
               Check_Keys(N);
               Check_Values(N);
         end case;
         if Is_Valid(Link(N)) then
            Check_High_Key_Order(N, N_A);
         end if;
         exit when not Is_Valid(Link(N));
         N_A := Valid_Link(N);
      end loop;
   end loop;
end DB.Gen_BTrees.Gen_Check;

