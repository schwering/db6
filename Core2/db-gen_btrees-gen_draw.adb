-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

with DB.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;

procedure DB.Gen_BTrees.Gen_Draw
  (Tree : in out Tree_Type)
is
   use Ada.Text_IO;
   use Nodes;

   package Sets is new Ada.Containers.Ordered_Sets
     (Valid_Address_Type, "<", "=");

   Set : Sets.Set;

   function Image (N_A : Valid_Address_Type) return String
   renames Block_IO.Image;

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
      Storage_Pool => Utils.Global_Pool.Global_Storage_Pool);


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

   procedure Draw_Node (N_A : Valid_Address_Type; N : Node_Type)
   is
      function Style return String is
      begin
         if Is_Leaf(N) then
            return "doublecircle";
         else
            return "circle";
         end if;
      end Style;
      Deg  : constant String := Degree_Type'Image(Degree(N));
      Addr : constant String := Image(To_Address(N_A));
      Lev  : constant String := Level_Type'Image(Nodes.Level(N));
   begin
      Put(""""& Addr &"""");
      Put(" [label="""& Addr &": "& Deg &", "& Lev &""",shape="""& Style &"""]");
      New_Line;
   end Draw_Node;

   procedure Draw_Arrow (From, To : Valid_Address_Type; Is_Link : Boolean) is
   begin
      Put(""""& Image(To_Address(From)) &"""");
      Put(" -> ");
      Put(""""& Image(To_Address(To)) &"""");
      if Is_Link then
         Put(" [arrowhead=diamond]");
      end if;
      New_Line;
   end Draw_Arrow;

   procedure Handle_Node (N_A : Nodes.Valid_Address_Type; N : out Node_Type) is
   begin
      if Sets.Contains(Set, N_A) then
         return;
      end if;
      Sets.Insert(Set, N_A);
      Read_Node(Tree, N_A, N);
      Draw_Node(N_A, N);
      if Is_Valid(Link(N)) then
         Draw_Arrow(N_A, Valid_Link(N), Is_Link => True);
      end if;
      if Is_Inner(N) then
         for I in 1 .. Degree(N) loop
            Draw_Arrow(N_A, Child(N, I), Is_Link => False);
            declare
               C : RO_Node_Type;
            begin
               Handle_Node(Child(N, I), C);
            end;
         end loop;
      end if;
   end Handle_Node;

   N_A : Valid_Address_Type;
   N   : RO_Node_Type;
begin
   Init_Stack;
   Put_Line("digraph {");
   --Put_Line("   node [shape=circle]");
   loop
      exit when Stacks.Is_Empty(Stack);
      Stacks.Pop(Stack, N_A);
      loop
         Handle_Node(N_A, N);
         exit when not Is_Valid(Link(N));
         N_A := Valid_Link(N);
      end loop;
   end loop;
   Put_Line("}");
end DB.Gen_BTrees.Gen_Draw;

