-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Gen_BTrees is

   package Initialization is
      procedure Create
        (ID : in  String);

      procedure Initialize
        (Tree : out Tree_Type;
         ID   : in  String);

      procedure Finalize
        (Tree : in out Tree_Type);
   end Initialization;


   package Retrieval is
      procedure Search
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);

      procedure Minimum
        (Tree     : in out Tree_Type;
         Key      :    out Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);
   end Retrieval;


   package Cursors is
      function Positive_Infinity_Bound
         return Bound_Type;

      function Negative_Infinity_Bound
         return Bound_Type;

      function New_Bound
        (Comparison : Comparison_Type;
         Key        : Key_Type)
         return Bound_Type;

      function New_Cursor
        (Tree        : Tree_Type;
         Thread_Safe : Boolean;
         Lower_Bound : Bound_Type;
         Upper_Bound : Bound_Type)
         return Cursor_Type;

      procedure Set_Thread_Safety
        (Cursor  : in out Cursor_Type;
         Enabled : in     Boolean);

      procedure Finalize_Cursor
        (Tree   : in     Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Pause
        (Tree   : in     Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Next
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         Key    :    out Key_Type;
         Value  :    out Value_Type;
         State  :    out State_Type);

      procedure Delete
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         Key    :    out Key_Type;
         Value  :    out Value_Type;
         State  :    out State_Type);
   end Cursors;


   package Misc is
      procedure Count
        (Tree  : in out Tree_Type;
         Count :    out Count_Type);

      procedure Reorganize
        (Tree  : in out Tree_Type;
         State :    out State_Type);
   end Misc;


--   function Image
--     (N_A : Nodes.Valid_Address_Type)
--      return String is
--   begin
--      return Block_IO.Image(Block_IO.Valid_Address_Type(N_A));
--   end Image;


--   function Image
--     (N_A : Nodes.Address_Type)
--      return String is
--   begin
--      if Nodes.Is_Valid(N_A) then
--         return Image(Nodes.To_Valid_Address(N_A));
--      else
--         return "Invalid_Address";
--      end if;
--   end Image;


   function "<"
     (Left, Right : Key_Type)
      return Boolean
   is
      use type Utils.Comparison_Result_Type;
      C : constant Utils.Comparison_Result_Type := Compare(Left, Right);
   begin
      return C = Utils.Less;
   end "<";


   function "<="
     (Left, Right : Key_Type)
      return Boolean
   is
      use type Utils.Comparison_Result_Type;
      C : constant Utils.Comparison_Result_Type := Compare(Left, Right);
   begin
      return C = Utils.Less or C = Utils.Equal;
   end "<=";


   function "="
     (Left, Right : Key_Type)
      return Boolean
   is
      use type Utils.Comparison_Result_Type;
      C : constant Utils.Comparison_Result_Type := Compare(Left, Right);
   begin
      return C = Utils.Equal;
   end "=";


   procedure Read_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type)
   is
      pragma Inline (Read_Node);

      function Same_Level return Boolean
      is
         use type Nodes.Level_Type;
         R : Nodes.RO_Node_Type;
      begin
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            return True;
         end if;
         Read_Node(Tree, Nodes.Valid_Link(N), R);
         return Nodes.Level(N) = Nodes.Level(R);
      end Same_Level;
   begin
      if not Same_Level then
         raise Tree_Error;
      end if;
      Block_IO.Read(Tree.File, Block_IO.Valid_Address_Type(N_A),
                    Blocks.Base_Block_Type(N(Nodes.RO_Node_Type'Range)));
   end Read_Node;


   procedure Write_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type)
   is
      pragma Inline (Write_Node);
      use type Nodes.Valid_Address_Type;
      pragma Assert (Nodes.Is_Safe(N, Is_Root => N_A = Root_Address));

      function Same_Level return Boolean
      is
         use type Nodes.Level_Type;
         R : Nodes.RO_Node_Type;
      begin
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            return True;
         end if;
         Read_Node(Tree, Nodes.Valid_Link(N), R);
         return Nodes.Level(N) = Nodes.Level(R);
      end Same_Level;
   begin
      if not Same_Level then
         raise Tree_Error;
      end if;
      Block_IO.Write(Tree.File, Block_IO.Valid_Address_Type(N_A),
                     Nodes.To_Block(N));
   end Write_Node;


   procedure Write_New_Node
     (Tree : in out Tree_Type;
      N_A  :    out Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type)
   is
      pragma Inline (Write_New_Node);
      pragma Assert (Nodes.Is_Safe(N, Is_Root => False));

      function Same_Level return Boolean
      is
         use type Nodes.Level_Type;
         R : Nodes.RO_Node_Type;
      begin
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            return True;
         end if;
         Read_Node(Tree, Nodes.Valid_Link(N), R);
         return Nodes.Level(N) = Nodes.Level(R);
      end Same_Level;
   begin
      if not Same_Level then
         raise Tree_Error;
      end if;
      Block_IO.Write_New_Block(Tree.File, Block_IO.Valid_Address_Type(N_A),
                               Nodes.To_Block(N));
   end Write_New_Node;


   procedure Lock
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type) is
   begin
      Block_IO.Lock(Tree.File, Block_IO.Valid_Address_Type(N_A));
   end Lock;


   procedure Unlock
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type) is
   begin
      Block_IO.Unlock(Tree.File, Block_IO.Valid_Address_Type(N_A));
   end Unlock;


   -- Returns the pointer to the child that induces the tree that contains Key
   -- if there is such a child. Otherwise, if the link to the right neighbor is
   -- valid, it returns this link, because this pointers to the next subtree
   -- candidate that might contain Key. If the link is invalid, then the node is
   -- the outermost right one of its level and therefore the address of the
   -- greatest (i.e. outermost right) subtree is returned.
   -- Note: Only defined for inner nodes!
   function Scan_Node
     (N   : Nodes.Node_Type;
      Key : Key_Type)
      return Nodes.Valid_Address_Type
   is
      pragma Assert (Nodes.Is_Inner(N));
      I : constant Nodes.Index_Type := Nodes.Key_Position(N, Key);
   begin
      if Nodes.Is_Valid(I) then
         return Nodes.Child(N, I);
      elsif Nodes.Is_Valid(Nodes.Link(N)) then
         return Nodes.Valid_Link(N);
      else
         return Nodes.Child(N, Nodes.Degree(N));
      end if;
   end Scan_Node;


   -- Moves to the right starting from the node at address N_A until a node
   -- which satisfies the Exit_Cond is reached.
   -- During all this time, it cares about the lock by locking the current node
   -- and unlocking it right after the right neighbor is locked.
   -- Hence on return, the node N at address N_A is locked!
   -- The maximum count of concurrently held locks is 2.
   procedure Move_Right
     (Tree      : in out          Tree_Type;
      Exit_Cond : not null access function (N : Nodes.Node_Type) return Boolean;
      N_A       : in out          Nodes.Valid_Address_Type;
      N         :    out          Nodes.Node_Type) is
   begin
      Lock(Tree, N_A);
      loop
         declare
            use type Nodes.Valid_Address_Type;
         begin
            Read_Node(Tree, N_A, N);
            exit when Exit_Cond(N);
            if not Nodes.Is_Valid(Nodes.Link(N)) then
               raise Tree_Error;
            end if;
         exception
            when others =>
               Unlock(Tree, N_A);
               raise;
         end;
         declare
            R_A : constant Nodes.Valid_Address_Type := Nodes.Valid_Link(N);
         begin
            Lock(Tree, R_A);
            Unlock(Tree, N_A);
            N_A := R_A;
         end;
      end loop;
   end Move_Right;


   procedure Create
     (ID : in String)
   renames Initialization.Create;


   procedure Initialize
     (Tree : out Tree_Type;
      ID   : in  String)
   renames Initialization.Initialize;


   procedure Finalize
     (Tree : in out Tree_Type)
   renames Initialization.Finalize;


   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type :=
        Blocks.Bits_To_Units(Value_Type'Size))
      return Blocks.Size_Type
   renames Nodes.Max_Key_Size;


   procedure Search
     (Tree  : in out Tree_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type;
      State :    out State_Type)
   renames Retrieval.Search;


   procedure Minimum
     (Tree  : in out Tree_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type;
      State :    out State_Type)
   renames Retrieval.Minimum;


   procedure Insert
     (Tree  : in out Tree_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type;
      State :    out State_Type)
   is separate;


   procedure Delete
     (Tree  : in out Tree_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type;
      State :    out State_Type)
   is separate;


   function Positive_Infinity_Bound
      return Bound_Type
   renames Cursors.Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type
   renames Cursors.Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type
   renames Cursors.New_Bound;


   function New_Cursor
     (Tree        : Tree_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type
   renames Cursors.New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   renames Cursors.Set_Thread_Safety;


   procedure Finalize_Cursor
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   renames Cursors.Finalize_Cursor;


   procedure Pause
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   renames Cursors.Pause;


   procedure Next
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type;
      State  :    out State_Type)
   renames Cursors.Next;


   procedure Delete
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type;
      State  :    out State_Type)
   renames Cursors.Delete;


   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   renames Misc.Count;

   procedure Reorganize
     (Tree  : in out Tree_Type;
      State :    out State_Type)
   renames Misc.Reorganize;


   package body Nodes is separate;
   package body Initialization is separate;
   package body Retrieval is separate;
   package body Cursors is separate;
   package body Misc is separate;

end DB.Gen_BTrees;

