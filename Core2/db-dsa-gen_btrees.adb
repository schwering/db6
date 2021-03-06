-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;

package body DB.DSA.Gen_BTrees is

   package Stacks is
      generic
         with function Exit_Cond
           (N : Nodes.Node_Type)
            return Boolean;

         with procedure Modify_Node
           (N_Old : in  Nodes.Node_Type;
            N     : out Nodes.RW_Node_Type;
            State : out State_Type);
      procedure Gen_Modify
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         State :    out State_Type);
      -- The generic operation which can be used to modify leaf nodes.
      -- Internally it looks for the leaf that contains Key. This is done using
      -- (eventually) Gen_Move_Right which is instantiated with Exit_Cond.
      -- Exit_Cond is a crucial component, because it controls which leaf is
      -- actually modified.
      -- Then Modify_Node is called and the old leaf, N_Old, is replaced with
      -- the new leaf, N. Subsequently, the node is written back and, if
      -- necessary, the stack is unwound, i.e. backtracking is done. This is
      -- required to keep the high-keys of the nodes consistent.
      --
      -- This procedure cares about race conditions and so on. Since the stack
      -- of nodes which represents the path from the root node to the leaf plays
      -- an important role, the package is named Stacks.
      -- Also see the documentation of Stacks, Insertions and Deletions.
   end Stacks;


   package Initialization is
      procedure Create
        (Tree : in out Tree_Type;
         ID   : in     String);

      procedure Create_Temporary
        (Tree : in out Tree_Type;
         ID   : in     String);

      procedure Open
        (Tree : in out Tree_Type;
         ID   : in     String);

      procedure Finalize
        (Tree : in out Tree_Type);
   end Initialization;


   package Searches is
      procedure Search_Node
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         N     :    out Nodes.Node_Type;
         Index :    out Nodes.Extended_Index_Type;
         State :    out State_Type);

      procedure Search
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Value :    out Values.Value_Type;
         State :    out State_Type);

      procedure Ceiling
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Ceil  :    out Keys.Key_Type;
         Value :    out Values.Value_Type;
         State :    out State_Type);

      procedure Search_Minimum_Node
        (Tree  : in out Tree_Type;
         N     :    out Nodes.Node_Type;
         Index :    out Nodes.Index_Type;
         State :    out State_Type);

      procedure Search_Minimum
        (Tree  : in out Tree_Type;
         Key   :    out Keys.Key_Type;
         Value :    out Values.Value_Type;
         State :    out State_Type);
   end Searches;


   package Insertions is
      procedure Insert
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Value : in     Values.Value_Type;
         State :    out State_Type);

      procedure Insert
        (Tree      : in out Tree_Type;
         Key       : in     Keys.Key_Type;
         Value     : in     Values.Value_Type;
         Existed   :    out Boolean;
         Old_Value :    out Values.Value_Type;
         State     :    out State_Type);

      procedure Replace
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Value : in     Values.Value_Type;
         State :    out State_Type);

      procedure Replace
        (Tree      : in out Tree_Type;
         Key       : in     Keys.Key_Type;
         Value     : in     Values.Value_Type;
         Existed   :    out Boolean;
         Old_Value :    out Values.Value_Type;
         State     :    out State_Type);

      procedure Append
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Value : in     Values.Value_Type;
         State :    out State_Type);
   end Insertions;


   package Deletions is
      procedure Delete
        (Tree  : in out Tree_Type;
         Key   : in     Keys.Key_Type;
         Value :    out Values.Value_Type;
         State :    out State_Type);

      procedure Delete_Range
        (Tree  : in out Tree_Type;
         First : in     Keys.Key_Type;
         Last  : in     Keys.Key_Type;
         State :    out State_Type);
   end Deletions;


   package Cursors is
      function Positive_Infinity_Bound
         return Bound_Type;

      function Negative_Infinity_Bound
         return Bound_Type;

      function New_Bound
        (Comparison : Comparison_Type;
         Key        : Keys.Key_Type)
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

      procedure Pause
        (Tree   : in     Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Next
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         Key    :    out Keys.Key_Type;
         Value  :    out Values.Value_Type;
         State  :    out State_Type);

      procedure Delete
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         State  :    out State_Type);
   end Cursors;


   package Misc is
      procedure Count
        (Tree  : in out Tree_Type;
         Count :    out Count_Type);
   end Misc;


   procedure Read_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type)
   is
      pragma Inline (Read_Node);
   begin
      Block_IO.Read
         (File    => Tree.File,
          Address => Block_IO.Valid_Address_Type (N_A),
          Block   => Blocks.Base_Block_Type (N (Nodes.RO_Node_Type'Range)));
   end Read_Node;


   procedure Write_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type)
   is
      pragma Inline (Write_Node);
      use type Nodes.Valid_Address_Type;
      pragma Assert (Nodes.Is_Safe (N, Is_Root => N_A = Root_Address));
   begin
      Block_IO.Write
         (File           => Tree.File,
          Address        => Block_IO.Valid_Address_Type (N_A),
          Block          => Nodes.To_Block (N),
          Cache_Priority => Natural (Nodes.Level (N)));
   end Write_Node;


   procedure Write_New_Node
     (Tree : in out Tree_Type;
      N_A  :    out Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type)
   is
      pragma Inline (Write_New_Node);
      pragma Assert (Nodes.Is_Safe (N, Is_Root => False));
   begin
      Block_IO.Write_New_Block
         (File           => Tree.File,
          Address        => Block_IO.Valid_Address_Type (N_A),
          Block          => Nodes.To_Block (N),
          Cache_Priority => Natural (Nodes.Level (N)));
   end Write_New_Node;


   procedure Try_Lock
     (Tree    : in out Tree_Type;
      N_A     : in     Nodes.Valid_Address_Type;
      Timeout : in     Duration := 0.0;
      Success :    out Boolean) is
   begin
      Block_IO.Try_Lock (Tree.File, Block_IO.Valid_Address_Type (N_A),
                         Timeout, Success);
   end Try_Lock;


   procedure Lock
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type) is
   begin
      Block_IO.Lock (Tree.File, Block_IO.Valid_Address_Type (N_A));
   end Lock;


   procedure Unlock
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type) is
   begin
      Block_IO.Unlock (Tree.File, Block_IO.Valid_Address_Type (N_A));
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
      Key : Keys.Key_Type)
      return Nodes.Valid_Address_Type
   is
      pragma Assert (Nodes.Is_Inner (N));
      I : constant Nodes.Extended_Index_Type := Nodes.Key_Position (N, Key);
   begin
      if Nodes.Is_Valid (I) then
         return Nodes.Child (N, I);
      elsif Nodes.Is_Valid (Nodes.Link (N)) then
         return Nodes.Valid_Link (N);
      else
         return Nodes.Child (N, Nodes.Degree (N));
      end if;
   end Scan_Node;


   generic
      with function Exit_Cond (N : Nodes.Node_Type) return Boolean;
   procedure Gen_Move_Right
     (Tree : in out Tree_Type;
      N_A  : in out Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type);
   -- Moves to the right starting from the node at address N_A until a node
   -- which satisfies the Exit_Cond is reached.
   -- During all this time, it cares about the lock by locking the current node
   -- and unlocking it right after the right neighbor is locked.
   -- Hence on return, the node N at address N_A is locked!
   -- The maximum count of concurrently held locks is 2.

   procedure Gen_Move_Right
     (Tree : in out Tree_Type;
      N_A  : in out Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type) is
   begin
      Lock (Tree, N_A);
      loop
         declare
            use type Nodes.Valid_Address_Type;
         begin
            Read_Node (Tree, N_A, N);
            exit when Exit_Cond (N);
            if not Nodes.Is_Valid (Nodes.Link (N)) then
               raise Tree_Error;
            end if;
         exception
            when others =>
               Unlock (Tree, N_A);
               raise;
         end;
         declare
            R_A : constant Nodes.Valid_Address_Type := Nodes.Valid_Link (N);
         begin
            Lock (Tree, R_A);
            Unlock (Tree, N_A);
            N_A := R_A;
         end;
      end loop;
   end Gen_Move_Right;


   procedure Create
     (Tree : in out Tree_Type;
      ID   : in     String)
   renames Initialization.Create;


   procedure Create_Temporary
     (Tree : in out Tree_Type;
      ID   : in     String)
   renames Initialization.Create_Temporary;


   procedure Open
     (Tree : in out Tree_Type;
      ID   : in     String)
   renames Initialization.Open;


   procedure Finalize
     (Tree : in out Tree_Type)
   renames Initialization.Finalize;


   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type :=
        Blocks.Bits_To_Units (Values.Value_Type'Size))
      return Blocks.Size_Type
   renames Nodes.Max_Key_Size;


   procedure Search
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   renames Searches.Search;


   procedure Ceiling
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Ceil  :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   renames Searches.Ceiling;


   procedure Search_Minimum
     (Tree  : in out Tree_Type;
      Key   :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   renames Searches.Search_Minimum;


   procedure Insert
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   renames Insertions.Insert;


   procedure Insert
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type)
   renames Insertions.Insert;


   procedure Replace
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   renames Insertions.Replace;


   procedure Replace
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type)
   renames Insertions.Replace;


   procedure Append
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   renames Insertions.Append;


   procedure Delete
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   renames Deletions.Delete;


   procedure Delete_Range
     (Tree  : in out Tree_Type;
      First : in     Keys.Key_Type;
      Last  : in     Keys.Key_Type;
      State :    out State_Type)
   renames Deletions.Delete_Range;


   function Positive_Infinity_Bound
      return Bound_Type
   renames Cursors.Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type
   renames Cursors.Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Keys.Key_Type)
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


   procedure Pause
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   renames Cursors.Pause;


   procedure Next
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type)
   renames Cursors.Next;


   procedure Delete
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      State  :    out State_Type)
   renames Cursors.Delete;


   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   renames Misc.Count;


   procedure Reorganize
     (Tree  : in out Tree_Type;
      State :    out State_Type)
   is separate;


   package body Nodes is separate;
   package body Stacks is separate;
   package body Initialization is separate;
   package body Searches is separate;
   package body Insertions is separate;
   package body Deletions is separate;
   package body Cursors is separate;
   package body Misc is separate;

end DB.DSA.Gen_BTrees;

