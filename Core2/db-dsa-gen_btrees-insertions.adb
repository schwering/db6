-- Abstract:
--
-- Multiple variants that all insert a new Key/Value pair as follows.
-- (1) Descend to a leaf.
-- (2) Move to the right as until a node is reached whose high key allows us to
--     insert the entry. If the high key equals the key of the entry, the entry
--     is only inserted if the node has enough space for it; otherwise the
--     procedure proceeds to the next node.
--     Insert the entry into the found leaf.
-- (3) Now there comes some variant-specific handling (replace or fail if Key
--     already existed) and then start the ascent.
--
-- Design Nodes:
--
-- The insertion obviously agrees to the Stacks.Gen_Modify contract. Check the
-- documentation of Stacks for more information about locking and so on.
--
-- Copyright 2008--2011 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
package body Insertions is

   generic
      with procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type);
   procedure Gen_Insert
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      State     :    out State_Type);

   procedure Gen_Insert
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      State     :    out State_Type)
   is
      function Exit_Cond (N : Nodes.Node_Type) return Boolean
      is
         function Fits_Into_Node
           (K : Keys.Key_Type;
            V : Values.Value_Type)
            return Boolean
         is
            use type Nodes.Extended_Index_Type;
            use type Nodes.Validation_State_Type;
            I : Nodes.Extended_Index_Type;
         begin
            I := Nodes.Key_Position (N, K);
            if not Nodes.Is_Valid (I) then
               I := Nodes.Degree (N) + 1;
            end if;
            return Nodes.Validation (Nodes.Insertion (N, I, K, V)) /=
                   Nodes.Too_Large;
         end Fits_Into_Node;
      begin
         if not Nodes.Is_Leaf (N) then
            return True;
         end if;
         if not Nodes.Is_Valid (Nodes.Link (N)) then
            return True;
         end if;
         case Keys.Compare (Key, Nodes.High_Key (N)) is
            when Utils.Less    => return True;
            when Utils.Equal   => return Fits_Into_Node (Key, Value);
            when Utils.Greater => return False;
         end case;
      end Exit_Cond;

      use type Blocks.Size_Type;
      procedure Modify is new Stacks.Gen_Modify (Exit_Cond, Modify_Node);
   begin
      if Keys.Size_Bound (Key) > Max_Key_Size (Values.Size_Bound (Value)) then
         State := Failure;
         return;
      end if;
      Modify (Tree, Key, State);
   end Gen_Insert;


   procedure Insert
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   is
      procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type)
      is
         use type Nodes.Degree_Type;
         I : Nodes.Extended_Index_Type;
      begin
         I := Nodes.Key_Position (N_Old, Key);
         if not Nodes.Is_Valid (I) then
            I := Nodes.Degree (N_Old) + 1;
            N := Nodes.Insertion (N_Old, I, Key, Value);
            State := Success;
         else
            if Nodes.Key (N_Old, I) = Key then
               State := Failure;
            else
               N := Nodes.Insertion (N_Old, I, Key, Value);
               State := Success;
            end if;
         end if;
      end Modify_Node;

      procedure Insert is new Gen_Insert (Modify_Node);
   begin
      Insert (Tree, Key, Value, State);
   end Insert;


   procedure Insert
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type)
   is
      procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type)
      is
         use type Nodes.Degree_Type;
         I : Nodes.Extended_Index_Type;
      begin
         I := Nodes.Key_Position (N_Old, Key);
         if not Nodes.Is_Valid (I) then
            I := Nodes.Degree (N_Old) + 1;
            N := Nodes.Insertion (N_Old, I, Key, Value);
            Existed := False;
            State := Success;
         else
            if Nodes.Key (N_Old, I) = Key then
               Existed := True;
               Old_Value := Nodes.Value (N_Old, I);
               State := Failure;
            else
               N := Nodes.Insertion (N_Old, I, Key, Value);
               Existed := False;
               State := Success;
            end if;
         end if;
      end Modify_Node;

      procedure Insert is new Gen_Insert (Modify_Node);
   begin
      Insert (Tree, Key, Value, State);
   end Insert;


   procedure Replace
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   is
      procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type)
      is
         use type Nodes.Degree_Type;
         I : Nodes.Extended_Index_Type;
      begin
         I := Nodes.Key_Position (N_Old, Key);
         if not Nodes.Is_Valid (I) then
            I := Nodes.Degree (N_Old) + 1;
            N := Nodes.Insertion (N_Old, I, Key, Value);
         else
            if Nodes.Key (N_Old, I) = Key then
               N := Nodes.Substitution (N_Old, I, Key, Value);
            else
               N := Nodes.Insertion (N_Old, I, Key, Value);
            end if;
         end if;
         State := Success;
      end Modify_Node;

      procedure Replace is new Gen_Insert (Modify_Node);
   begin
      Replace (Tree, Key, Value, State);
   end Replace;


   procedure Replace
     (Tree      : in out Tree_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type)
   is
      procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type)
      is
         use type Nodes.Degree_Type;
         I : Nodes.Extended_Index_Type;
      begin
         I := Nodes.Key_Position (N_Old, Key);
         if not Nodes.Is_Valid (I) then
            I := Nodes.Degree (N_Old) + 1;
            N := Nodes.Insertion (N_Old, I, Key, Value);
            Existed := False;
         else
            if Nodes.Key (N_Old, I) = Key then
               N := Nodes.Substitution (N_Old, I, Key, Value);
               Existed := True;
               Old_Value := Nodes.Value (N_Old, I);
            else
               N := Nodes.Insertion (N_Old, I, Key, Value);
               Existed := False;
            end if;
         end if;
         State := Success;
      end Modify_Node;

      procedure Replace is new Gen_Insert (Modify_Node);
   begin
      Replace (Tree, Key, Value, State);
   end Replace;


   procedure Append
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type)
   is
      procedure Modify_Node
        (N_Old : in  Nodes.Node_Type;
         N     : out Nodes.RW_Node_Type;
         State : out State_Type)
      is
         use type Nodes.Degree_Type;
         I : Nodes.Extended_Index_Type;
      begin
         I := Nodes.Key_Position (N_Old, Key);
         if not Nodes.Is_Valid (I) then
            I := Nodes.Degree (N_Old) + 1;
         end if;
         N     := Nodes.Insertion (N_Old, I, Key, Value);
         State := Success;
      end Modify_Node;

      procedure Append is new Gen_Insert (Modify_Node);
   begin
      Append (Tree, Key, Value, State);
   end Append;

end Insertions;

