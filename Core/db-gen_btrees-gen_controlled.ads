-- Abstract:
--
-- A wrapper package for Gen_BTrees which simply finalizes all data structures.
-- This includes Tree_Type, Transaction_Type and Cursor_Type objects.
--
-- Tree_Type objects are Finalized which means that the file is closed (which
-- again might release locks or free memory, this depends on the Block_IO
-- implementation).
--
-- RO_Transaction_Type objects are Finished, RW_Transaction_Type objects are
-- Aborted. In both cases, locks are released and, in the RW_Transaction_Type,
-- the memory for the transaction buffer is freed.
--
-- Cursor_Type objects are Finalized. Since cursors hold no own ressources at
-- the moment, this does nothing.
--
-- Note that the summaries of the finalization effects might not be correct
-- due to changes in the meantime. For exact information, have a look at the
-- respective procedures in Gen_BTrees.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

with DB.IO.Blocks;

generic
package DB.Gen_BTrees.Gen_Controlled is
   pragma Preelaborate;

   type Tree_Type is tagged limited private;
   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;

   type State_Type is new Gen_BTrees.State_Type;
   subtype Count_Type is Gen_BTrees.Count_Type;

   type Comparison_Type is new Gen_BTrees.Comparison_Type;
   type Bound_Type is new Gen_BTrees.Bound_Type;

   type Cursor_Type is tagged limited private;


   procedure Create
     (ID : in String);

   procedure Initialize
     (Tree : out Tree_Type'Class;
      ID   : in  String);

   procedure Finalize
     (Tree : in out Tree_Type'Class);

   function Max_Key_Size
     (Max_Value_Size : IO.Blocks.Size_Type
                     := IO.Blocks.Bits_To_Units(Value_Type'Size))
      return IO.Blocks.Size_Type;

   function New_RO_Transaction
     (Tree : Tree_Type'Class)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RO_Transaction_Type);

   procedure Finish_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RO_Transaction_Type);

   function New_RW_Transaction
     (Tree : Tree_Type'Class)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RW_Transaction_Type);

   procedure Abort_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type);

   procedure Retrieve
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);

   procedure Retrieve
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Minimum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);

   procedure Minimum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Maximum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);

   procedure Maximum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Insert
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      State    :    out State_Type);

   procedure Insert
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      State       :    out State_Type);

   procedure Delete
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Count
     (Tree : in out Tree_Type'Class;
      N    :    out Count_Type);

   procedure Count
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      N           :    out Count_Type);

   procedure Get_Height
     (Tree   : in out Tree_Type'Class;
      Height :    out Natural);

   procedure Get_Height
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Natural);

   procedure Clusterize
     (Tree  : in out Tree_Type'Class;
      State :    out State_Type);

   function Positive_Infinity_Bound
      return Bound_Type;

   function Negative_Infinity_Bound
      return Bound_Type;

   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type;

   function New_Cursor
     (Tree              : Tree_Type'Class;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type;

   procedure Finalize_Cursor
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class);

   procedure Pause
     (Tree   : in out Tree_Type'Class;
      Cursor : in out Cursor_Type'Class);

   procedure Unpause
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class);

   procedure Next
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);

private
   package AF renames Ada.Finalization;

   type Tree_Type is new AF.Limited_Controlled with
      record
         Tree : Gen_BTrees.Tree_Type;
      end record;
   overriding procedure Finalize (Tree : in out Tree_Type);

   type Transaction_Type is abstract new AF.Limited_Controlled with null record;

   type RO_Transaction_Type is new Transaction_Type with
      record
         Transaction : Gen_BTrees.RO_Transaction_Type;
      end record;
   overriding procedure Finalize (Transaction : in out RO_Transaction_Type);

   type RW_Transaction_Type is new Transaction_Type with
      record
         Transaction : Gen_BTrees.RW_Transaction_Type;
      end record;
   overriding procedure Finalize (Transaction : in out RW_Transaction_Type);

   type Cursor_Type is new AF.Limited_Controlled with
      record
         Cursor : Gen_BTrees.Cursor_Type;
      end record;
   overriding procedure Finalize (Cursor : in out Cursor_Type);

end DB.Gen_BTrees.Gen_Controlled;

