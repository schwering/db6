with Ada.Finalization;

with DB.IO.Blocks;

generic
package DB.Gen_BTrees.Gen_Controlled is
   --pragma Preelaborate;

   type Tree_Type is tagged limited private;
   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;

   type Result_Type is new Gen_BTrees.Result_Type;
   type Count_Type is new Gen_BTrees.Count_Type;
   type Height_Type is new Gen_BTrees.Height_Type;

   type Comparison_Type is new Gen_BTrees.Comparison_Type;
   type Bound_Type is new Gen_BTrees.Bound_Type;

   type Cursor_Type is tagged limited private;


   Tree_Error : exception renames Gen_BTrees.Tree_Error;


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

   procedure Look_Up
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Look_Up
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Look_Up
     (Tree     : in out Tree_Type'Class;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);

   procedure Look_Up
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);

   procedure Minimum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Minimum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Maximum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Maximum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Insert
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
 
   procedure Insert
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree     : in out Tree_Type'Class;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);

   procedure Count
     (Tree : in out Tree_Type'Class;
      N    :    out Count_Type);

   procedure Count
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      N           :    out Count_Type);

   procedure Get_Height
     (Tree   : in out Tree_Type'Class;
      Height :    out Height_Type);

   procedure Get_Height
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type);

   procedure Clusterize
     (Tree  : in out Tree_Type'Class;
      State :    out Result_Type);

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

   procedure Finalize
     (Tree   : in out Tree_Type'Class;
      Cursor : in out Cursor_Type'Class);

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
      State       :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      State       :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      State       :    out Result_Type);

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

