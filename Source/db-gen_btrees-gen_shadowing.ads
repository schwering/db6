with DB.IO.Blocks;

generic
   
package DB.Gen_BTrees.Gen_Shadowing is
   --pragma Preelaborate;

   type Tree_Type is new Gen_BTrees.Tree_Type;
   subtype Transaction_Type is Gen_BTrees.Transaction_Type;
   type RO_Transaction_Type is new Gen_BTrees.RO_Transaction_Type with private;
   type RW_Transaction_Type is new Gen_BTrees.RW_Transaction_Type with private;

   type Version_Type is mod 2**8;
   type Result_Type is new Gen_BTrees.Result_Type;
   type Count_Type is new Gen_BTrees.Count_Type;
   type Height_Type is new Gen_BTrees.Height_Type;

   type Comparison_Type is new Gen_BTrees.Comparison_Type;
   type Bound_Type is new Gen_BTrees.Bound_Type;

   type Cursor_Type is new Gen_BTrees.Cursor_Type;


   Tree_Error : exception renames Gen_BTrees.Tree_Error;


   procedure Create
     (ID : in String);

   procedure Initialize
     (Tree    : out Tree_Type;
      ID      : in  String);

   procedure Finalize
     (Tree : in out Tree_Type);

   procedure Shadow
     (Tree    : in out Tree_Type;
      Version :    out Version_Type);

   function Max_Key_Size
     (Max_Value_Size : IO.Blocks.Size_Type
                     := IO.Blocks.Bits_To_Units(Value_Type'Size))
      return IO.Blocks.Size_Type;

   function New_RO_Transaction
     (Tree : Tree_Type)
      return RO_Transaction_Type;

   function New_RO_Transaction
     (Tree    : Tree_Type;
      Version : Version_Type)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction :    out RO_Transaction_Type);

   procedure Finish_Transaction
     (Tree        : in out Tree_Type;
      Transaction :    out RO_Transaction_Type);

   function New_RW_Transaction
     (Tree : Tree_Type)
      return RW_Transaction_Type;

   function New_RW_Transaction
     (Tree    : Tree_Type;
      Version : Version_Type)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction :    out RW_Transaction_Type);

   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Look_Up
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Look_Up
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);

   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);

   procedure Minimum
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Maximum
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Insert
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
 
   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Update
     (Tree      : in out Tree_Type;
      Version   : in     Version_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type;
      Old_Value :    out Value_Type;
      Position  :    out Count_Type;
      State     :    out Result_Type);

   procedure Update
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Old_Value   :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Update
     (Tree      : in out Tree_Type;
      Version   : in     Version_Type;
      Position  : in     Count_Type;
      Value     : in     Value_Type;
      Old_Value :    out Value_Type;
      Key       :    out Key_Type;
      State     :    out Result_Type);

   procedure Update
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       : in     Value_Type;
      Old_Value   :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree     : in out Tree_Type;
      Version  : in     Version_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);

   procedure Count
     (Tree    : in out Tree_Type;
      Version : in     Version_Type;
      N       :    out Count_Type);

   procedure Count
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      N           :    out Count_Type);

   procedure Get_Height
     (Tree    : in out Tree_Type;
      Version : in     Version_Type;
      Height  :    out Height_Type);

   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type);

   procedure Clusterize
     (Tree  : in out Tree_Type;
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
     (Tree              : Tree_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type;

   procedure Finalize
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type);

   procedure Pause
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type);

   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type);

   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type);

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type);

private
   type Tree_Type is new Gen_BTrees.Tree_Type with null record;

   type RO_Transaction_Type is new Gen_BTrees.RO_Transaction_Type with
      record
         Version : Version_Type;
      end record;

   type RW_Transaction_Type is new Gen_BTrees.RW_Transaction_Type with
      record
         Version : Version_Type;
      end record;

end DB.Gen_BTrees.Gen_Shadowing;

