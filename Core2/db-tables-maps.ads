-- Abstract:
--
-- Wrapper for BTree and Blob_Tree. Decides which to use depending on the
-- maximum value size.
--
-- Design Notes:
--
-- Experimentally, I have removed the (<>) in the public type declarations and
-- given all Short discriminants the default value True. The idea is that this
-- allows to have objects of the types defined here as component in records.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks.Gen_Values_Signature;
with DB.Gen_BTrees;
--with DB.Gen_Blob_Trees;
with DB.Blocks;
with DB.Blocks.Local_IO;

package DB.Tables.Maps is
   pragma Elaborate_Body;

   ----------
   -- Map initialization operations.

   type Map_Type is limited private;

   function New_Map
     (Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type)
      return Map_Type;
   -- Initializes a map object.

   procedure Create
     (ID             : in String;
      Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type);
   -- Creates a new map named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Initialize
     (Map  : out Map_Type;
      ID   : in  String);
   -- Initializes Map with the map named ID.

   procedure Finalize
     (Map  : in out Map_Type);
   -- Finalizes Map, i.e. closes opened files.

   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.


   ----------
   -- Core operations: Search, Insertion, Deletion.

   type State_Type is (Success, Failure, Error);

   procedure Search
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);

   procedure Minimum
     (Map      : in out Map_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);

   procedure Insert
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type'Class;
      State    :    out State_Type);

   procedure Delete
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);


   ----------
   -- Miscellaneous procedures.

   subtype Count_Type is Long_Integer;

   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type);

   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type);


   ----------
   -- Cursor operations.

   type Comparison_Type is (Less, Less_Or_Equal, Equal, Greater_Or_Equal,
      Greater);
   type Bound_Type is private;
   type Cursor_Type is limited private;

   function Positive_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Map        : Map_Type;
      Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Map         : Map_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type;

   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean);

   procedure Finalize_Cursor
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type);

   procedure Pause
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type);

   procedure Next
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type);

   procedure Delete
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type);

private
   package Bounded_Values_IO   renames Types.Values.Bounded.Uncompressed;
   package Unbounded_Values_IO renames Types.Values.Unbounded.Uncompressed;
   package Block_IO_Impl       renames Blocks.Local_IO;
   package Block_IO            renames Block_IO_Impl.IO_Signature;

   package Values is
      use Bounded_Values_IO;   -- so that the serialization instances take
      use Unbounded_Values_IO; -- the default parameters

      package Bounded_Values_Signature is new Blocks.Gen_Values_Signature
        (Value_Type         => Types.Values.Bounded.String_Type,
         Read_Context_Type  => Bounded_Values_IO.Read_Context_Type,
         Write_Context_Type => Bounded_Values_IO.Write_Context_Type);

      package Unbounded_Values_Signature is new Blocks.Gen_Values_Signature
        (Value_Type         => Types.Values.Unbounded.String_Type,
         Read_Context_Type  => Unbounded_Values_IO.Read_Context_Type,
         Write_Context_Type => Unbounded_Values_IO.Write_Context_Type);
   end Values;

   package BTrees is new Gen_BTrees
     (Keys             => Types.Keys.Keys_Signature,
      Values           => Values.Bounded_Values_Signature,
      Allow_Duplicates => True,
      Block_IO         => Block_IO);

   package Blob_Trees is new Gen_BTrees
     (Keys             => Types.Keys.Keys_Signature,
      Values           => Values.Unbounded_Values_Signature,
      Block_IO         => Block_IO,
      Allow_Duplicates => True);
      --Parted_Value_Context_Type => Types.Values.Unbounded.Parted.Context_Type,
      --New_Parted_Value_Context => Types.Values.Unbounded.Parted.New_Context,
      --Parted_Value_Size_Bound => Types.Values.Unbounded.Parted.Size_Bound,
      --Fold_Value_Contexts => Types.Values.Unbounded.Parted.Fold_Contexts,
      --Value_Context_Size_Bound =>
                             --Types.Values.Unbounded.Parted.Context_Size_Bound,
      --Read_Value_Context  => Types.Values.Unbounded.Parted.Read_Context,
      --Write_Value_Context => Types.Values.Unbounded.Parted.Write_Context,
      --Read_Part_Of_Value  => Types.Values.Unbounded.Parted.Read_Part_Of_String,
      --Write_Part_Of_Value => Types.Values.Unbounded.Parted.Write_Part_Of_String,

   ----------
   -- Type wrappers. Always Short (=> BTrees) and not Short (=> Blob_Trees).

   type Map_Type (Short : Boolean := True) is limited
      record
         case Short is
            when True =>  Short_Tree : BTrees.Tree_Type;
            when False => Long_Tree  : Blob_Trees.Tree_Type;
         end case;
      end record;

   type Bound_Type (Short : Boolean := True) is
      record
         case Short is
            when True =>  Short_Bound : BTrees.Bound_Type;
            when False => Long_Bound : Blob_Trees.Bound_Type;
         end case;
      end record;

   type Cursor_Type (Short : Boolean := True) is limited
      record
         case Short is
            when True =>  Short_Cursor : BTrees.Cursor_Type;
            when False => Long_Cursor  : Blob_Trees.Cursor_Type;
         end case;
      end record;

end DB.Tables.Maps;

