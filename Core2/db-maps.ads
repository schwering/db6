-- Abstract:
--
-- A general, object-oriented map API whose keys are composite types consisting
-- of a row ID, a column ID and a timestamp whereas the values must implement
-- the Value_Type interface.
--
-- The API of Value_Type has a disadvantage:
-- The implented Read procedure usually wants to know how many bytes can be
-- read, for example the length of the string. This information could be written
-- in the corresponding Write procedure, but this would be redundant as the
-- data is written as DB.Types.Values.Bounded.String_Type, i.e. with a length.
-- The latter cannot be (easily) changed, because it is an integral part of the
-- Gen_BTrees instance and Value_Type's Read function is invoked later, way out
-- of control of Gen_BTrees. However, the length information must be present in
-- Gen_BTrees control flow.
-- So there are multiple ways to handle this problem:
-- 1. Choose DB.Types.Values.Bounded.Streams'Class instead of
--    Ada.Streams.Root_Stream_Type'Class as Read/Write and make use of the
--    Remaining function.
-- 2. Cast the Ada.Streams.Root_Stream_Type'Class and call the Remaining
--    function when needed (i.e. for objects with non-static size).
--    This yields the question whether it is a broken design aspect that the
--    length of values is always written. But I think it isn't:
--
--    One might argue that we could leave the length -- or some other size
--    indicator -- out and determine the size -- or the actual Read procedure --
--    via the value object. A standard way how to do this would be object
--    orientation. One could claim that the user searches for a certain key or
--    so and knows the type of the corresponding value.
--    But even then this idea doesn't work out: the cursor, or more precisely
--    the caller of the cursor's Next operation, doesn't know the next item's
--    type.
-- 3. Forgot it. Is there even a third approach?
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Finalization;
with Ada.Streams;

with DB.Blocks;
with DB.Types.Keys;
with DB.Utils.Gen_Auto_Pointers;

package DB.Maps is

   package AF renames Ada.Finalization;

   type Serializable_Type is interface;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Object : in              Serializable_Type)
   is abstract;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Object : out             Serializable_Type)
   is abstract;

   function Size_Bound
     (Object : Serializable_Type)
      return Ada.Streams.Stream_Element_Offset
   is abstract;


   type Comparable_Type is interface;

   function Equals (A, B : Comparable_Type) return Boolean
   is abstract;

   function Equals (Left, Right : Comparable_Type'Class) return Boolean;


   type Printable_Type is interface;

   function Image (Printable : Printable_Type) return String
   is abstract;


   package Keys renames Types.Keys;
   subtype Key_Type is Keys.Key_Type;

   function Row_To_String (Row : Keys.Rows.String_Type) return String;
   function Column_To_String (Column : Keys.Columns.String_Type) return String;
   function String_To_Row (S : String) return Keys.Rows.String_Type;
   function String_To_Column (S : String) return Keys.Columns.String_Type;
   function Strings_To_Key (Row : String; Column : String) return Keys.Key_Type;

   type Value_Type is abstract new AF.Controlled and
                                   Serializable_Type and
                                   Comparable_Type and
                                   Printable_Type with null record;

   type Value_Ref_Type is access Value_Type'Class;

   type Value_Parameters_Type is null record;

   function New_Value
     (Params : not null access Value_Parameters_Type)
      return Value_Type
   is abstract;
   -- The purpose of this constructor is to initialize a default instance of a
   -- Value_Type subclass.
   -- It is required for the Ada.Tags.Generic_Dispatching_Constructor.

   package Value_Wrappers is new Utils.Gen_Auto_Pointers
     (Value_Type'Class, Value_Ref_Type);

   subtype Value_Wrapper_Type is Value_Wrappers.Auto_Pointer_Type;

   function New_Value_Wrapper
     (Value : Value_Type'Class)
      return Value_Wrapper_Type
   renames Value_Wrappers.New_Auto_Pointer;


   ----------
   -- Map initialization operations.

   type Implementation_Type is (BTree, Multi);
   type Map_Type is abstract new AF.Limited_Controlled with private;
   type Map_Ref_Type is access Map_Type'Class;

   Default_Allow_Duplicates : constant Boolean := True;

   function New_Map
     (Implementation   : in Implementation_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class;
   -- Initializes a map object living on the stack.

   function New_Map_Ref
     (Implementation   : in Implementation_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Ref_Type;
   -- Initializes a map object living on the heap.

   function New_Map
     (Max_Key_Size     : in Blocks.Size_Type;
      Max_Value_Size   : in Blocks.Size_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class;
   -- Initializes a map object.

   procedure Create (Map : in out Map_Type; ID : in String)
   is abstract;
   -- Creates a new map named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Create_Temporary (Map : in out Map_Type; ID : in String)
   is abstract;
   -- Creates a new temporary map named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Open (Map : in out Map_Type; ID : in String)
   is abstract;
   -- Initializes Map with the map named ID.

   procedure Finalize (Map : in out Map_Type)
   is abstract;
   -- Finalizes Map, i.e. closes opened files.

   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type
   is abstract;
   -- Returns the maximum allowed size of keys.


   ----------
   -- Core operations: Search, Insertion, Deletion.

   type State_Type is (Success, Failure);

   function Contains
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean
   is abstract;

   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is abstract;

   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);

   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is abstract;

   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);

   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Insert
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is abstract;

   procedure Replace
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Replace
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is abstract;

   procedure Append
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is abstract;

   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);


   ----------
   -- Miscellaneous procedures.

   subtype Count_Type is Long_Integer;
   subtype Level_Type is Natural;
   subtype Absolute_Type is Long_Integer;
   type Average_Type is delta 10.0**(-1) digits 17;
   type Data_Type (Compound : Boolean) is
      record
         case Compound is
            when True =>
               Avg : Average_Type;
               Var : Average_Type;
               Min : Absolute_Type;
               Max : Absolute_Type;
            when False =>
               Val : Absolute_Type;
         end case;
      end record;

   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type)
   is abstract;

   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type)
   is abstract;

   procedure Check
     (Map : in out Map_Type)
   is abstract;

   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type))
   is abstract;


   ----------
   -- Cursor operations.

   type Comparison_Type is (Less, Less_Or_Equal, Equal, Greater_Or_Equal,
      Greater);
   type Bound_Type is private;
   type Cursor_Type is abstract new AF.Limited_Controlled with private;
   type Cursor_Ref_Type is access Cursor_Type'Class;

   function Positive_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound (Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min, where Key is the tuple (Row, Col, Time).

   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Cursor_Type'Class
   is abstract;
   -- Creates a new cursor that iterates over the elements between Lower_Bound
   -- and Upper_Bound.
   -- If Thread_Safe is true, one can use the cursor from multiple tasks.
   -- Column_Regexp can be used to give a hint which attributes are interesting;
   -- the implementation does not have to iterate over elements whose key's
   -- column doesn't match Column_Regexp; i.e. to ensure that some columns are
   -- not sent to the user, you have to check the columns yourself. The
   -- implementation may ignore Column_Regexp. An empty string means is a
   -- shorthand for the regexp that accepts everything; i.e. all columns are
   -- visited. For regular expression syntax and implementation, check out
   -- DB.Utils.Regular_Expressions.

   function New_Cursor_Ref
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Cursor_Ref_Type;

   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   is abstract;

   procedure Pause
     (Cursor : in out Cursor_Type)
   is abstract;

   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Wrapper_Type;
      State  :    out State_Type)
   is abstract;

   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type);

   procedure Delete
     (Cursor : in out Cursor_Type;
      State  :    out State_Type)
   is abstract;

private
   type Map_Type is abstract new AF.Limited_Controlled with null record;

   type Cursor_Type is abstract new AF.Limited_Controlled with null record;

   type Infinity_Type is (Positive_Infinity, Negative_Infinity);

   type Bound_Type (Concrete : Boolean := True) is
      record
         case Concrete is
            when True =>
               Comparison : Comparison_Type;
               Key        : Key_Type;
            when False =>
               Infinity   : Infinity_Type;
         end case;
      end record;

   pragma Inline (New_Map);
   pragma Inline (Positive_Infinity_Bound);
   pragma Inline (Negative_Infinity_Bound);
   pragma Inline (New_Bound);

end DB.Maps;

