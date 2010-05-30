-- Abstract:
--
-- A general, object-oriented map API.
-- The keys of the maps are Types.Keys whereas the values must implement the
-- Value_Type interface.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

with DB.Blocks;
with DB.Types.Keys;
with DB.Types.Values.Bounded.Streams;

package DB.Maps is

   package AF renames Ada.Finalization;

   type Serializable_Type is interface;

   procedure Write
     (Stream : in out Types.Values.Bounded.Streams.Stream_Type'Class;
      Object : in     Serializable_Type)
   is abstract;

   procedure Read
     (Stream : in out Types.Values.Bounded.Streams.Stream_Type'Class;
      Object :    out Serializable_Type)
   is abstract;


   type Comparable_Type is interface;

   function Equals
     (A, B : Comparable_Type)
      return Boolean
   is abstract;

   function Equals
     (Left, Right : Comparable_Type'Class)
      return Boolean;


   subtype Key_Type is DB.Types.Keys.Key_Type;
   use type Key_Type;

   type Value_Type is interface and Serializable_Type and Comparable_Type;

   function Image
     (Value : Value_Type)
      return String
   is abstract;

   ----------
   -- Map initialization operations.

   type Map_Type is abstract new AF.Limited_Controlled with private;

   function New_Map
     (Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type)
      return Map_Type'Class;
   -- Initializes a map object.

   procedure Create
     (Map : in out Map_Type;
      ID  : in     String)
   is abstract;
   -- Creates a new map named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Initialize
     (Map  : in out Map_Type;
      ID   : in     String)
   is abstract;
   -- Initializes Map with the map named ID.

   overriding
   procedure Finalize
     (Map  : in out Map_Type)
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

   Default_Allow_Duplicates : constant Boolean := True;

   function Contains
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean
   is abstract;

   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is abstract;

   procedure Insert
     (Map              : in out Map_Type;
      Key              : in     Key_Type;
      Value            : in     Value_Type'Class;
      Allow_Duplicates : in     Boolean;
      State            :    out State_Type)
   is abstract;

   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type)
   is abstract;


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
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Map         : Map_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type'Class
   is abstract;

   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   is abstract;

   overriding
   procedure Finalize
     (Cursor : in out Cursor_Type)
   is abstract;

   procedure Pause
     (Cursor : in out Cursor_Type)
   is abstract;

   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is abstract;

   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is abstract;

private
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

   type Map_Type is abstract new AF.Limited_Controlled with
      record
         Initialized : Boolean := False;
      end record;

   type Cursor_Type is abstract new AF.Limited_Controlled with
      record
         Initialized : Boolean := False;
      end record;


   pragma Inline (New_Map);
   pragma Inline (Positive_Infinity_Bound);
   pragma Inline (Negative_Infinity_Bound);
   pragma Inline (New_Bound);

end DB.Maps;
