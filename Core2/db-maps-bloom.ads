-- Abstract:
--
-- Maps implementation with a Bloom filter.
--
-- Copyright 2008--2011 Christoph Schwering

private with DB.Maps.Bounded;
private with DB.DSA.Utils.Gen_Bloom_Filters;

package DB.Maps.Bloom is
   pragma Elaborate_Body;

   ----------
   -- Map initialization operations.

   type Map_Type is new Maps.Map_Type with private;

   not overriding
   function New_Map
     (Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type;

   overriding
   procedure Create (Map : in out Map_Type; ID : in String);
   -- Creates a new map named ID or raises a DB.IO_Error when creation
   -- fails.

   overriding
   procedure Create_Temporary (Map : in out Map_Type; ID : in String);
   -- Creates a new temporary map named ID or raises a DB.IO_Error when creation
   -- fails.

   overriding
   procedure Open (Map : in out Map_Type; ID : in String);
   -- Opens Map with the map named ID.

   overriding
   procedure Finalize (Map : in out Map_Type);
   -- Finalizes Map, i.e. closes opened files.

   overriding
   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.


   ----------
   -- Core operations: Search, Insertion, Deletion.

   overriding
   function Contains
     (Map : Map_Type;
      Key : Keys.Key_Type)
      return Boolean;

   overriding
   procedure Ceiling
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Ceil  :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Insert
     (Map       : in out Map_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type);

   overriding
   procedure Replace
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Replace
     (Map       : in out Map_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type);

   overriding
   procedure Append
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Delete_Range
     (Map   : in out Map_Type;
      First : in     Keys.Key_Type;
      Last  : in     Keys.Key_Type;
      State :    out State_Type);


   ----------
   -- Miscellaneous procedures.

   overriding
   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type);

   overriding
   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type);

   overriding
   procedure Check
     (Map : in out Map_Type);

   overriding
   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type));


   ----------
   -- Cursor operations.

   type Cursor_Type is new Maps.Cursor_Type with private;

   overriding
   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Maps.Cursor_Type'Class;

   overriding
   procedure Finalize
     (Cursor : in out Cursor_Type);

   overriding
   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean);

   overriding
   procedure Pause
     (Cursor : in out Cursor_Type);

   overriding
   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type);

   overriding
   procedure Delete
     (Cursor : in out Cursor_Type;
      State  :    out State_Type);

private
   subtype Base_Map_Type is Bounded.Map_Type;
   type Base_Map_Ref_Type is access Base_Map_Type;

   Mega_Bytes : constant := 1024 * 1024;

   package Bloom_Filters is new DSA.Utils.Gen_Bloom_Filters
     (Keys.Key_Type, Keys.Hash_1, Keys.Hash_2, Keys.Hash_3, Keys.Hash_4, 
      Size_In_Bytes => 2 * Mega_Bytes);

   type Map_Type is new Maps.Map_Type with
      record
         Map    : Base_Map_Ref_Type := null;
         Filter : Bloom_Filters.Bloom_Filter_Type;
      end record;

   subtype Base_Cursor_Type is Maps.Cursor_Type'Class;
   type Base_Cursor_Ref_Type is access Base_Cursor_Type;

   type Cursor_Type is new Maps.Cursor_Type with
      record
         Cursor : Base_Cursor_Ref_Type := null;
      end record;

end DB.Maps.Bloom;

