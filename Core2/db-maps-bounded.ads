-- Abstract:
--
-- Maps implementation using normal Gen_BTrees with Types.Keys and
-- Maps.Values.
--
-- This implementation ignores Column_Regexp parameters.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
private with DB.DSA.Gen_BTrees;
private with DB.Blocks.Local_IO;

package DB.Maps.Bounded is
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

   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.

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
   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);

   overriding
   procedure Ceiling
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Ceil  :    out Keys.Key_Type;
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

   type Cursor_Type is limited new Maps.Cursor_Type with private;

   overriding
   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Maps.Cursor_Type'Class;

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
   package Block_IO_Impl renames Blocks.Local_IO;
   package Block_IO      renames Block_IO_Impl.IO_Signature;

   package BTrees is new DSA.Gen_BTrees
     (Keys     => Keys.Keys_Signature,
      Values   => Values.Values_Signature,
      Block_IO => Block_IO);

   type Map_Ref_Type is access all Map_Type;
   pragma Controlled (Map_Ref_Type);
   for Map_Ref_Type'Storage_Size use 0;

   type Map_Type is new Maps.Map_Type with
      record
         Initialized      : Boolean;
         Allow_Duplicates : Boolean;
         Self             : Map_Ref_Type := Map_Type'Unchecked_Access;
         Tree             : BTrees.Tree_Type;
      end record;


   type Cursor_Type is limited new Maps.Cursor_Type with
      record
         Initialized : Boolean;
         Map         : Map_Ref_Type;
         Cursor      : BTrees.Cursor_Type;
      end record;

end DB.Maps.Bounded;

