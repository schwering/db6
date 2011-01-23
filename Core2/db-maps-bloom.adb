-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Maps.Bloom is

   function New_Map
     (Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type is
   begin
      return (Maps.Map_Type with
              Map    => new Base_Map_Type'(Bounded.New_Map (Allow_Duplicates)),
              others => <>);
   end New_Map;


   procedure Init_Bloom_Filter (Map : in out Map_Type)
   is
      Cursor : Maps.Cursor_Type'Class := Map.Map.New_Cursor
        (Thread_Safe => False,
         Lower_Bound => Negative_Infinity_Bound,
         Upper_Bound => Positive_Infinity_Bound);
   begin
      Bloom_Filters.Reset (Map.Filter);
      loop
         declare
            Key   : Keys.Key_Type;
            Value : Values.Value_Type;
            State : State_Type;
         begin
            Cursor.Next (Key, Value, State);
            exit when State /= Success;
            Bloom_Filters.Insert (Map.Filter, Key);
         end;
      end loop;
   end Init_Bloom_Filter;


   procedure Create (Map : in out Map_Type; ID : in String) is
   begin
      Map.Map.Create (ID);
      Init_Bloom_Filter (Map);
   end Create;


   procedure Create_Temporary (Map : in out Map_Type; ID : in String) is
   begin
      Map.Map.Create_Temporary (ID);
      Init_Bloom_Filter (Map);
   end Create_Temporary;


   procedure Open (Map : in out Map_Type; ID : in String) is
   begin
      Map.Map.Open (ID);
      Init_Bloom_Filter (Map);
   end Open;


   procedure Finalize (Map : in out Map_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Base_Map_Type, Base_Map_Ref_Type);
   begin
      if Map.Map /= null then
         Map.Map.Finalize;
         Free (Map.Map);
      end if;
   end Finalize;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type is
   begin
      return Map.Map.Max_Key_Size (Max_Value_Size);
   end Max_Key_Size;


   overriding
   function Contains
     (Map : Map_Type;
      Key : Keys.Key_Type)
      return Boolean is
   begin
      return Bloom_Filters.Contains (Map.Filter, Key) and then
             Map.Map.Contains (Key);
   end Contains;


   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type) is
   begin
      if Bloom_Filters.Contains (Map.Filter, Key) then
         Map.Map.Search (Key, Value, State);
      else
         State := Failure;
         return;
      end if;
   end Search;


   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type) is
   begin
      Map.Map.Search_Minimum (Key, Value, State);
   end Search_Minimum;


   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type) is
   begin
      Map.Map.Insert (Key, Value, State);
      if State = Success then
         Bloom_Filters.Insert (Map.Filter, Key);
      end if;
   end Insert;


   procedure Insert
     (Map       : in out Map_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type) is
   begin
      Map.Map.Insert (Key, Value, Existed, Old_Value, State);
      if State = Success then
         Bloom_Filters.Insert (Map.Filter, Key);
      end if;
   end Insert;


   procedure Replace
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type) is
   begin
      Map.Map.Replace (Key, Value, State);
      if State = Success then
         Bloom_Filters.Insert (Map.Filter, Key);
      end if;
   end Replace;


   procedure Replace
     (Map       : in out Map_Type;
      Key       : in     Keys.Key_Type;
      Value     : in     Values.Value_Type;
      Existed   :    out Boolean;
      Old_Value :    out Values.Value_Type;
      State     :    out State_Type) is
   begin
      Map.Map.Replace (Key, Value, Existed, Old_Value, State);
      if State = Success then
         Bloom_Filters.Insert (Map.Filter, Key);
      end if;
   end Replace;


   procedure Append
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type) is
   begin
      Map.Map.Append (Key, Value, State);
      if State = Success then
         Bloom_Filters.Insert (Map.Filter, Key);
      end if;
   end Append;


   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type) is
   begin
      if Bloom_Filters.Contains (Map.Filter, Key) then
         Map.Map.Delete (Key, Value, State);
      else
         State := Failure;
         return;
      end if;
   end Delete;


   procedure Delete_Range
     (Map   : in out Map_Type;
      First : in     Keys.Key_Type;
      Last  : in     Keys.Key_Type;
      State :    out State_Type) is
   begin
      Map.Map.Delete_Range (First, Last, State);
   end Delete_Range;


   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Maps.Cursor_Type'Class is
   begin
      return Cursor_Type'(Maps.Cursor_Type with
              Cursor => new Base_Cursor_Type'(Bounded.New_Cursor
                 (Map.Map.all, Thread_Safe, Lower_Bound, Upper_Bound,
                  Column_Regexp)),
              others => <>);
   end New_Cursor;


   overriding
   procedure Finalize
     (Cursor : in out Cursor_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Base_Cursor_Type, Base_Cursor_Ref_Type);
   begin
      if Cursor.Cursor /= null then
         Cursor.Cursor.Finalize;
         Free (Cursor.Cursor);
      end if;
   end Finalize;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      Cursor.Cursor.Set_Thread_Safety (Enabled);
   end Set_Thread_Safety;


   procedure Pause (Cursor : in out Cursor_Type) is
   begin
      Cursor.Cursor.Pause;
   end Pause;


   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type) is
   begin
      Cursor.Cursor.Next (Key, Value, State);
   end Next;


   procedure Delete
     (Cursor : in out Cursor_Type;
      State  :    out State_Type) is
   begin
      Cursor.Cursor.Delete (State);
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type)
   is
   begin
      Map.Map.Count (Count);
   end Count;


   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type) is
   begin
      Map.Map.Reorganize (State);
   end Reorganize;


   overriding
   procedure Check
     (Map : in out Map_Type) is
   begin
      Map.Map.Check;
   end Check;


   overriding
   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type)) is
   begin
      Map.Map.Stats (Emit);
   end Stats;

end DB.Maps.Bloom;

