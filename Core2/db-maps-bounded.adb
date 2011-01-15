-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Gen_BTrees.Gen_Check;
with DB.DSA.Gen_BTrees.Gen_Stats;

package body DB.Maps.Bounded is

   function To_State
     (State : BTrees.State_Type)
      return State_Type
   is
      use type BTrees.State_Type;
      S : State_Type;
   begin
      case State is
         when BTrees.Success => S := Success;
         when BTrees.Failure => S := Failure;
      end case;
      return S;
   end To_State;


   function New_Map
     (Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type is
   begin
      return Map_Type'(Maps.Map_Type with
                       Initialized      => False,
                       Allow_Duplicates => Allow_Duplicates,
                       others           => <>);
   end New_Map;


   procedure Create (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);
   begin
      BTrees.Create (Map.Tree, ID);
      Map.Initialized := True;
   end Create;


   procedure Create_Temporary (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);
   begin
      BTrees.Create_Temporary (Map.Tree, ID);
      Map.Initialized := True;
   end Create_Temporary;


   procedure Open (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);
   begin
      BTrees.Open (Map.Tree, ID);
      Map.Initialized := True;
   end Open;


   procedure Finalize (Map : in out Map_Type) is
   begin
      if Map.Initialized then
         BTrees.Finalize (Map.Tree);
         Map.Initialized := False;
      end if;
   end Finalize;


   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type is
   begin
      return BTrees.Max_Key_Size (Max_Value_Size);
   end Max_Key_Size;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type
   is
      pragma Unreferenced (Map);
   begin
      return Max_Key_Size (Max_Value_Size);
   end Max_Key_Size;


   overriding
   function Contains
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean
   is
      use type BTrees.State_Type;
      State         : BTrees.State_Type;
      Value_Wrapper : Value_Wrapper_Type;
   begin
      BTrees.Search (Map.Self.Tree, Key, Value_Wrapper, State);
      return State = BTrees.Success;
   end Contains;


   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Search (Map.Tree, Key, Value, S);
      State := To_State (S);
   end Search;


   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Search_Minimum (Map.Tree, Key, Value, S);
      State := To_State (S);
   end Search_Minimum;


   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      VW : constant Value_Wrapper_Type := New_Value_Wrapper (Value);
      S  : BTrees.State_Type;
   begin
      BTrees.Insert (Map.Tree, Key, VW, S);
      State := To_State (S);
   end Insert;


   procedure Insert
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is
      VW : constant Value_Wrapper_Type := New_Value_Wrapper (Value);
      S  : BTrees.State_Type;
   begin
      BTrees.Insert (Map.Tree, Key, VW, Existed, Old_Value, S);
      State := To_State (S);
   end Insert;


   procedure Replace
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      VW : constant Value_Wrapper_Type := New_Value_Wrapper (Value);
      S  : BTrees.State_Type;
   begin
      BTrees.Replace (Map.Tree, Key, VW, S);
      State := To_State (S);
   end Replace;


   procedure Replace
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is
      VW : constant Value_Wrapper_Type := New_Value_Wrapper (Value);
      S  : BTrees.State_Type;
   begin
      BTrees.Replace (Map.Tree, Key, VW, Existed, Old_Value, S);
      State := To_State (S);
   end Replace;


   procedure Append
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      VW : constant Value_Wrapper_Type := New_Value_Wrapper (Value);
      S  : BTrees.State_Type;
   begin
      BTrees.Append (Map.Tree, Key, VW, S);
      State := To_State (S);
   end Append;



   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Delete (Map.Tree, Key, Value, S);
      State := To_State (S);
   end Delete;


   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Maps.Cursor_Type'Class
   is
      pragma Precondition (Map.Initialized);
      pragma Unreferenced (Column_Regexp);

      function New_Bound
        (Comparison : Comparison_Type;
         Key        : Key_Type)
         return BTrees.Bound_Type
      is
         C : BTrees.Comparison_Type;
      begin
         case Comparison is
            when Less             => C := BTrees.Less;
            when Less_Or_Equal    => C := BTrees.Less_Or_Equal;
            when Equal            => C := BTrees.Equal;
            when Greater_Or_Equal => C := BTrees.Greater_Or_Equal;
            when Greater          => C := BTrees.Greater;
         end case;
         return BTrees.New_Bound (C, Key);
      end New_Bound;

      function To_Bound
        (Bound : Bound_Type)
         return BTrees.Bound_Type is
      begin
         case Bound.Concrete is
            when True =>
               return New_Bound (Bound.Comparison, Bound.Key);
            when False =>
               case Bound.Infinity is
                  when Negative_Infinity =>
                     return BTrees.Negative_Infinity_Bound;
                  when Positive_Infinity =>
                     return BTrees.Positive_Infinity_Bound;
               end case;
         end case;
      end To_Bound;

      B_Lower_Bound : constant BTrees.Bound_Type := To_Bound (Lower_Bound);
      B_Upper_Bound : constant BTrees.Bound_Type := To_Bound (Upper_Bound);
   begin
      return Cursor_Type'(Maps.Cursor_Type with
                          Initialized => True,
                          Map         => Map.Self,
                          Cursor      => BTrees.New_Cursor
                            (Tree        => Map.Tree,
                             Thread_Safe => Thread_Safe,
                             Lower_Bound => B_Lower_Bound,
                             Upper_Bound => B_Upper_Bound));
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      BTrees.Set_Thread_Safety (Cursor.Cursor, Enabled);
   end Set_Thread_Safety;


   procedure Pause
     (Cursor : in out Cursor_Type) is
   begin
      BTrees.Pause (Cursor.Map.Tree, Cursor.Cursor);
   end Pause;


   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Wrapper_Type;
      State  :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Next (Cursor.Map.Tree, Cursor.Cursor, Key, Value, S);
      State := To_State (S);
   end Next;


   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Wrapper_Type;
      State  :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Delete (Cursor.Map.Tree, Cursor.Cursor, Key, Value, S);
      State := To_State (S);
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type) is
   begin
      BTrees.Count (Map.Tree, Count);
   end Count;


   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type)
   is
      S      : BTrees.State_Type;
   begin
      BTrees.Reorganize (Map.Tree, S);
      State := To_State (S);
   end Reorganize;


   overriding
   procedure Check
     (Map : in out Map_Type)
   is
      function Key_To_String (Key : Key_Type) return String
      is
         function Row_Image (S : Keys.Rows.String_Type) return String is
         begin
            return String (Keys.Rows.To_Buffer (S));
         end Row_Image;

         function Column_Image
           (S : Types.Keys.Columns.String_Type)
            return String is
         begin
            return String (Keys.Columns.To_Buffer (S));
         end Column_Image;
      begin
         return "("& Row_Image (Key.Row) &", "& Column_Image (Key.Column) &", "&
                Key.Time'Img &")";
      end Key_To_String;

      function Value_To_String
        (Value : Value_Wrapper_Type)
         return String is
      begin
         return Value.Ref.Image;
      end Value_To_String;

      procedure Check is new BTrees.Gen_Check
        (Key_To_String   => Key_To_String,
         Value_To_String => Value_To_String);
   begin
      Check (Map.Tree);
   end Check;


   overriding
   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type))
   is
      package Stats is new BTrees.Gen_Stats;
      procedure My_Emit
        (Level : in Stats.Level_Type;
         Key   : in String;
         Value : in Stats.Data_Type) is
      begin
         case Value.Compound is
            when True =>
               Emit (Level_Type (Level),
                     Key,
                     Data_Type'(Compound => True,
                                Avg => Average_Type (Value.Avg),
                                Var => Average_Type (Value.Var),
                                Min => Absolute_Type (Value.Min),
                                Max => Absolute_Type (Value.Max)));
            when False =>
               Emit (Level_Type (Level),
                     Key,
                     Data_Type'(Compound => False,
                                Val => Absolute_Type (Value.Val)));
         end case;
      end;
   begin
      Stats.Make_Stats (Map.Tree, My_Emit'Access);
   end Stats;

end DB.Maps.Bounded;

