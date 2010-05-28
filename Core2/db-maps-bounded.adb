-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Maps.Bounded is

   procedure Create
     (Map : in Map_Type;
      ID  : in String)
   is
      pragma Unreferenced (Map);
   begin
      BTrees.Create(ID);
   end Create;


   procedure Initialize
     (Map : out Map_Type;
      ID  : in  String) is
   begin
      BTrees.Initialize(Map.Tree, ID);
   end Initialize;


   procedure Finalize
     (Map : in out Map_Type) is
   begin
      BTrees.Finalize(Map.Tree);
   end Finalize;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type is
   begin
      return BTrees.Max_Key_Size(Max_Value_Size);
   end Max_Key_Size;


   function To_State
     (State : BTrees.State_Type)
      return State_Type
   is
      use type BTrees.State_Type;
   begin
      case State is
         when BTrees.Success => return Success;
         when BTrees.Failure => return Failure;
         when BTrees.Error   => return Error;
      end case;
   end To_State;


   function To_State
     (State : Blob_Trees.State_Type)
      return State_Type
   is
      use type Blob_Trees.State_Type;
   begin
      case State is
         when Blob_Trees.Success => return Success;
         when Blob_Trees.Failure => return Failure;
         when Blob_Trees.Error   => return Error;
      end case;
   end To_State;


   procedure Search
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type)
   is
      S : BTrees.State_Type;
      V : Types.Values.Bounded.String_Type;
   begin
      BTrees.Search(Map.Tree, Key, V, S);
      State := To_State(S);
      if State = Success then
         Value := From_Bounded(V);
      end if;
   end Search;


   procedure Minimum
     (Map      : in out Map_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type)
   is
      S : BTrees.State_Type;
      V : Types.Values.Bounded.String_Type;
   begin
      BTrees.Minimum(Map.Tree, Key, V, S);
      State := To_State(S);
      if State = Success then
         Value := From_Bounded(V);
      end if;
   end Minimum;


   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type) is
   begin
      Insert(Map, Key, Value, Default_Allow_Duplicates, State);
   end Insert;


   procedure Insert
     (Map              : in out Map_Type;
      Key              : in     Key_Type;
      Value            : in     Value_Type'Class;
      Allow_Duplicates : in     Boolean;
      State            :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Insert(Map.Tree, Key, To_Bounded(Value),
                    Allow_Duplicates, S);
      State := To_State(S);
   end Insert;


   procedure Delete
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type)
   is
      S : BTrees.State_Type;
      V : Types.Values.Bounded.String_Type;
   begin
      BTrees.Delete(Map.Tree, Key, V, S);
      State := To_State(S);
      if State = Success then
         Value := From_Bounded(V);
      end if;
   end Delete;


   function Positive_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Bound => BTrees.Positive_Infinity_Bound);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Bound => BTrees.Negative_Infinity_Bound);
   end Negative_Infinity_Bound;


   function New_Bound
     (Map        : Map_Type;
      Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type
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
      return Bound_Type'(Bound => BTrees.New_Bound(C, Key));
   end New_Bound;


   function New_Cursor
     (Map         : Map_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type
   is
      pragma Assert (Map.Short = Lower_Bound.Short);
      pragma Assert (Map.Short = Upper_Bound.Short);
   begin
      return Cursor_Type'(Trees.New_Cursor(Map.Tree,
                                           Thread_Safe,
                                           Lower_Bound.Bound,
                                           Upper_Bound.Bound));
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      BTrees.Set_Thread_Safety(Cursor.Cursor, Enabled);
   end Set_Thread_Safety;


   procedure Finalize_Cursor
     (Cursor : in out Cursor_Type) is
   begin
      BTrees.Finalize_Cursor(Cursor.Tree, Cursor.Cursor);
   end Finalize_Cursor;


   procedure Pause
     (Cursor : in out Cursor_Type) is
   begin
      BTrees.Pause(Cursor.Tree, Cursor.Cursor);
   end Pause;


   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
      S : BTrees.State_Type;
      V : Types.Values.Bounded.String_Type;
   begin
      BTrees.Next(Cursor.Tree, Cursor.Cursor, Key, V, S);
      State := To_State(S);
      if State = Success then
         Value := From_Bounded(V);
      end if;
   end Next;


   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
      S : BTrees.State_Type;
      V : Types.Values.Bounded.String_Type;
   begin
      BTrees.Delete(Cursor.Tree, Cursor.Cursor, Key, V, S);
      State := To_State(S);
      if State = Success then
         Value := From_Bounded(V);
      end if;
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type) is
   begin
      BTrees.Count(Map.Tree, Count);
   end Count;


   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type)
   is
      S : BTrees.State_Type;
   begin
      BTrees.Reorganize(Map.Tree, S);
      State := To_State(S);
   end Reorganize;

end DB.Maps.Bounded;

