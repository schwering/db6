-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Tables.Maps is

   function New_Map
     (Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type)
      return Map_Type
   is
      use type Blocks.Size_Type;
   begin
      if Max_Key_Size <= BTrees.Max_Key_Size(Max_Value_Size) then
         return Map_Type'(Short => True, others => <>);
      else
         return Map_Type'(Short => False, others => <>);
      end if;
   end New_Map;


   procedure Create
     (ID             : in String;
      Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type)
   is
      use type Blocks.Size_Type;
   begin
      if Max_Key_Size <= BTrees.Max_Key_Size(Max_Value_Size) then
         BTrees.Create(ID);
      else
         Blob_Trees.Create(ID);
      end if;
   end Create;


   procedure Initialize
     (Map : out Map_Type;
      ID  : in  String) is
   begin
      if Map.Short then
         BTrees.Initialize(Map.Short_Tree, ID);
      else
         Blob_Trees.Initialize(Map.Long_Tree, ID);
      end if;
   end Initialize;


   procedure Finalize
     (Map : in out Map_Type) is
   begin
      if Map.Short then
         BTrees.Finalize(Map.Short_Tree);
      else
         Blob_Trees.Finalize(Map.Long_Tree);
      end if;
   end Finalize;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type is
   begin
      if Map.Short then
         return BTrees.Max_Key_Size(Max_Value_Size);
      else
         return Blob_Trees.Max_Key_Size;
      end if;
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
      State    :    out State_Type) is
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
            V : Types.Values.Bounded.String_Type;
         begin
            BTrees.Search(Map.Short_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Bounded(V);
            end if;
         end;
      else
         declare
            S : Blob_Trees.State_Type;
            V : Types.Values.Unbounded.String_Type;
         begin
            Blob_Trees.Search(Map.Long_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Unbounded(V);
            end if;
         end;
      end if;
   end Search;


   procedure Minimum
     (Map      : in out Map_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type) is
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
            V : Types.Values.Bounded.String_Type;
         begin
            BTrees.Minimum(Map.Short_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Bounded(V);
            end if;
         end;
      else
         declare
            S : Blob_Trees.State_Type;
            V : Types.Values.Unbounded.String_Type;
         begin
            Blob_Trees.Minimum(Map.Long_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Unbounded(V);
            end if;
         end;
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
      State            :    out State_Type) is
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
         begin
            BTrees.Insert(Map.Short_Tree, Key, To_Bounded(Value),
                          Allow_Duplicates, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.State_Type;
         begin
            Blob_Trees.Insert(Map.Long_Tree, Key, To_Unbounded(Value),
                              Allow_Duplicates, S);
            State := To_State(S);
         end;
      end if;
   end Insert;


   procedure Delete
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type) is
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
            V : Types.Values.Bounded.String_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Bounded(V);
            end if;
         end;
      else
         declare
            S : Blob_Trees.State_Type;
            V : Types.Values.Unbounded.String_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Unbounded(V);
            end if;
         end;
      end if;
   end Delete;


   function Positive_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type is
   begin
      if Map.Short then
         return Bound_Type'(Short       => True,
                            Short_Bound => BTrees.Positive_Infinity_Bound);
      else
         return Bound_Type'(Short      => False,
                            Long_Bound => Blob_Trees.Positive_Infinity_Bound);
      end if;
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type is
   begin
      if Map.Short then
         return Bound_Type'(Short       => True,
                            Short_Bound => BTrees.Negative_Infinity_Bound);
      else
         return Bound_Type'(Short      => False,
                            Long_Bound => Blob_Trees.Negative_Infinity_Bound);
      end if;
   end Negative_Infinity_Bound;


   function New_Bound
     (Map        : Map_Type;
      Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type is
   begin
      if Map.Short then
         declare
            C : BTrees.Comparison_Type;
         begin
            case Comparison is
               when Less             => C := BTrees.Less;
               when Less_Or_Equal    => C := BTrees.Less_Or_Equal;
               when Equal            => C := BTrees.Equal;
               when Greater_Or_Equal => C := BTrees.Greater_Or_Equal;
               when Greater          => C := BTrees.Greater;
            end case;
            return Bound_Type'(Short       => True,
                               Short_Bound => BTrees.New_Bound(C, Key));
         end;
      else
         declare
            C : Blob_Trees.Comparison_Type;
         begin
            case Comparison is
               when Less             => C := Blob_Trees.Less;
               when Less_Or_Equal    => C := Blob_Trees.Less_Or_Equal;
               when Equal            => C := Blob_Trees.Equal;
               when Greater_Or_Equal => C := Blob_Trees.Greater_Or_Equal;
               when Greater          => C := Blob_Trees.Greater;
            end case;
            return Bound_Type'(Short      => False,
                               Long_Bound => Blob_Trees.New_Bound(C, Key));
         end;
      end if;
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
      if Map.Short then
         return (Short        => True,
                 Short_Cursor => BTrees.New_Cursor(Map.Short_Tree,
                                                   Thread_Safe,
                                                   Lower_Bound.Short_Bound,
                                                   Upper_Bound.Short_Bound));
      else
         return (Short       => False,
                 Long_Cursor => Blob_Trees.New_Cursor(Map.Long_Tree,
                                                      Thread_Safe,
                                                      Lower_Bound.Long_Bound,
                                                      Upper_Bound.Long_Bound));
      end if;
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      if Cursor.Short then
         BTrees.Set_Thread_Safety(Cursor.Short_Cursor, Enabled);
      else
         Blob_Trees.Set_Thread_Safety(Cursor.Long_Cursor, Enabled);
      end if;
   end Set_Thread_Safety;


   procedure Finalize_Cursor
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Cursor.Short then
         BTrees.Finalize_Cursor(Map.Short_Tree, Cursor.Short_Cursor);
      else
         Blob_Trees.Finalize_Cursor(Map.Long_Tree, Cursor.Long_Cursor);
      end if;
   end Finalize_Cursor;


   procedure Pause
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Cursor.Short then
         BTrees.Pause(Map.Short_Tree, Cursor.Short_Cursor);
      else
         Blob_Trees.Pause(Map.Long_Tree, Cursor.Long_Cursor);
      end if;
   end Pause;


   procedure Next
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
            V : Types.Values.Bounded.String_Type;
         begin
            BTrees.Next(Map.Short_Tree, Cursor.Short_Cursor, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Bounded(V);
            end if;
         end;
      else
         declare
            S : Blob_Trees.State_Type;
            V : Types.Values.Unbounded.String_Type;
         begin
            Blob_Trees.Next(Map.Long_Tree, Cursor.Long_Cursor, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Unbounded(V);
            end if;
         end;
      end if;
   end Next;


   procedure Delete
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
            V : Types.Values.Bounded.String_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Cursor.Short_Cursor, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Bounded(V);
            end if;
         end;
      else
         declare
            S : Blob_Trees.State_Type;
            V : Types.Values.Unbounded.String_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Cursor.Long_Cursor, Key, V, S);
            State := To_State(S);
            if State = Success then
               Value := From_Unbounded(V);
            end if;
         end;
      end if;
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type) is
   begin
      if Map.Short then
         BTrees.Count(Map.Short_Tree, Count);
      else
         Blob_Trees.Count(Map.Long_Tree, Count);
      end if;
   end Count;


   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type) is
   begin
      if Map.Short then
         declare
            S : BTrees.State_Type;
         begin
            BTrees.Reorganize(Map.Short_Tree, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.State_Type;
         begin
            Blob_Trees.Reorganize(Map.Long_Tree, S);
            State := To_State(S);
         end;
      end if;
   end Reorganize;

end DB.Tables.Maps;

