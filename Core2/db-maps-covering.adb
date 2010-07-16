-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Maps.Covering is

   type Cover_Type is array (Positive range <>) of Boolean;

   function Cover
     (R   : RE.Regexp_Type; 
      Map : Map_Type'Class)
      return Cover_Type
   is
      pragma Precondition (Map.Slices /= null);
      Slices          : Slice_Array_Type renames Map.Slices.all;
      Cover           : Cover_Type (Slices'Range) := (others => False);
      Already_Covered : RE.Regexp_Type := RE.Empty_Regexp;
   begin
      for I in Slices'Range loop
         declare
            Guard       : RE.Regexp_Type renames Slices (I).Guard;
            Max_Benefit : constant RE.Regexp_Type := RE.Intersection (Guard, R);
            -- The maximum benefit of having the slice in the cover.
            Has_Benefit : constant Boolean :=
               not RE.Is_Subset (Max_Benefit, Already_Covered);
            -- If Max_Benefit is a subset of the slices we have already covered,
            -- there is in fact no benefit.
         begin
            if Has_Benefit then
               Cover (I)       := True;
               Already_Covered := RE.Union (Already_Covered, Guard);
            end if;
         end;
      end loop;
      return Cover;
   end Cover;


   function Cover (Regexp : String; Map : Map_Type'Class) return Cover_Type
   is
      R : RE.Regexp_Type := RE.Compile (Regexp);
   begin
      return Cover (R, Map);
   end Cover;


   function Total_Cover (Map : Map_Type'Class) return Cover_Type
   is
      Union : RE.Regexp_Type := RE.Empty_Regexp;
   begin
      for I in Map.Slices'Range loop
         Union := RE.Union (Union, Map.Slices (I).Guard);
      end loop;
      return Cover (Union, Map);
   end Total_Cover;


   function New_Map (Allow_Duplicates : in Boolean) return Map_Type is
   begin
      return Map_Type' (AF.Limited_Controlled with
                        Allow_Duplicates => Allow_Duplicates,
                        others => <>);
   end New_Map;


   procedure Add_Slice
     (Map   : in out Map_Type;
      Guard : in     String;
      Impl  : in     String;
      ID    : in     String)
   is
      Pred : Node_Ref_Type := Map.Config;
      Node : constant Node_Ref_Type :=
        new Node_Type' (Guard_Length => Guard'Length,
                        Impl_Length  => ID'Length,
                        ID_Length    => ID'Length,
                        Guard        => Guard,
                        Impl         => Impl,
                        ID           => ID,
                        Next         => null);
   begin
      if Pred = null then
         Map.Config := Node;
      else
         while Pred.Next /= null loop
            Pred := Pred.Next;
         end loop;
         Pred.Next := Node;
      end if;
   end Add_Slice;


   procedure Init_Slices (Map : in out Map_Type)
   is
      --pragma Precondition (Map.Config /= null);
      --pragma Postcondition (Map.Slices /= null);
      Count : Natural := 0;
   begin
      declare
         Node : Node_Ref_Type := Map.Config;
      begin
         while Node /= null loop
            Count := Count + 1;
            Node  := Node.Next;
         end loop;
      end;
      Map.Slices := new Slice_Array_Type (1 .. Count);
   end Init_Slices;


   procedure Create (Map : in out Map_Type; ID : in String)
   is
      pragma Unreferenced (ID);
      pragma Precondition (not Map.Initialized);
   begin
      Init_Slices (Map);
      declare
         Node : Node_Ref_Type := Map.Config;
         I    : Natural := 1;
      begin
         while Node /= null loop
            Map.Slices (I).Guard := RE.Compile (Node.Guard);
            Map.Slices (I).Map :=
               new Base_Map_Type' (Maps.New_Map (Node.Impl,
                                                 Map.Allow_Duplicates));
            Maps.Create (Map.Slices (I).Map.all, Node.ID);
            Node := Node.Next;
            I := I + 1;
         end loop;
      end;
      Map.Initialized := True;
   end Create;


   procedure Create_Temporary (Map : in out Map_Type; ID : in String)
   is
      pragma Unreferenced (ID);
      pragma Precondition (not Map.Initialized);
   begin
      Map.Initialized := True;
   end Create_Temporary;


   procedure Open (Map : in out Map_Type; ID : in String)
   is
      pragma Unreferenced (ID);
      pragma Precondition (not Map.Initialized);
   begin
      Map.Initialized := True;
   end Open;


   procedure Finalize (Map : in out Map_Type)
   is
      procedure Free_Base_Map_Array is new Ada.Unchecked_Deallocation
        (Slice_Array_Type, Slice_Array_Ref_Type);
      procedure Free_Base_Map is new Ada.Unchecked_Deallocation
        (Base_Map_Type, Base_Map_Ref_Type);
      procedure Free_Node is new Ada.Unchecked_Deallocation
        (Node_Type, Node_Ref_Type);
   begin
      if Map.Initialized then
         for I in Map.Slices'Range loop
            Map.Slices (I).Map.Finalize;
            Free_Base_Map (Map.Slices (I).Map);
         end loop;
         Free_Base_Map_Array (Map.Slices);
         declare
            Current : Node_Ref_Type := Map.Config;
            Next    : Node_Ref_Type;
         begin
            while Current /= null loop
               Next := Current.Next;
               Free_Node (Current);
               Current := Next;
            end loop;
            Map.Config := null;
         end;
         Map.Initialized := False;
      end if;
   end Finalize;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type
   is
      Max_Key_Size : Blocks.Size_Type := Blocks.Size_Type'Last;
   begin
      for I in Map.Slices'Range loop
         Max_Key_Size := Blocks.Size_Type'Min (Max_Key_Size,
                                               Map.Slices (I).Map.Max_Key_Size
                                                 (Max_Value_Size));
      end loop;
      return Max_Key_Size;
   end Max_Key_Size;


   function Matches
     (Col   : String;
      Guard : RE.Regexp_Type)
      return Boolean
   is
      pragma Inline (Matches);
   begin
      return RE.Match (Col, Guard);
   end Matches;


   function Col_Image (Key : Key_Type) return String
   is
      pragma Inline (Col_Image);
   begin
      return String (Types.Keys.Columns.To_Buffer (Key.Column));
   end Col_Image;


   function Matches
     (Key   : Key_Type;
      Guard : RE.Regexp_Type)
      return Boolean
   is
      pragma Inline (Matches);
   begin
      return Matches (Col_Image (Key), Guard);
   end Matches;


   overriding
   function Contains 
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean
   is
      C : constant String := Col_Image (Key);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            return Map.Slices (I).Map.Contains (Key);
         end if;
      end loop;
      return False;
   end Contains;


   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type)
   is
      C : constant String := Col_Image (Key);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Map.Slices (I).Map.Search (Key, Value, State);
            return;
         end if;
      end loop;
   end Search;


   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type) is
   begin
      if Map.Slices'Length = 0 then
         State := Failure;
         return;
      end if;
      Map.Slices (Map.Slices'First).Map.Search_Minimum (Key, Value, State);
      for I in Map.Slices'First + 1 .. Map.Slices'Last loop
         declare
            use type Utils.Comparison_Result_Type;
            This_Key   : Key_Type;
            This_Value : Value_Type'Class := Value; -- Value is the prototype
            This_State : State_Type;
         begin
            Map.Slices (I).Map.Search_Minimum
              (This_Key, This_Value, This_State);
            if This_State = Success and then
               Keys.Compare (This_Key, Key) = Utils.Less then
               Key   := This_Key;
               Value := This_Value;
               State := This_State;
            end if;
         end;
      end loop;
   end Search_Minimum;


   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      C : constant String := Col_Image (Key);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Map.Slices (I).Map.Insert (Key, Value, State);
         end if;
      end loop;
   end Insert;


   procedure Insert
     (Map              : in out Map_Type;
      Key              : in     Key_Type;
      Value            : in     Value_Type'Class;
      Allow_Duplicates : in     Boolean;
      State            :    out State_Type)
   is
      C : constant String := Col_Image (Key);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Map.Slices (I).Map.Insert
               (Key, Value, Allow_Duplicates, State);
         end if;
      end loop;
   end Insert;


   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type)
   is
      C : constant String := Col_Image (Key);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Map.Slices (I).Map.Delete (Key, Value, State);
         end if;
      end loop;
   end Delete;


   function New_Cursor
     (Map         : Map_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Maps.Cursor_Type'Class
   is
      pragma Precondition (Map.Initialized);
   begin
      return Cursor_Type' (Maps.Cursor_Type with others => <>);
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      null;
   end Set_Thread_Safety;


   procedure Pause
     (Cursor : in out Cursor_Type) is
   begin
      null;
   end Pause;


   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
   begin
      null;
   end Next;


   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type)
   is
   begin
      null;
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type)
   is
      C : Count_Type;
   begin
      Count := 0;
      for I in Map.Slices'Range loop
         Map.Slices (I).Map.Count (C);
         Count := Count + C;
      end loop;
   end Count;


   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type) is
   begin
      for I in Map.Slices'Range loop
         Map.Slices (I).Map.Reorganize (State);
         exit when State /= Success;
      end loop;
   end Reorganize;


   overriding
   procedure Check
     (Map : in out Map_Type) is
   begin
      for I in Map.Slices'Range loop
         Map.Slices (I).Map.Check;
      end loop;
   end Check;


   overriding
   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type)) is
   begin
      for I in Map.Slices'Range loop
         Map.Slices (I).Map.Stats (Emit);
      end loop;
   end Stats;


end DB.Maps.Covering;

