-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

with Input_Sources;
with Sax.Attributes;
with Sax.Readers;
with Unicode;
with Unicode.CES;

with DB.Blocks.Gen_ASCII_Layer;
with DB.Blocks.Local_IO;

package body DB.Maps.Covering is

   function Cover
     (R      : RE.Regexp_Type;
      Slices : Slice_Array_Type)
      return Cover_Type
   is
      type Booleans_Type is array (Positive range <>) of Boolean;

      function Booleans_To_Cover
         (Take : Booleans_Type;
          Size : Natural)
          return Cover_Type
      is
         Cover : Cover_Type (1 .. Cover_Index_Type (Size));
         J     : Cover_Index_Type := 0;
      begin
         for I in Take'Range loop
            if Take (I) then
               J         := J + 1;
               Cover (J) := I;
            end if;
         end loop;
         pragma Assert (Natural (J) = Size);
         return Cover;
      end Booleans_To_Cover;

      Take            : Booleans_Type (Slices'Range) := (others => False);
      Size            : Natural                      := 0;
      Already_Covered : RE.Regexp_Type               := RE.Empty_Regexp;
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
               Take (I)        := True;
               Size            := Size + 1;
               Already_Covered := RE.Union (Already_Covered, Guard);
            end if;
         end;
      end loop;
      pragma Assert (RE.Is_Subset (R, Already_Covered));
      return Booleans_To_Cover (Take, Size);
   end Cover;


   function Cover (Regexp : String; Map : Map_Type'Class) return Cover_Type
   is
      R : constant RE.Regexp_Type := RE.Compile (Regexp);
   begin
      return Cover (R, Map.Slices.all);
   end Cover;


   function Total_Cover (Map : Map_Type'Class) return Cover_Type
   is
      Union : RE.Regexp_Type := RE.Empty_Regexp;
   begin
      for I in Map.Slices'Range loop
         Union := RE.Union (Union, Map.Slices (I).Guard);
      end loop;
      return Cover (Union, Map.Slices.all);
   end Total_Cover;


   package XML is
      type File_Type is limited private;

      procedure Create (ID : in String; File : in out File_Type);
      procedure Open (ID : in String; File : in out File_Type);
      procedure Close (File : in out File_Type);
      procedure Write
        (File             : in out File_Type;
         Allow_Duplicates : in     Boolean;
         Config_Head      : in     Node_Ref_Type);
      procedure Read
        (File             : in out File_Type;
         Allow_Duplicates :    out Boolean;
         Config_Head      :    out Node_Ref_Type);

   private
      package ASCII_IO is new Blocks.Gen_ASCII_Layer
        (Blocks.Local_IO.IO_Signature);

      type File_Type is limited new Input_Sources.Input_Source with
         record
            File : ASCII_IO.File_Type;
            Char : Unicode.Unicode_Char;
            EOF  : Boolean;
         end record;

      overriding
      procedure Next_Char
        (From : in out File_Type;
         C    :    out Unicode.Unicode_Char);

      overriding
      function EOF (From : File_Type) return Boolean;
   end XML;


   package body XML is

      procedure Lookahead (File : in out File_Type)
      is
         C : Character;
      begin
         ASCII_IO.Read (File.File, C, File.EOF);
         File.Char := Unicode.To_Unicode (C);
      end Lookahead;


      procedure Create (ID : in String; File : in out File_Type) is
      begin
         ASCII_IO.Create (ID, File.File);
      end;


      procedure Open (ID : in String; File : in out File_Type) is
      begin
         ASCII_IO.Open (ID, File.File);
         Lookahead (File);
      end;


      overriding
      procedure Close (File : in out File_Type) is
      begin
         ASCII_IO.Close (File.File);
      end Close;


      procedure Write (File : in out File_Type; S : in String) is
      begin
         for I in S'Range loop
            ASCII_IO.Write (File.File, S (I));
         end loop;
      end Write;


      procedure Write
        (File             : in out File_Type;
         Allow_Duplicates : in     Boolean;
         Config_Head      : in     Node_Ref_Type)
      is
         N : Node_Ref_Type := Config_Head;
      begin
         Write (File, "<table ");
         Write (File, "allow_duplicates=""" &
                      Boolean'Image (Allow_Duplicates) &
                      """>");
         while N /= null loop
            Write (File, "<slice ");
            Write (File, "guard="""& N.Guard &""" ");
            Write (File, "impl="""& N.Impl &""" ");
            Write (File, "id="""& N.ID &""" ");
            Write (File, "/>");
            N := N.Next;
         end loop;
         Write (File, "</table>");
      end Write;


      procedure Read
        (File             : in out File_Type;
         Allow_Duplicates :    out Boolean;
         Config_Head      :    out Node_Ref_Type)
      is
         type String_Ref_Type is access String;

         procedure Free is new Ada.Unchecked_Deallocation
           (String, String_Ref_Type);

         type Reader_Type is new Sax.Readers.Reader with
            record
               Current : Node_Ref_Type   := null;
               Guard   : String_Ref_Type := null;
               Impl    : String_Ref_Type := null;
               ID      : String_Ref_Type := null;
            end record;

         procedure Start_Element
           (Handler       : in out Reader_Type;
            Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
            Local_Name    : in     Unicode.CES.Byte_Sequence := "";
            QName         : in     Unicode.CES.Byte_Sequence := "";
            Atts          : in     Sax.Attributes.Attributes'Class)
         is
            pragma Unreferenced (Namespace_URI);
            pragma Unreferenced (Local_Name);
         begin
            if QName = "table" then
               for I in 0 .. Sax.Attributes.Get_Length (Atts) - 1 loop
                  declare
                     K : constant String := Sax.Attributes.Get_QName (Atts, I);
                     V : constant String := Sax.Attributes.Get_Value (Atts, I);
                  begin
                     if K = "allow_duplicates" then
                        Allow_Duplicates := Boolean'Value (V);
                     else
                        raise IO_Error;
                     end if;
                  end;
               end loop;
            elsif QName = "slice" then
               for I in 0 .. Sax.Attributes.Get_Length (Atts) - 1 loop
                  declare
                     P : constant Node_Ref_Type := Handler.Current;
                     K : constant String := Sax.Attributes.Get_QName (Atts, I);
                     V : constant String := Sax.Attributes.Get_Value (Atts, I);
                  begin
                     if K = "guard" then
                        pragma Assert (Handler.Guard = null);
                        Handler.Guard := new String'(V);
                     elsif K = "impl" then
                        pragma Assert (Handler.Impl = null);
                        Handler.Impl := new String'(V);
                     elsif K = "id" then
                        pragma Assert (Handler.ID = null);
                        Handler.ID := new String'(V);
                     else
                        raise IO_Error;
                     end if;
                     if Handler.Guard /= null and
                        Handler.Impl /= null and
                        Handler.ID /= null then
                        Handler.Current := new Node_Type'
                          (Guard_Length => Handler.Guard'Length,
                           Impl_Length  => Handler.Impl'Length,
                           ID_Length    => Handler.ID'Length,
                           Guard        => Handler.Guard.all,
                           Impl         => Handler.Impl.all,
                           ID           => Handler.ID.all,
                           Next         => null);
                        Free (Handler.Guard);
                        Free (Handler.Impl);
                        Free (Handler.ID);

                        if P = null then -- It's the first element.
                           Config_Head := Handler.Current;
                        else
                           P.Next := Handler.Current;
                        end if;
                     end if;
                  end;
               end loop;
            end if;
         end Start_Element;

         Reader : Reader_Type;
      begin
         Reader.Parse (File);
      end Read;


      overriding
      procedure Next_Char
        (From : in out File_Type;
         C    :    out Unicode.Unicode_Char) is
      begin
         C := From.Char;
         Lookahead (From);
      end;


      overriding
      function EOF (From : File_Type) return Boolean is
      begin
         return From.EOF;
      end;

   end XML;


   function New_Map
     (Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type is
   begin
      return Map_Type'(Maps.Map_Type with
                       Initialized      => False,
                       Allow_Duplicates => Allow_Duplicates,
                       others           => <>);
   end New_Map;


   procedure Add_Slice
     (Map   : in out Map_Type;
      Guard : in     String;
      Impl  : in     Implementation_Type;
      ID    : in     String)
   is
      Pred     : Node_Ref_Type := Map.Config;
      Impl_Str : constant String := Implementation_Type'Image (Impl);
      Node     : constant Node_Ref_Type :=
        new Node_Type'(Guard_Length => Guard'Length,
                       Impl_Length  => Impl_Str'Length,
                       ID_Length    => ID'Length,
                       Guard        => Guard,
                       Impl         => Impl_Str,
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


   generic
      with procedure Init_Sub_Map
         (Map : in out Maps.Map_Type'Class;
          ID  : in     String);
   procedure Gen_Init_Map
     (Map : in out Map_Type);

   procedure Gen_Init_Map
     (Map : in out Map_Type)
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

      declare
         Node : Node_Ref_Type := Map.Config;
      begin
         for I in Map.Slices'Range loop
            Map.Slices (I).Guard := RE.Compile (Node.Guard);
            declare
               Impl    : constant Implementation_Type :=
                  Implementation_Type'Value (Node.Impl);
               Dupes   : constant Boolean := Map.Allow_Duplicates;
               Map_Ref : constant Base_Map_Ref_Type :=
                  new Base_Map_Type'(Maps.New_Map (Impl, Dupes));
            begin
               Map.Slices (I).Map := Map_Ref;
            end;
            Init_Sub_Map (Map.Slices (I).Map.all, Node.ID);
            Node := Node.Next;
         end loop;
         pragma Assert (Node = null);
      end;

      Map.Cover := new Cover_Type'(Total_Cover (Map));
      Map.Initialized := True;
   end Gen_Init_Map;


   procedure Create (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);

      procedure Create (Map : in out Maps.Map_Type'Class; ID : in String) is
      begin
         Maps.Create (Map, ID);
      end Create;

      procedure Init_Map is new Gen_Init_Map (Create);

      XML_File : XML.File_Type;
   begin
      XML.Create (ID, XML_File);
      XML.Write (XML_File, Map.Allow_Duplicates, Map.Config);
      XML.Close (XML_File);
      Init_Map (Map);
   end Create;


   procedure Create_Temporary (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);
      pragma Unreferenced (ID);

      procedure Create (Map : in out Maps.Map_Type'Class; ID : in String) is
      begin
         Map.Create_Temporary (ID);
      end Create;

      procedure Init_Map is new Gen_Init_Map (Create);
   begin
      Init_Map (Map);
   end Create_Temporary;


   procedure Open (Map : in out Map_Type; ID : in String)
   is
      pragma Precondition (not Map.Initialized);

      procedure Open (Map : in out Maps.Map_Type'Class; ID : in String) is
      begin
         Map.Open (ID);
      end Open;

      procedure Init_Map is new Gen_Init_Map (Open);

      XML_File : XML.File_Type;
   begin
      XML.Open (ID, XML_File);
      XML.Read (XML_File, Map.Allow_Duplicates, Map.Config);
      XML.Close (XML_File);
      Init_Map (Map);
   exception
      when others =>
         XML.Close (XML_File);
         raise;
   end Open;


   procedure Finalize (Map : in out Map_Type)
   is
      pragma Precondition (Map.Initialized = (Map.Slices /= null));
      pragma Precondition (Map.Initialized = (Map.Cover /= null));
      procedure Free is new Ada.Unchecked_Deallocation
        (Slice_Array_Type, Slice_Array_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Base_Map_Type, Base_Map_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Type, Node_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Cover_Type, Cover_Ref_Type);
   begin
      if Map.Initialized then
         -- Free Config
         while Map.Config /= null loop
            declare
               Next : Node_Ref_Type;
            begin
               Next := Map.Config.Next;
               Free (Map.Config);
               Map.Config := Next;
            end;
         end loop;
         Map.Config := null;

         -- Free Slices and Maps
         for I in Map.Slices'Range loop
            Map.Slices (I).Map.Finalize;
            Free (Map.Slices (I).Map);
         end loop;
         Free (Map.Slices);

         -- Free Cover
         Free (Map.Cover);

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
   renames RE.Match;


   overriding
   function Contains
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean
   is
      C : constant String := Column_To_String (Key.Column);
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
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is
      C : constant String := Column_To_String (Key.Column);
   begin
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Map.Slices (I).Map.Search (Key, Value, State);
            return;
         end if;
      end loop;
      raise Map_Error;
   end Search;


   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type) is
   begin
      if Map.Cover'Length = 0 then
         raise Map_Error;
      end if;
      Map.Slices (Map.Cover (Map.Cover'First)).Map.Search_Minimum
        (Key, Value, State);
      for I in Map.Cover'First + 1 .. Map.Cover'Last loop
         declare
            use type Utils.Comparison_Result_Type;
            This_Key   : Key_Type;
            This_Value : Value_Wrapper_Type;
            This_State : State_Type;
         begin
            Map.Slices (Map.Cover (I)).Map.Search_Minimum
              (This_Key, This_Value, This_State);
            if This_State = Success and then
               Keys.Compare (This_Key, Key) = Utils.Less then
               Key   := This_Key;
               Value := This_Value;
               State := This_State;
            end if;
         end;
      end loop;
      raise Map_Error;
   end Search_Minimum;


   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Insert (Key, Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Insert;


   procedure Insert
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Insert (Key, Value, Existed, Old_Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Insert;


   procedure Replace
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Replace (Key, Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Replace;


   procedure Replace
     (Map       : in out Map_Type;
      Key       : in     Key_Type;
      Value     : in     Value_Type'Class;
      Existed   :    out Boolean;
      Old_Value :    out Value_Wrapper_Type;
      State     :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Replace (Key, Value, Existed, Old_Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Replace;


   procedure Append
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Append (Key, Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Append;


   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Wrapper_Type;
      State :    out State_Type)
   is
      C       : constant String := Column_To_String (Key.Column);
      Matched : Boolean := False;
   begin
      State := Failure;
      for I in Map.Slices'Range loop
         if Matches (C, Map.Slices (I).Guard) then
            Matched := True;
            Map.Slices (I).Map.Delete (Key, Value, State);
            exit when State /= Success;
         end if;
      end loop;
      if not Matched then
         raise Map_Error;
      end if;
   end Delete;


   procedure Free_Heap_Item (Item : in out Heap_Item_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Value_Type'Class, Value_Ref_Type);
   begin
      if Item.Value /= null then
         Free (Item.Value);
      end if;
   end Free_Heap_Item;


   procedure Free_Heap (Heap : in out Heap_Ref_Type)
   is
      pragma Precondition (Heap /= null);
      pragma Precondition (Heap = null);

      procedure Free is new Ada.Unchecked_Deallocation
        (Heaps.Heap_Type, Heap_Ref_Type);
   begin
      Heaps.Clear (Heap.all, Free_Heap_Item'Access);
      Free (Heap);
   end Free_Heap;


   function New_Cursor
     (Map           : Map_Type;
      Cover         : Cover_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String)
      return Maps.Cursor_Type'Class
   is
      pragma Precondition (Map.Initialized);

      procedure Free is new Ada.Unchecked_Deallocation
        (Base_Cursor_Ref_Array_Type, Base_Cursor_Ref_Array_Ref_Type);

      Cursors : Base_Cursor_Ref_Array_Ref_Type := null;
      Heap    : Heap_Ref_Type                  := null;
   begin
      Cursors := new Base_Cursor_Ref_Array_Type (1 .. Cover'Length);
      Heap    := new Heaps.Heap_Type'(Heaps.New_Heap (Cover'Length));
      for I in Cover'Range loop
         Cursors (Cover (I)) := new Base_Cursor_Type'
           (Map.Slices (Cover (I)).Map.New_Cursor (Thread_Safe,
                                                   Lower_Bound,
                                                   Upper_Bound,
                                                   Column_Regexp));
      end loop;
      return Cursor_Type'(Maps.Cursor_Type with
                          Initialized => True,
                          Map         => Map.Self,
                          Cursors     => Cursors,
                          Heap        => Heap,
                          Heap_Filled => False);
   exception
      when others =>
         if Cursors /= null then
            Free (Cursors);
         end if;
         if Heap /= null then
            Free_Heap (Heap);
         end if;
         raise;
   end New_Cursor;


   function New_Cursor
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Maps.Cursor_Type'Class is
   begin
      if Column_Regexp = "" then
         return New_Cursor
           (Map, Map.Cover.all, Thread_Safe, Lower_Bound, Upper_Bound,
            Column_Regexp);
      else
         declare
            C : constant Cover_Type :=
              Cover (RE.Compile (Column_Regexp), Map.Slices.all);
         begin
            return New_Cursor
              (Map, C, Thread_Safe, Lower_Bound, Upper_Bound, Column_Regexp);
         end;
      end if;
   end New_Cursor;


   overriding
   procedure Finalize
     (Cursor : in out Cursor_Type)
   is
      pragma Precondition (Cursor.Initialized = (Cursor.Cursors /= null));
      pragma Precondition (Cursor.Initialized = (Cursor.Heap /= null));

      procedure Free is new Ada.Unchecked_Deallocation
        (Base_Cursor_Ref_Array_Type, Base_Cursor_Ref_Array_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Heaps.Heap_Type, Heap_Ref_Type);
   begin
      if Cursor.Initialized then
         Cursor.Initialized := False;
         if Cursor.Cursors /= null then
            Free (Cursor.Cursors);
         end if;
         if Cursor.Heap /= null then
            Free (Cursor.Heap);
         end if;
      end if;
   end Finalize;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      for I in Cursor.Cursors'Range loop
         Cursor.Cursors (I).Set_Thread_Safety (Enabled);
      end loop;
   end Set_Thread_Safety;


   procedure Pause
     (Cursor : in out Cursor_Type) is
   begin
      for I in Cursor.Cursors'Range loop
         Cursor.Cursors (I).Pause;
      end loop;
   end Pause;


   function "<" (Left, Right : Heap_Item_Type) return Boolean
   is
      use type Key_Type;
   begin
      return Left.Key < Right.Key;
   end "<";


   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Wrapper_Type;
      State  :    out State_Type)
   is
      pragma Precondition (Cursor.Initialized);
      pragma Precondition (Cursor.Cursors /= null);
      pragma Precondition (Cursor.Heap /= null);

      -- The idea is to populate the heap with at most one item per subcursor.
      -- When one item is taken from the heap, a new one is inserted from the
      -- owning subcursor, if the subcursor has a next value.
      -- This guarantees that if a subcursor has no next value, Next isn't
      -- invoked anymore on this subcursor.

      procedure Insert_From_Sub_Cursor (Sub_Cursor : in Base_Cursor_Ref_Type)
      is
         K : Key_Type;
         V : Value_Wrapper_Type := Value;
         S : State_Type;
      begin
         Sub_Cursor.Next (K, V, S);
         if S = Success then
            declare
               V_Ref : constant Value_Ref_Type :=
                 new Value_Type'Class'(V.Ref.all);
               Item  : constant Heap_Item_Type :=
                 Heap_Item_Type'(K, V_Ref, Sub_Cursor);
            begin
               Heaps.Insert (Cursor.Heap.all, Item);
            end;
         end if;
      end Insert_From_Sub_Cursor;

   begin
      if not Cursor.Heap_Filled then
         for I in Cursor.Cursors'Range loop
            Insert_From_Sub_Cursor (Cursor.Cursors (I));
         end loop;
         Cursor.Heap_Filled := True;
      end if;

      if Heaps.Is_Empty (Cursor.Heap.all) then
         State := Failure;
         return;
      end if;

      declare
         Item : Heap_Item_Type;
      begin
         Heaps.Extract_Min (Cursor.Heap.all, Item);
         Key   := Item.Key;
         Value := New_Value_Wrapper (Item.Value.all);
         State := Success;
         Insert_From_Sub_Cursor (Item.Cursor);
      end;
   end Next;


   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Wrapper_Type;
      State  :    out State_Type)
   is
      pragma Unreferenced (Cursor);
      pragma Unreferenced (Key);
      pragma Unreferenced (Value);
      pragma Unreferenced (State);
   begin
      -- TODO implement
      -- XXX How should it behave?!
      null;
      raise Map_Error;
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

