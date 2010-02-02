-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Tables.Column_Families is

   function New_RO_Transaction
     (Column_Family : Column_Family_Type)
      return RO_Transaction_Type is
   begin
      return Maps.New_RO_Transaction(Column_Family.Map.all);
   end New_RO_Transaction;


   procedure Start_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RO_Transaction_Type) is
   begin
      Maps.Start_Transaction(Column_Family.Map.all, Transaction);
   end Start_Transaction;


   procedure Finish_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RO_Transaction_Type) is
   begin
      Maps.Finish_Transaction(Column_Family.Map.all, Transaction);
   end Finish_Transaction;


   function New_RW_Transaction
     (Column_Family : Column_Family_Type)
      return RW_Transaction_Type is
   begin
      return Maps.New_RW_Transaction(Column_Family.Map.all);
   end New_RW_Transaction;


   procedure Start_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type) is
   begin
      Maps.Start_Transaction(Column_Family.Map.all, Transaction);
   end Start_Transaction;


   procedure Abort_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type) is
   begin
      Maps.Abort_Transaction(Column_Family.Map.all, Transaction);
   end Abort_Transaction;


   procedure Commit_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type) is
   begin
      Maps.Commit_Transaction(Column_Family.Map.all, Transaction);
   end Commit_Transaction;


   function New_Column_Family
     (Regexp         : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type)
      return Column_Family_Type is
   begin
      return (Ada.Finalization.Limited_Controlled with
              Guard => GNAT.Regexp.Compile(Regexp),
              Map   => new Maps.Map_Type'(Maps.New_Map(Max_Key_Size,
                                                       Max_Value_Size)));
   end New_Column_Family;


   function Matches
     (Column_Family : Column_Family_Type;
      Column_Name   : String)
      return Boolean is
   begin
      return GNAT.Regexp.Match(Column_Name, Column_Family.Guard);
   end Matches;


   function Matches
     (Column_Family : Column_Family_Type;
      Column_Name   : Types.Keys.Columns.String_Type)
      return Boolean is
   begin
      return Matches(Column_Family,
                     String(Types.Keys.Columns.To_Buffer(Column_Name)));
   end Matches;


   procedure Create
     (ID             : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type) is
   begin
      Maps.Create(ID, Max_Key_Size, Max_Value_Size);
   end Create;


   procedure Initialize
     (Column_Family : out Column_Family_Type;
      ID            : in  String) is
   begin
      Column_Family.Map := null;
      Maps.Initialize(Column_Family.Map.all, ID);
   exception
      when others =>
         Column_Family.Map := null;
         raise;
   end Initialize;


   overriding
   procedure Finalize
     (Column_Family : in out Column_Family_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Maps.Map_Type, Map_Ref_Type);
   begin
      if Column_Family.Map /= null then
         Maps.Finalize(Column_Family.Map.all);
         Free(Column_Family.Map);
      end if;
   end Finalize;


   function Max_Key_Size
     (Column_Family  : Column_Family_Type;
      Max_Value_Size : IO.Blocks.Size_Type)
      return IO.Blocks.Size_Type is
   begin
      return Maps.Max_Key_Size(Column_Family.Map.all, Max_Value_Size);
   end Max_Key_Size;


   procedure Retrieve
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Retrieve(Column_Family.Map.all, Key, Value, State);
   end Retrieve;


   procedure Retrieve
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Retrieve(Column_Family.Map.all, Transaction, Key, Value, State);
   end Retrieve;


   procedure Minimum
     (Column_Family : in out Column_Family_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Minimum(Column_Family.Map.all, Key, Value, State);
   end Minimum;


   procedure Minimum
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Minimum(Column_Family.Map.all, Transaction, Key, Value, State);
   end Minimum;


   procedure Maximum
     (Column_Family : in out Column_Family_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Maximum(Column_Family.Map.all, Key, Value, State);
   end Maximum;


   procedure Maximum
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Maximum(Column_Family.Map.all, Transaction, Key, Value, State);
   end Maximum;


   procedure Insert
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         : in     Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Insert(Column_Family.Map.all, Key, Value, State);
   end Insert;


   procedure Insert
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         : in     Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Insert(Column_Family.Map.all, Transaction, Key, Value, State);
   end Insert;


   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Delete(Column_Family.Map.all, Key, Value, State);
   end Delete;


   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Delete(Column_Family.Map.all, Transaction, Key, Value, State);
   end Delete;


   function Positive_Infinity_Bound
     (Column_Family : Column_Family_Type)
      return Bound_Type is
   begin
      return Maps.Positive_Infinity_Bound(Column_Family.Map.all);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
     (Column_Family : Column_Family_Type)
      return Bound_Type is
   begin
      return Maps.Negative_Infinity_Bound(Column_Family.Map.all);
   end Negative_Infinity_Bound;


   function New_Bound
     (Column_Family : Column_Family_Type;
      Comparison    : Comparison_Type;
      Key           : Key_Type)
      return Bound_Type is
   begin
      return Maps.New_Bound(Column_Family.Map.all, Comparison, Key);
   end New_Bound;


   function New_Cursor
     (Column_Family     : Column_Family_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type is
   begin
      return Maps.New_Cursor(Column_Family.Map.all, Transaction, Thread_Safe,
                             Lower_Bound, Upper_Bound, Reverse_Direction);
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean) is
   begin
      Maps.Set_Thread_Safety(Cursor, Enabled);
   end Set_Thread_Safety;


   procedure Finalize_Cursor
     (Column_Family : in     Column_Family_Type;
      Transaction   : in     Transaction_Type'Class;
      Cursor        : in out Cursor_Type) is
   begin
      Maps.Finalize_Cursor(Column_Family.Map.all, Transaction, Cursor);
   end Finalize_Cursor;


   procedure Pause
     (Column_Family : in out Column_Family_Type;
      Cursor        : in out Cursor_Type) is
   begin
      Maps.Pause(Column_Family.Map.all, Cursor);
   end Pause;


   procedure Unpause
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Cursor        : in out Cursor_Type) is
   begin
      Maps.Unpause(Column_Family.Map.all, Transaction, Cursor);
   end Unpause;


   procedure Next
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Cursor        : in out Cursor_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Next(Column_Family.Map.all, Transaction, Cursor, Key, Value, State);
   end Next;


   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Cursor        : in out Cursor_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type) is
   begin
      Maps.Delete(Column_Family.Map.all, Transaction, Cursor, Key, Value,
                  State);
   end Delete;


   procedure Count
     (Column_Family : in out Column_Family_Type;
      Count         :    out Count_Type) is
   begin
      Maps.Count(Column_Family.Map.all, Count);
   end Count;


   procedure Count
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Count         :    out Count_Type) is
   begin
      Maps.Count(Column_Family.Map.all, Transaction, Count);
   end Count;


   procedure Get_Height
     (Column_Family : in out Column_Family_Type;
      Height        :    out Natural) is
   begin
      Maps.Get_Height(Column_Family.Map.all, Height);
   end Get_Height;


   procedure Get_Height
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Height        :    out Natural) is
   begin
      Maps.Get_Height(Column_Family.Map.all, Transaction, Height);
   end Get_Height;


   procedure Clusterize
     (Column_Family : in out Column_Family_Type;
      State         :    out State_Type) is
   begin
      Maps.Clusterize(Column_Family.Map.all, State);
   end Clusterize;

end DB.Tables.Column_Families;

