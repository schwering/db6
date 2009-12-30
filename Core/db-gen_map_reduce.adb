with System.Storage_Pools;

with DB.Gen_BTrees;
with DB.Locks.Mutexes;
with DB.Utils.Global_Pool;

procedure DB.Gen_Map_Reduce is

   ----------
   -- Helpers for intermediate keys/values.

   type Intermediate_Key_Context_Type is null record;
   type Intermediate_Value_Context_Type is null record;

   procedure Read_Intermediate_Key
     (Context : in out Intermediate_Key_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     :    out Intermediate_Key_Type)
   is
      pragma Unreferenced (Context);
   begin
      Read_Intermediate_Key(Block, Cursor, Key);
   end Read_Intermediate_Key;


   procedure Skip_Intermediate_Key
     (Context : in out Intermediate_Key_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Key : Intermediate_Key_Type;
   begin
      Read_Intermediate_Key(Context, Block, Cursor, Key);
   end Skip_Intermediate_Key;


   procedure Write_Intermediate_Key
     (Context : in out Intermediate_Key_Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     : in     Intermediate_Key_Type)
   is
      pragma Unreferenced (Context);
   begin
      Write_Intermediate_Key(Block, Cursor, Key);
   end Write_Intermediate_Key;


   procedure Read_Intermediate_Value
     (Context : in out Intermediate_Value_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   :    out Intermediate_Value_Type)
   is
      pragma Unreferenced (Context);
   begin
      Read_Intermediate_Value(Block, Cursor, Value);
   end Read_Intermediate_Value;


   procedure Skip_Intermediate_Value
     (Context : in out Intermediate_Value_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Value : Intermediate_Value_Type;
   begin
      Read_Intermediate_Value(Context, Block, Cursor, Value);
   end Skip_Intermediate_Value;


   procedure Write_Intermediate_Value
     (Context : in out Intermediate_Value_Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in     Intermediate_Value_Type)
   is
      pragma Unreferenced (Context);
   begin
      Write_Intermediate_Value(Block, Cursor, Value);
   end Write_Intermediate_Value;


   package Intermediate_BTrees is new Gen_BTrees
     (Key_Type                      => Intermediate_Key_Type,
      Key_Context_Type              => Intermediate_Key_Context_Type,
      Key_Size_Bound                => Intermediate_Key_Size_Bound,
      Read_Key                      => Read_Intermediate_Key,
      Skip_Key                      => Skip_Intermediate_Key,
      Write_Key                     => Write_Intermediate_Key,
      "="                           => "=",
      "<="                          => "<=",
      Value_Type                    => Intermediate_Value_Type,
      Value_Context_Type            => Intermediate_Value_Context_Type,
      Value_Size_Bound              => Intermediate_Value_Size_Bound,
      Read_Value                    => Read_Intermediate_Value,
      Skip_Value                    => Skip_Intermediate_Value,
      Write_Value                   => Write_Intermediate_Value,
      Is_Context_Free_Serialization => False,
      Storage_Pool                  => Utils.Global_Pool.Global'Storage_Pool,
      Block_IO                      => Intermediate_Block_IO);

   type Context_Type is
      record
         Intermediates : Intermediate_BTrees.Tree_Type;
      end record;


   ----------
   -- Logs with external sorting.

   generic
      type Item_Type is private;
      type Context_Type is private;
      with procedure Read_Part_Of_Item
             (Context : in out Context_Type;
              Block   : in     IO.Blocks.Base_Block_Type;
              Cursor  : in out IO.Blocks.Cursor_Type;
              Item    : in out Item_Type;
              Done    :    out Boolean);
      with procedure Write_Part_Of_Item
             (Context : in out Context_Type;
              Block   : in out IO.Blocks.Base_Block_Type;
              Cursor  : in out IO.Blocks.Cursor_Type;
              Item    : in     Item_Type;
              Done    :    out Boolean);

      Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;

      with package Block_IO is new IO.Blocks.Gen_IO (<>);
   package Gen_Logs is

      type Mode_Type is (Read, Write);
      type Log_Type is limited private;
      type Result_Type is (Success, Failure, Error);

      procedure Create
        (Log : out Log_Type;
         ID  : in  String);

      procedure Finalize
        (Log : in out Log_Type);

      procedure Switch
        (Log  : in out Log_Type;
         Mode : in     Mode_Type);

      procedure Get
        (Log   : in out Log_Type;
         Item  :    out Item_Type;
         State :    out Result_Type);

      procedure Put
        (Log   : in out Log_Type;
         Item  : in     Item_Type;
         State :    out Result_Type);

   private
      type Log_Ref_Type is access all Log_Type;
      pragma Controlled (Log_Ref_Type);
      for Log_Ref_Type'Storage_Size use 0;

      type Log_Type is limited
         record
            File          : Block_IO.File_Type;
            Mode          : Mode_Type;
            Mutex         : Locks.Mutexes.Mutex_Type;
            Block         : IO.Blocks.Block_Type;
            Address       : Block_IO.Valid_Address_Type;
            Position      : IO.Blocks.Base_Position_Type;
            Last_Address  : Block_IO.Address_Type;
            Last_Position : IO.Blocks.Base_Position_Type;
            Initialized   : Boolean := False;
         end record;

   end Gen_Logs;

   package body Gen_Logs is separate;


   ----------
   -- Real meat: the map, sort and reduce phase.

   procedure Map_Phase
     (Context : in out Context_Type)
   is
      task type Map_Task_Type is
         entry Start;
      end Map_Task_Type;

      task body Map_Task_Type
      is
         procedure Emit
           (Key     : in Intermediate_Key_Type;
            Value   : in Intermediate_Value_Type)
         is
            use type Intermediate_BTrees.Result_Type;
            Position : Intermediate_BTrees.Count_Type;
            State    : Intermediate_BTrees.Result_Type;
         begin
            Intermediate_BTrees.Insert(Context.Intermediates, Key, Value,
                                       Position, State);
            if State = Intermediate_BTrees.Success then
               raise Tree_Error;
            end if;
         end Emit;

         In_Key   : In_Key_Type;
         In_Value : In_Value_Type;
         Success  : Boolean;
      begin
         accept Start;
         loop
            Input(In_Key, In_Value, Success);
            exit when not Success;
            Map(In_Key, In_Value, Emit'Access);
         end loop;
      end Map_Task_Type;

      Tasks : array (1 .. Map_Task_Count) of Map_Task_Type;
   begin
      for I in Tasks'Range loop
         Tasks(I).Start;
      end loop;
      -- let the tasks work
   end Map_Phase;


   procedure Sort_Phase
     (Context : in out Context_Type)
   is null;


   procedure Reduce_Phase
     (Context : in out Context_Type)
   is
      task type Reduce_Task_Type is
         entry Start;
      end Reduce_Task_Type;

      Transaction : Intermediate_BTrees.RO_Transaction_Type :=
         Intermediate_BTrees.New_RO_Transaction(Context.Intermediates);

      task body Reduce_Task_Type is
      begin
         accept Start;
         loop
            declare
               Intermediate_Key : Intermediate_Key_Type;
            begin
               -- get next Intermediate_Key and Cursor to it
               declare
                  -- TODO XXX wrong bounds due to wrong Intermediate_Key_Type
                  Lower_Bound : constant Intermediate_BTrees.Bound_Type :=
                     Intermediate_BTrees.New_Bound(Intermediate_BTrees.Equal,
                                                   Intermediate_Key);
                  Upper_Bound : constant Intermediate_BTrees.Bound_Type :=
                     Intermediate_BTrees.New_Bound(Intermediate_BTrees.Equal,
                                                   Intermediate_Key);
                  Cursor : Intermediate_BTrees.Cursor_Type :=
                     Intermediate_BTrees.New_Cursor
                       (Tree              => Context.Intermediates,
                        Transaction       => Transaction,
                        Thread_Safe       => False,
                        Lower_Bound       => Lower_Bound,
                        Upper_Bound       => Upper_Bound,
                        Reverse_Direction => False);

                  procedure Next_Value
                    (Value   : out Intermediate_Value_Type;
                     Success : out Boolean)
                  is
                     use type Intermediate_BTrees.Result_Type;
                     Key   : Intermediate_Key_Type;
                     State : Intermediate_BTrees.Result_Type;
                  begin
                     Intermediate_BTrees.Next
                        (Tree        => Context.Intermediates,
                         Transaction => Transaction,
                         Cursor      => Cursor,
                         Key         => Key,
                         Value       => Value,
                         State       => State);
                     Success := State = Intermediate_BTrees.Success;
                  end Next_Value;

                  Out_Key   : Out_Key_Type;
                  Out_Value : Out_Value_Type;
               begin
                  Reduce(Intermediate_Key, Next_Value'Access,
                         Out_Key, Out_Value);
                  Output(Out_Key, Out_Value);
               exception
                  when others =>
                     Intermediate_BTrees.Finalize(Context.Intermediates,
                                                  Cursor);
                     raise;
               end;
            end;
         end loop;
      end Reduce_Task_Type;

      Tasks : array (1 .. Reduce_Task_Count) of Reduce_Task_Type;
   begin
      Intermediate_BTrees.Start_Transaction(Context.Intermediates,
                                            Transaction);
      for I in Tasks'Range loop
         Tasks(I).Start;
      end loop;
      -- let the tasks work
   exception
      when others =>
         Intermediate_BTrees.Finish_Transaction(Context.Intermediates,
                                                Transaction);
   end Reduce_Phase;


   Context : Context_Type;
begin
   Intermediate_BTrees.Initialize(Context.Intermediates,
                                  "bluhp.intermediate");
   Map_Phase(Context);
   Sort_Phase(Context);
   Reduce_Phase(Context);
   Intermediate_BTrees.Finalize(Context.Intermediates);
exception
   when others =>
      Intermediate_BTrees.Finalize(Context.Intermediates);
      raise;
end DB.Gen_Map_Reduce;

