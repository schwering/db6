-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

separate (DB.Gen_Map_Reduce)
package body Gen_Logs is

   ----------
   -- The first meat! Creation etc.

   procedure Create
     (Log : out Log_Type;
      ID  : in  String) is
   begin
      pragma Assert (not Log.Initialized);
      Block_IO.Create(ID, Log.File);
      Block_IO.Close(Log.File);
      IO.Blocks.Reset(Log.Block);
      Log.Mode        := Write;
      Log.Address     := Block_IO.First;
      Log.Position    := Log.Block'First;
      Log.Initialized := True;
   end Create;


   procedure Finalize
     (Log : in out Log_Type) is
   begin
      if Log.Initialized then
         Block_IO.Close(Log.File);
         Log.Initialized := False;
      end if;
   end Finalize;


   procedure Switch
     (Log  : in out Log_Type;
      Mode : in     Mode_Type)
   is
      use Block_IO;
   begin
      case Log.Mode is
         when Read =>
            null;
         when Write =>
            Block_IO.Write(Log.File, Log.Address, Log.Block); 
      end case;

      Log.Mode     := Mode;
      Log.Address  := Block_IO.First;
      Log.Position := Log.Block'First;

      case Log.Mode is
         when Read =>
            Block_IO.Read(Log.File, Log.Address, Log.Block); 
         when Write =>
            Log.Last_Address := Invalid_Address;
      end case;
   end Switch;


   procedure Get
     (Log   : in out Log_Type;
      Item  :    out Item_Type;
      State :    out Result_Type)
   is
      pragma Assert (Log.Initialized);
      use Block_IO;

      function Leq (Cur, Last : IO.Blocks.Position_Type) return Boolean
      is
         use type IO.Blocks.Position_Type;
      begin
         return not IO.Blocks.Is_Valid(Log.Block, Last) or else
                (IO.Blocks.Is_Valid(Log.Block, Cur) and then Cur <= Last);
      end Leq;

      Context : Context_Type;
   begin
      Locks.Mutexes.Lock(Log.Mutex);
      if not Is_Valid_Address(Log.Last_Address) or else
         not (Log.Address < To_Valid_Address(Log.Last_Address) or
              (Log.Address = To_Valid_Address(Log.Last_Address) and
               Leq(Log.Position, Log.Last_Position))) then
          raise Map_Reduce_Error;
      end if;

      loop
         declare
            Cursor : IO.Blocks.Cursor_Type
                   := IO.Blocks.New_Cursor(Log.Position);
            Done   : Boolean;
         begin
            Read_Part_Of_Item(Context, Log.Block, Cursor, Item, Done);
            exit when Done;
            Log.Address  := Succ(Log.Address);
            Log.Position := Log.Block'First;
            Read(Log.File, Log.Address, Log.Block);
         end;
      end loop;
      Locks.Mutexes.Unlock(Log.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(Log.Mutex);
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Get;


   procedure Put
     (Log   : in out Log_Type;
      Item  : in     Item_Type;
      State :    out Result_Type)
   is
      pragma Assert (Log.Initialized);
      use Block_IO;
      Context : Context_Type;
   begin
      Locks.Mutexes.Lock(Log.Mutex);
      loop
         declare
            Cursor : IO.Blocks.Cursor_Type
                   := IO.Blocks.New_Cursor(Log.Position);
            Done   : Boolean;
         begin
            Write_Part_Of_Item(Context, Log.Block, Cursor, Item, Done);
            Log.Position := IO.Blocks.Position(Cursor);
            exit when Done;
            Write(Log.File, Log.Address, Log.Block);
            Log.Address  := Succ(Log.Address);
            Log.Position := Log.Block'First;
         end;
      end loop;
      Log.Last_Address  := To_Address(Log.Address);
      Log.Last_Position := Log.Position;
      Locks.Mutexes.Unlock(Log.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(Log.Mutex);
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Put;

end Gen_Logs;

