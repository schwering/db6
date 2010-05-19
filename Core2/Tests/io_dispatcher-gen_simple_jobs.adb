with Ada.Text_IO; use Ada.Text_IO;

with DB.Blocks;

package body IO_Dispatcher.Gen_Simple_Jobs is

   procedure Insert
   is
      KV    : constant Key_Value_Type := Next_Entry;
      State : State_Type := Success;
   begin
      Check_Key_Value(KV);
      P_Insert(Object, Get_Key(KV), Get_Value(KV), State);
      if State /= Success then
         Put_Line("Insertion failed "& State'Img);
         raise Stop_Now;
      end if;
      declare
         Val   : Value_Type := Null_Value;
         State : State_Type := Failure;
      begin
         P_Search(Object, Get_Key(KV), Val, State);
         if State /= Success or else
            not Equal_Values(Get_Value(KV), Val) then
            Put_Line("Look up failed "& State'Img);
            Put_Line("Key   = """& Key_To_String(Get_Key(KV)) &"""");
            Put_Line("Value = """& Value_To_String(Get_Value(KV)) &"""");
            Put_Line("Value = """& Value_To_String(Val) &"""");
            raise Stop_Now;
         end if;
      end;
   end Insert;


   procedure Delete
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Success;
   begin
      P_Delete(Object, Get_Key(KV), Val, State);
      if State /= Success or else
         not Equal_Values(Get_Value(KV), Val) then
         Put_Line("Deletion failed "& State'Img);
         Put_Line("Key   = """& Key_To_String(Get_Key(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Get_Value(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Val) &"""");
         raise Stop_Now;
      end if;
   end Delete;


   procedure Search
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Success;
   begin
      P_Search(Object, Get_Key(KV), Val, State);
      if State /= Success or else
         not Equal_Values(Get_Value(KV), Val) then
         Put_Line("Look up failed "& State'Img);
         Put_Line("Key   = """& Key_To_String(Get_Key(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Get_Value(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Val) &"""");
         Put_Line("Equal = "& Boolean'Image(Equal_Values(Get_Value(KV), Val)));
         raise Stop_Now;
      end if;
   end Search;


   procedure Antisearch
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Failure;
   begin
      P_Search(Object, Get_Key(KV), Val, State);
      if State /= Failure then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Antisearch;


   procedure Count
   is
      Count : Count_Type;
   begin
      P_Count(Object, Count);
      Put_Line("Count:"& Count'Img);
   end Count;


   procedure Stats is
   begin
      P_Stats(Object);
   end Stats;


   procedure Check is
   begin
      P_Check(Object);
   end Check;

end IO_Dispatcher.Gen_Simple_Jobs;

