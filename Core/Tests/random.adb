-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with DB.Types;
with DB.Types.Rows;
with DB.Types.Columns;
with DB.Types.Times;
with DB.Types.Gen_Bounded_Strings;
with DB.Types.Gen_Unbounded_Strings;
with DB.Locks.Mutexes;

with Ada.Unchecked_Deallocation;

package body Random is

   Mutex           : DB.Locks.Mutexes.Mutex_Type;
   Max_Size        : constant := 2048;
   --Max_Length      : constant := 1530 - 8 - 4 - 1;
   Max_Length      : constant := 1012 - 8 - 4;-- + 10;

   generic
      with package Str is new DB.Types.Gen_Bounded_Strings
        (Item_Type  => Char_Type,
         Max_Length => Max_Size);
      Random_Factor : in Natural := 1;
   function Make_String (I : Natural) return Str.String_Type;

   X : Natural := 0;
   function NI return Natural
   is
      N : constant := 5;
   begin
      X := (X + 1) mod N;
      if X = N-1 then
         return 0;
      else
         return 1;
      end if;
   end NI;

   function Make_String (I : Natural) return Str.String_Type
   is
      AZ : constant := Character'Pos('z') - Character'Pos('a') + 1;
      R  : constant Natural
         := Random_Factor * (Key_Value_Pairs'Length - I + 10) / I + 10 + I;
      L  : constant Str.Length_Type
         := Str.Length_Type((R mod Max_Length) + 1);
         --:= Str.Length_Type((NI * Max_Length) + 1);
         --:= Str.Length_Type(Max_Length);
      B  : Str.Indefinite_Buffer_Type(1 .. L);
   begin
      for J in 1 .. L loop
         if J mod 2 = 0 then
            B(J) := Char_Type'Val(Char_Type'Pos('a') + ((R*J) mod AZ));
         else
            B(J) := Char_Type'Val(Char_Type'Pos('A') + ((R*J) mod AZ));
         end if;
      end loop;
      return Str.New_String(B);
   end Make_String;


   generic
      with package Str is new DB.Types.Gen_Unbounded_Strings
        (Item_Type  => Char_Type);
      Random_Factor : in Natural := 1;
   function Make_UString (I : Natural) return Str.String_Type;


   function Make_UString (I : Natural) return Str.String_Type
   is
      AZ : constant := Character'Pos('z') - Character'Pos('a') + 1;
      R  : constant Natural
         := Random_Factor * (Key_Value_Pairs'Length - I + 10) / I + 10 + I;
      L  : constant Str.Length_Type
         := Str.Length_Type((R mod Max_Length) + 1);
      B  : Str.Buffer_Type(1 .. L);
   begin
      for J in 1 .. L loop
         if J mod 2 = 0 then
            B(J) := Char_Type'Val(Char_Type'Pos('a') + ((R*J) mod AZ));
         else
            B(J) := Char_Type'Val(Char_Type'Pos('A') + ((R*J) mod AZ));
         end if;
      end loop;
      return Str.New_String(B);
   end Make_UString;


   function Make_Row is new Make_String
     (Str => DB.Types.Rows,
      Random_Factor => 1);


   function Make_Column is new Make_String
     (Str => DB.Types.Columns,
      Random_Factor => 3);


   procedure Init_Key_Value_Pairs (Init : in Count_Type)
   is begin
      DB.Locks.Mutexes.Lock(Mutex);
      Initial_KV := Init;
      Current_KV := Initial_KV;
      for I in Key_Value_Pairs'Range loop
         Key_Value_Pairs(I).Key.Row    := Make_Row(I);
         Key_Value_Pairs(I).Key.Column := Make_Column(I);
         Key_Value_Pairs(I).Key.Time   := 0;
         Key_Value_Pairs(I).Value      := 1;
      end loop;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Init_Key_Value_Pairs;


   procedure Reset_String_Generation
   is begin
      DB.Locks.Mutexes.Lock(Mutex);
      Current_KV := Initial_KV;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Reset_String_Generation;


   function To_Value (Count : Count_Type) return DB.Types.Values.Value_Type
   is
      Last : constant Count_Type
           := Count_Type(DB.Types.Values.Value_Type'Last);
      Off  : constant Count_Type
           := Count_Type(DB.Types.Values.Value_Type'First);
   begin
      return DB.Types.Values.Value_Type((Count mod (Last)) + Off);
   end To_Value;


   function Random_Entry return Key_Value_Type
   is
      KV : Key_Value_Type;
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      declare
         I  : constant Positive
         := Positive((Current_KV mod Count_Type(Key_Value_Pairs'Length)) + 1);
      begin
         Key_Value_Pairs(I).Key.Time := DB.Types.Times.Number_Type(Current_KV);
         Key_Value_Pairs(I).Value := To_Value(Current_KV);
         Current_KV := Current_KV + 1;
         KV := Key_Value_Pairs(I);
      end;
      DB.Locks.Mutexes.Unlock(Mutex);
      return KV;
   end Random_Entry;


   function Some_Entry return Key_Value_Type
   is
      KV : Key_Value_Type;
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      declare
         I  : constant Positive
         := Positive((Current_KV mod Count_Type(Key_Value_Pairs'Length)) + 1);
      begin
         Key_Value_Pairs(I).Key.Time := DB.Types.Times.Number_Type(Current_KV);
         Key_Value_Pairs(I).Value := To_Value(Current_KV);
         KV := Key_Value_Pairs(I);
      end;
      DB.Locks.Mutexes.Unlock(Mutex);
      return KV;
   end Some_Entry;


   procedure Finalize_Key_Value_Pairs
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Key_Value_Array_Type, Key_Value_Array_Access_Type);
   begin
      Free(Key_Value_Pairs);
   end Finalize_Key_Value_Pairs;

end Random;

