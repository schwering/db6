with DB.Types;
with DB.Types.Times;

with DB.Types.Gen_Strings;
with DB.Types.Strings;
with DB.Types.Values;

pragma Warnings (Off);
with DB.Types.Gen_Strings.Gen_Bounded;
pragma Warnings (On);

pragma Warnings (Off);
with DB.Types.Gen_Strings.Gen_Unbounded;
pragma Warnings (On);

with DB.Locks.Mutexes;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body IO_Dispatcher.Random is

   Mutex             : DB.Locks.Mutexes.Mutex_Type;
   Max_String_Length : constant := 1020 - 2 - 8 - 4 - 4
   --                               ^M    ^P  ^T  ^L  ^V
                                ;--+ 1; -- to enforce heaped map
   -- M = (4096 - Meta_Data_Size) * 1 / 4
   -- P = Long_Position_Type for inner nodes
   -- T = Timestamp_Size (part of Key_Type)
   -- L = Value_Length_Size for value
   -- V = Value_Buffer_Size for value


   generic
      with package Strings is new DB.Types.Gen_Strings(Char_Type);
      type String_Type is private;
      with function New_String (Buf : Strings.Indefinite_Buffer_Type)
         return String_Type;
      Random_Weight : in Natural := 1;
   function Make_String (Index : Natural) return String_Type;

   function Make_String (Index : Natural) return String_Type
   is
      Alphabet_Length : constant
                      := Char_Type'Pos('z') - Char_Type'Pos('a') + 1;
      Total_KV_Count  : constant Positive
                      := Key_Value_Pairs'Length;
      Random_Factor   : constant Strings.Length_Type'Base
                      := Random_Weight * Total_KV_Count / Index + 10 + Index;
      String_Length   : constant Strings.Length_Type
                      := ((Random_Factor mod Max_String_Length) + 1);
      String_Buffer   : Strings.Indefinite_Buffer_Type(1 .. String_Length);
   begin
      for J in 1 .. String_Length loop
         declare
            Char_Pos : Natural;
            Offset   : constant Natural
                     := (Random_Factor * J) mod Alphabet_Length;
         begin
            if (String_Length + J) mod 2 = 0 then
               Char_Pos := Char_Type'Pos('a') + Offset;
            else
               Char_Pos := Char_Type'Pos('A') + Offset;
            end if;
            String_Buffer(J) := Char_Type'Val(Char_Pos);
         end;
      end loop;
      return New_String(String_Buffer);
   end Make_String;


   function Make_Row is new Make_String
     (Strings       => DB.Types.Strings,
      String_Type   => DB.Types.Keys.Rows.String_Type,
      New_String    => DB.Types.Keys.Rows.New_String,
      Random_Weight => 1);


   function Make_Column is new Make_String
     (Strings       => DB.Types.Strings,
      String_Type   => DB.Types.Keys.Columns.String_Type,
      New_String    => DB.Types.Keys.Columns.New_String,
      Random_Weight => 3);


   function Make_Value1 (Count : Count_Type) return Values.String_Type
   is
      Max_Len : constant := 4;
      type Uint32 is mod 2**32;
      type Definite_Buffer_Type is
         new DB.Types.Values.Indefinite_Buffer_Type(1 .. 4);
      function Convert is new Ada.Unchecked_Conversion
        (Uint32, Definite_Buffer_Type);

      I   : constant Uint32 
          := Uint32(Count mod Uint32'Modulus);
      Buf : constant DB.Types.Values.Indefinite_Buffer_Type
          := DB.Types.Values.Indefinite_Buffer_Type(Convert(I));
   begin
      return Values.New_String(Buf);
   end Make_Value1;

   function Make_Value2 (Count : Count_Type) return Values.String_Type
   is
      Max_Len : constant := 4;
      Img  : constant String   := Count_Type'Image(Count);
      From : constant Positive := Integer'Max(Img'Last - Max_Len+1, Img'First);
      Sub  : constant String   := Img(From .. Img'Last);

      subtype R is Positive range From .. Img'Last;
      type Definite_String_Type is new String(R);
      type Definite_Buffer_Type is
         new DB.Types.Values.Indefinite_Buffer_Type(R);
      function Convert is new Ada.Unchecked_Conversion
        (Definite_String_Type, Definite_Buffer_Type);

      Buf : constant DB.Types.Values.Indefinite_Buffer_Type
          := DB.Types.Values.Indefinite_Buffer_Type
                (Convert(Definite_String_Type(Sub)));
   begin
      return Values.New_String(Buf);
   end Make_Value2;


   function Make_Value (Count : Count_Type) return Values.String_Type
   renames Make_Value2;


   procedure Init_Key_Value_Pairs (Init : in Count_Type) is
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      Initial_KV := Init;
      Current_KV := Initial_KV;
      for I in Key_Value_Pairs'Range loop
         Key_Value_Pairs(I).Key.Row    := Make_Row(I);
         Key_Value_Pairs(I).Key.Column := Make_Column(I);
         Key_Value_Pairs(I).Key.Time   := 0;
         Key_Value_Pairs(I).Value      := Make_Value(Count_Type(I));
      end loop;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Init_Key_Value_Pairs;


   procedure Reset_String_Generation is
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      Current_KV := Initial_KV;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Reset_String_Generation;


   function Random_Entry return Key_Value_Type
   is
      KV : Key_Value_Type;
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      declare
         I : constant Positive
         := Positive((Current_KV mod Count_Type(Key_Value_Pairs'Length)) + 1);
      begin
         Key_Value_Pairs(I).Key.Time := DB.Types.Times.Number_Type(Current_KV);
         Key_Value_Pairs(I).Value := Make_Value(Current_KV);
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
         Key_Value_Pairs(I).Value := Make_Value(Current_KV);
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


   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type is
   begin
      return KV.Key;
   end Key;


   function Value (KV : Key_Value_Type)
      return Values.String_Type is
   begin
      return KV.Value;
   end Value;

end IO_Dispatcher.Random;

