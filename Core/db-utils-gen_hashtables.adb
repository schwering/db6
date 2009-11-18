-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Utils.Gen_Hashtables is

   function Visit_Ratio (T : Table_Type) return Float
   is
      pragma Inline (Visit_Ratio);
   begin
      return Float(T.Visits) / Float(T.Arr'Length);
   end Visit_Ratio;


   function Size_Ratio (T : Table_Type) return Float
   is
      pragma Inline (Size_Ratio);
   begin
      return Float(T.Size) / Float(T.Arr'Length);
   end Size_Ratio;


   function To_Index (T : Table_Type; H : Hash_Type) return Hash_Type
   is
      pragma Inline (To_Index);
   begin
      return (H mod T.Arr'Length) + T.Arr'First;
   end To_Index;


   procedure Reorganize
     (Table : in out Table_Type)
   is
      type Container_Type is
         record
            Key   : Key_Type;
            Value : Value_Type;
         end record;
      Containers : array (1 .. Table.Size) of Container_Type;
      J          : Integer := Containers'First;
   begin
      for I in Table.Arr'Range loop
         if Table.Arr(I).State = Used then
            Containers(J) := (Table.Arr(I).Key, Table.Arr(I).Value);
            J             := J + 1;
         end if;
      end loop;
      Table.Arr    := (others => Element_Type'(State => Free));
      Table.Size   := 0;
      Table.Visits := 0;
      for I in Containers'Range loop
         Put(Table, Containers(I).Key, Containers(I).Value);
      end loop;
   end Reorganize;


   function To_Prime
     (Size : Size_Type)
      return Hash_Type
   is
      Primes : constant array (Positive range <>) of Hash_Type :=
        (53,         97,         193,       389,       769,
         1543,       3079,       6151,      12289,     24593,
         49157,      98317,      196613,    393241,    786433,
         1572869,    3145739,    6291469,   12582917,  25165843,
         50331653,   100663319,  201326611, 402653189, 805306457,
         1610612741, 3221225473, 4294967291);
   begin
      for I in Primes'Range loop
         if Primes(I) >= Hash_Type(Size) then
            return Primes(I);
         end if;
      end loop;
      raise Constraint_Error;
   end To_Prime;


   function New_Table
     (Size : Size_Type)
      return Table_Type
   is
      Capacity : constant Hash_Type := To_Prime(Size);
   begin
      return Table_Type'(Last_Index => Capacity - 1, others => <>);
   end New_Table;


   procedure Allocate_Table
     (Size  : in  Size_Type;
      Table : out Table_Ref_Type)
   is
      Capacity : constant Hash_Type := To_Prime(Size);
   begin
      Table := new Table_Type(Capacity - 1);
   end Allocate_Table;


   procedure Put
     (Table : in out Table_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type)
   is
      First_H       : constant Hash_Type := To_Index(Table, Hash(Key));
      H             : Hash_Type := First_H;
      Visited_H     : Hash_Type := First_H;
      Found_Match   : Boolean;
      Found_Free    : Boolean;
      Found_Visited : Boolean := False;
   begin
      loop
         Found_Match   := Table.Arr(H).State = Used
                          and then Table.Arr(H).Key = Key;
         Found_Free    := Table.Arr(H).State = Free;
         if not Found_Visited and Table.Arr(H).State = Visited then
            Found_Visited := True;
            Visited_H     := H;
         end if;
         exit when Found_Match;
         exit when Found_Free;
         H := To_Index(Table, Rehash(H));
         exit when H = First_H;
      end loop;
      if Found_Match then
         Table.Arr(H) := (Used, Key, Value);
      elsif Found_Free then
         Table.Arr(H) := (Used, Key, Value);
         Table.Size   := Table.Size + 1;
      elsif Found_Visited then -- wrapped around
         Table.Arr(Visited_H) := (Used, Key, Value);
         Table.Size           := Table.Size + 1;
         Table.Visits         := Table.Visits - 1;
         if Visit_Ratio(Table) > Visit_Threshold then
            Reorganize(Table);
         end if;
      else
         raise Hash_Table_Error;
      end if;
   end Put;


   procedure Delete
     (Table : in out Table_Type;
      Key   : in     Key_Type)
   is
      First_H     : constant Hash_Type := To_Index(Table, Hash(Key));
      H           : Hash_Type := First_H;
      Found_Match : Boolean;
      Found_Free  : Boolean;
   begin
      loop
         Found_Match := Table.Arr(H).State = Used
                        and then Table.Arr(H).Key = Key;
         Found_Free  := Table.Arr(H).State = Free;
         exit when Found_Match;
         exit when Found_Free;
         H := To_Index(Table, Rehash(H));
         exit when H = First_H;
      end loop;
      if Found_Match then
         Table.Arr(H) := (State => Visited);
         Table.Size   := Table.Size - 1;
         Table.Visits := Table.Visits + 1;
      end if;
   end Delete;


   procedure Get
     (Table : in  Table_Type;
      Key   : in  Key_Type;
      Value : out Value_Type;
      Found : out Boolean)
   is
      First_H     : constant Hash_Type := To_Index(Table, Hash(Key));
      H           : Hash_Type := First_H;
      Found_Match : Boolean;
      Found_Free  : Boolean;
   begin
      loop
         Found_Match := Table.Arr(H).State = Used
                        and then Table.Arr(H).Key = Key;
         Found_Free  := Table.Arr(H).State = Free;
         exit when Found_Match;
         exit when Found_Free;
         H := To_Index(Table, Rehash(H));
         exit when H = First_H;
      end loop;
      if Found_Match then
         Value := Table.Arr(H).Value;
      end if;
      Found := Found_Match;
   end Get;


   function Get
     (Table : Table_Type;
      Key   : Key_Type)
      return Value_Type
   is
      Value : Value_Type;
      Found : Boolean;
   begin
      Get(Table, Key, Value, Found);
      return Value;
   end Get;


   function Contains
     (Table : Table_Type;
      Key   : Key_Type)
      return Boolean
   is
      First_H     : constant Hash_Type := To_Index(Table, Hash(Key));
      H           : Hash_Type := First_H;
      Found_Match : Boolean;
      Found_Free  : Boolean;
   begin
      loop
         Found_Match := Table.Arr(H).State = Used
                        and then Table.Arr(H).Key = Key;
         Found_Free  := Table.Arr(H).State = Free;
         exit when Found_Match;
         exit when Found_Free;
         H := To_Index(Table, Rehash(H));
         exit when H = First_H;
      end loop;
      return Found_Match;
   end Contains;


   procedure Pop
     (Table   : in out Table_Type;
      Key     :    out Key_Type;
      Value   :    out Value_Type)
   is
      Success : Boolean;
   begin
      Pop(Table, Key, Value, Success);
      if not Success then
         raise Hash_Table_Error;
      end if;
   end Pop;


   procedure Pop
     (Table   : in out Table_Type;
      Key     :    out Key_Type;
      Value   :    out Value_Type;
      Success :    out Boolean) is
   begin
      for H in Table.Arr'Range loop
         if Table.Arr(H).State = Used then
            Key          := Table.Arr(H).Key;
            Value        := Table.Arr(H).Value;
            Table.Arr(H) := (State => Visited);
            Table.Size   := Table.Size - 1;
            Table.Visits := Table.Visits + 1;
            Success      := True;
            return;
         end if;
      end loop;
      Success := False;
   end Pop;


   function Size
     (Table : Table_Type)
      return Size_Type is
   begin
      return Table.Size;
   end Size;

end DB.Utils.Gen_Hashtables;

