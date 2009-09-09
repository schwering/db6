package body DB.Util.Gen_Hashtables is

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


   procedure Reorganize
     (Table : in out Table_Type)
   is
      type Container_Type is
         record
            Key   : Key_Type;
            Value : Value_Type;
         end record;
      Containers : array (1 .. Table.Size) of Container_Type;
      J : Integer := Containers'First;
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


   procedure Put
     (Table : in out Table_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type)
   is
      First_H     : constant Hash_Type := Hash(Key);
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
         H := Rehash(H);
         exit when H = First_H;
      end loop;
      if Found_Match then
         Table.Arr(H) := (Used, Key, Value);
      elsif Found_Free then
         Table.Arr(H) := (Used, Key, Value);
         Table.Size   := Table.Size + 1;
      else -- wrapped around
         declare
            Found_Visited : Boolean;
         begin
            H := First_H;
            loop
               Found_Visited := Table.Arr(H).State = Visited;
               exit when Found_Visited;
               H := Rehash(H);
               exit when H = First_H;
            end loop;
            if Found_Visited then
               Table.Arr(H) := (Used, Key, Value);
               Table.Size   := Table.Size + 1;
               Table.Visits := Table.Visits - 1;
               if Visit_Ratio(Table) > Visit_Threshold
               and Visit_Ratio(Table) + Size_Ratio(Table)
                     > Visit_Threshold + Size_Threshold then
                  Reorganize(Table);
               end if;
            else
               raise Hash_Table_Error;
            end if;
         end;
      end if;
   end Put;


   procedure Delete
     (Table : in out Table_Type;
      Key   : in     Key_Type)
   is
      First_H     : constant Hash_Type := Hash(Key);
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
         H := Rehash(H);
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
      First_H     : constant Hash_Type := Hash(Key);
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
         H := Rehash(H);
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
      First_H     : constant Hash_Type := Hash(Key);
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
         H := Rehash(H);
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
      Success :    out Boolean)
   is begin
      for H in Table.Arr'Range loop
         if Table.Arr(H).State = Used then
            Key          := Table.Arr(H).Key;
            Value        := Table.Arr(H).Value;
            Table.Arr(H) := (State => Visited);
            Table.Size   := Table.Size - 1;
            Table.Visits := Table.Visits + 1;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Pop;


   function Size
     (Table : Table_Type)
      return Natural
   is begin
      return Table.Size;
   end Size;

end DB.Util.Gen_Hashtables;

