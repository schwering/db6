with Ada.Text_IO;
use Ada.Text_IO;

procedure Rabin_Karp
is
   Base : constant := 101;

   type Hash_Type is mod 2**32;

   type Cursor_Type is
      record
         From   : Positive;
         Length : Natural;
         Hash   : Hash_Type := Hash_Type'Last;
      end record;

   function New_Cursor (S : String; Length : Natural) return Cursor_Type
   is begin
      pragma Assert (Length <= S'Length);
      return (From   => S'First,
              Length => Length,
              Hash   => Hash_Type'Last);
   end New_Cursor;

   procedure Roll (S : in String; Cursor : in out Cursor_Type)
   is
      From   : Positive  renames Cursor.From;
      Length : Natural   renames Cursor.Length;
      Hash   : Hash_Type renames Cursor.Hash;
   begin
      pragma Assert (Hash = Hash_Type'Last); -- Initialized
      pragma Assert (From + Length - 1 <= S'Last);
      if From = S'First then
         Hash := 0;
         for I in From .. From + Length - 1 loop
            Hash := Base * Hash + Character'Pos(S(I));
         end loop;
         From := From + 1;
      else
         declare
            Minus : constant Hash_Type
                  := Character'Pos(S(From - 1)) * (Base ** (Length - 1));
            Plus  : constant Hash_Type
                  := Character'Pos(S(From + Length - 1));
         begin
            Hash := (Hash - Minus) * Base + Plus;
            From := From + 1;
         end;
      end if;
   end Roll;

   function Hash (Cursor : Cursor_Type) return Hash_Type
   is begin
      return Cursor.Hash;
   end Hash;

   function Done (S : String; Cursor : Cursor_Type) return Boolean
   is
      From   : Positive renames Cursor.From;
      Length : Natural  renames Cursor.Length;
   begin
      return From + Length - 1 > S'Last;
   end Done;

   function Substring (S : String; Cursor : Cursor_Type) return String
   is
      From   : Positive renames Cursor.From;
      Length : Natural  renames Cursor.Length;
   begin
      return S(From - 1 .. From - 1 + Length - 1);
   end Substring;

   S   : constant String := "abracadabra";
   Sub : constant String := "bra";
   Len : constant Positive := Sub'Length;

   C   : Cursor_Type;
   Ref : Hash_Type;
begin
   C := New_Cursor(Sub, Sub'Length);
   Roll(Sub, C);
   Ref := Hash(C);
   Put_Line("Ref: "& Ref'Img &" "& Substring(Sub, C));

   New_Line;

   C := New_Cursor(S, Sub'Length);
   while not Done(S, C) loop
      Roll(S, C);
      Put_Line("Hash:"& Hash(C)'Img &" "& Substring(S, C));
      if Hash(C) = Ref then
         Put_Line("Ding ding ding!");
         if Substring(S, C) = Sub then
            Put_Line("Jabadabaduuu");
         end if;
      end if;
   end loop;
end;

