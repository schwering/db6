-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO;

with DB.Compression.Gen_Levenshtein;
with DB.Types;

procedure Levenshtein
is

   generic
      type Item_Type is (<>);
      with function "=" (A, B : Item_Type) return Boolean is <>;
      type Item_Array_Type is array (Positive range <>) of Item_Type;
      with function Image (A : Item_Array_Type) return String;
   procedure Test (S, T : in Item_Array_Type);


   procedure Test (S, T : in Item_Array_Type)
   is
      package Levenshtein is new DB.Compression.Gen_Levenshtein
        (Item_Type             => Item_Type,
         Item_Array_Type       => Item_Array_Type,
         Max_Length            => 255);
      use Levenshtein;
      D : constant Delta_Type      := Encode(S, T);
      U : constant Item_Array_Type := Decode(S, D);
      B : constant Boolean         := T = U;
      Compression_Error : exception;
   begin
      Ada.Text_IO.Put(Image(T) &" = "& Image(U));
      Ada.Text_IO.Put_Line(" = "& Boolean'Image(B));
      Ada.Text_IO.Put_Line("Sizeof(T) ="& Integer'Image(T'Size)
                         &" Sizeof(D) ="& Integer'Image(D'Size));
      if not B then raise Compression_Error; end if;
   end Test;


   procedure S_Test_Both (S, T : in String)
   is
      function Image (S : String) return String is
      begin
         return S;
      end Image;

      procedure S_Test is new Test(Character, Standard."=",
                                   String, Image);
   begin
      S_Test(S, T);
      S_Test(T, S);
   end S_Test_Both;


   procedure L_Test_Both (S, T : in DB.Types.Letter_Array_Type)
   is
      function Image (S : DB.Types.Letter_Array_Type) return String
      is
         T : String(S'Range);
      begin
         for I in S'Range loop
            T(I) := DB.Types.Letter_Type'Image(S(I))(2);
         end loop;
         return T;
      end Image;

      procedure L_Test is new Test(DB.Types.Letter_Type,
                                   DB.Types."=",
                                   DB.Types.Letter_Array_Type,
                                   Image);
   begin
      L_Test(S, T);
      L_Test(T, S);
   end L_Test_Both;

begin
   S_Test_Both("Tuer", "Tuer");
   S_Test_Both("Tier", "Tor");
   S_Test_Both("Tuer", "Tor");
   S_Test_Both("Tier", "Tuer");
   S_Test_Both("Tier", "Papa");
   S_Test_Both("Fummel", "Fummelei");
   S_Test_Both("ummel", "Fummelei");
   S_Test_Both("a", "aa");
   S_Test_Both("ab", "aa");
   S_Test_Both("http://www.cnn.com", "http://www.cnn.com/index.html");
   S_Test_Both("http://www.cnn.com", "http://www.cnn.com/i");
   L_Test_Both("http://www.cnn.com", "http://www.cnn.com/index.html");
   L_Test_Both("http://www.cnn.com", "http://www.cnn.com/i");
end Levenshtein;

