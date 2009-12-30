with Ada.Text_IO; use Ada.Text_IO;

procedure Modular
is
   type Uint64 is mod 2**64;
   type Uint63 is mod 2**63;
   type Uint32 is mod 2**32;
   type Uint6 is mod 2**6;
   --for Uint64'Size use 64;
   J : constant Uint32 := 0 - 1; --2#11111111_11111111_11111111_11111111#;
   K : constant Uint63 := 0 - 1; --2#11111111_11111111_11111111_11111111#;
   I : constant Uint64 := 0 - 1; --2#11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111# + 1;
   Index  : constant Uint6 := 53;
   Length : constant Uint6 := 20;
   Great  : constant Uint64 := Uint64'Last;
   Small  : constant Uint32 := Uint32(Great mod Uint32'Modulus);

   type Mod100 is mod 100;
   M100 : Mod100 := 90;
begin
   Put_Line(Mod100'Image(Mod100(0) - 1));
   for I in 1 .. 1000 loop
      Put_Line("Mod100 (90 +"& I'Img &") ="& Mod100'Image(M100+Mod100(I)));
   end loop;


   Put_Line("Natural'Size ="& Natural'Size'Img);
   Put_Line("Cast:"& Uint32'Image(Small));
   Put_Line(" "& Uint6'Image(Index));
   Put_Line(" "& Uint6'Image(Length));
   Put_Line(" "& Uint6'Image(Index+Length));
   Put_Line("Uint32'Size ="& Integer'Image(Uint32'Size));
   Put_Line("Uint32'Object_Size ="& Integer'Image(Uint32'Object_Size));
   Put_Line("Uint63'Size ="& Integer'Image(Uint63'Size));
   Put_Line("Uint63'Object_Size ="& Integer'Image(Uint63'Object_Size));
   Put_Line("Uint64'Size ="& Integer'Image(Uint64'Size));
   Put_Line("Uint64'Object_Size ="& Integer'Image(Uint64'Object_Size));
   Put_Line("J ="& Uint32'Image(J));
   Put_Line("J+1 ="& Uint32'Image(J+1));
   Put_Line("K ="& Uint63'Image(K));
   Put_Line("K+1 ="& Uint63'Image(K+1));
   Put_Line("I ="& Uint64'Image(I));
   Put_Line("I+1 ="& Uint64'Image(I+1));
end Modular;

