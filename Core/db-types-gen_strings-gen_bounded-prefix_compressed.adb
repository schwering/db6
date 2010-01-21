-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;
with DB.Compression.Gen_Prefix;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Prefix_Compressed is

   package Compression is new DB.Compression.Gen_Prefix
     (Item_Type       => Item_Type,
      String_Type     => String_Type,
      Index_Type      => Index_Type,
      Length_Type     => Length_Type,
      Empty_String    => Empty_String,
      To_Index        => To_Index,
      New_String      => New_String,
      Length          => Length,
      Element         => Element,
      Substring       => Substring,
      "="             => "=");


   type String_Info_Type is
      record
         Compressed : Boolean;
         Length     : IO.Blocks.Base_Position_Type;
      end record;
   pragma Pack (String_Info_Type);

   type Prefix_Info_Type is
      record
         Block_Position : IO.Blocks.Base_Position_Type;
         Length         : IO.Blocks.Base_Position_Type;
      end record;
   pragma Pack (Prefix_Info_Type);


   function Size_Bound
     (S : String_Type)
      return IO.Blocks.Size_Type
   is
      use type IO.Blocks.Size_Type;
      type String_Buffer_Type is new Indefinite_Buffer_Type(1 .. Length(S));
      function Size_Of is new IO.Blocks.Size_Of(String_Info_Type);
      function Size_Of is new IO.Blocks.Size_Of(String_Buffer_Type);
   begin
      return Size_Of(String_Info_Type'(others => <>)) +
             Size_Of(String_Buffer_Type(S.Buffer(1 .. Length(S))));
   end Size_Bound;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      use type IO.Blocks.Size_Type;
      use type IO.Blocks.Base_Position_Type;
      subtype LPos_Type is IO.Blocks.Base_Position_Type;

      procedure Write_Uncompressed
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type)
      is
         pragma Unreferenced (Context);
         type String_Buffer_Type is
            new Indefinite_Buffer_Type(1 .. Length(S));
         procedure Write is new IO.Blocks.Write(String_Info_Type);
         procedure Write is new IO.Blocks.Write(String_Buffer_Type);

         SI : constant String_Info_Type
            := (Compressed => False,
                Length     => LPos_Type(Length(S)));
      begin
         Write(Block, Cursor, SI);
         Write(Block, Cursor, String_Buffer_Type(S.Buffer(1 .. Length(S))));
      end Write_Uncompressed;

      procedure Write_Compressed
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S_Delta : in     Compression.Delta_Type)
      is
         type Postfix_Buffer_Type is
            new Indefinite_Buffer_Type(1 .. Length(S_Delta.Postfix));
         procedure Write is new IO.Blocks.Write(String_Info_Type);
         procedure Write is new IO.Blocks.Write(Prefix_Info_Type);
         procedure Write is new IO.Blocks.Write(Postfix_Buffer_Type);

         SI : constant String_Info_Type
            := (Compressed => True,
                Length     => LPos_Type(Length(S_Delta.Postfix)));
         PI : constant Prefix_Info_Type
            := (Block_Position => Context.Previous_Block_Position,
                Length         => LPos_Type(S_Delta.Prefix_Length));
      begin
         Write(Block, Cursor, SI);
         Write(Block, Cursor, PI);
         Write(Block, Cursor,
               Postfix_Buffer_Type(S_Delta.Postfix.Buffer
                                           (1 .. Length(S_Delta.Postfix))));
      end Write_Compressed;

      Previous_Position : constant IO.Blocks.Base_Position_Type
                        := IO.Blocks.Position(Cursor);
   begin
      if not Context.Initialized then
         Write_Uncompressed(Context, Block, Cursor, S);
         Context.Initialized             := True;
         Context.Previous                := S;
         Context.Previous_Block_Position := Previous_Position;
      else
         declare
            S_Delta : constant Compression.Delta_Type
                    := Compression.Encode(Context.Previous, S);

            type Prefix_Buffer_Type is
               new Indefinite_Buffer_Type(1 .. S_Delta.Prefix_Length);
            function Size_Of is new IO.Blocks.Size_Of(Prefix_Info_Type);
            function Size_Of is new IO.Blocks.Size_Of(Prefix_Buffer_Type);
         begin
            if Size_Of(Prefix_Info_Type'(others => <>)) >
               Size_Of(Prefix_Buffer_Type'(others => <>)) then
               Write_Uncompressed(Context, Block, Cursor, S);
            else
               Write_Compressed(Context, Block, Cursor, S_Delta);
            end if;
         end;
         Context.Previous                := S;
         Context.Previous_Block_Position := Previous_Position;
      end if;
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      use type IO.Blocks.Size_Type;

      procedure Read_Uncompressed
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type;
         Length  : in     Length_Type)
      is
         pragma Unreferenced (Context);
         type String_Buffer_Type is
            new Indefinite_Buffer_Type(1 .. Length);
         procedure Read is new IO.Blocks.Read(String_Buffer_Type);
      begin
         Read(Block, Cursor, String_Buffer_Type(S.Buffer(1 .. Length)));
         S.Length := Length;
      end Read_Uncompressed;

      procedure Read_Compressed
        (Context        : in out Context_Type;
         Block          : in     IO.Blocks.Base_Block_Type;
         Cursor         : in out IO.Blocks.Cursor_Type;
         S              :    out String_Type;
         Postfix_Length : in     Length_Type)
      is
         type Postfix_Buffer_Type is
            new Indefinite_Buffer_Type(1 .. Postfix_Length);
         procedure Read is new IO.Blocks.Read(Prefix_Info_Type);
         procedure Read is new IO.Blocks.Read(Postfix_Buffer_Type);
         PI      : Prefix_Info_Type;
         Prefix  : String_Type;
      begin
         Read(Block, Cursor, PI);
         declare
            Prefix_Cursor : IO.Blocks.Cursor_Type
                          := IO.Blocks.New_Cursor(PI.Block_Position);
         begin
            Read(Context, Block, Prefix_Cursor, Prefix);
         end;
         declare
            S_Delta : Compression.Delta_Type;
         begin
            S_Delta.Prefix_Length := Length_Type(PI.Length);
            Read(Block, Cursor,
                 Postfix_Buffer_Type(S_Delta.Postfix.Buffer
                                       (1 .. Postfix_Length)));
            S_Delta.Postfix.Length := Postfix_Length;
            S := Compression.Decode(Prefix, S_Delta);
         end;
      end Read_Compressed;

      procedure Read is new IO.Blocks.Read(String_Info_Type);
      SI : String_Info_Type;
   begin
      Read(Block, Cursor, SI);
      if not SI.Compressed then
         Read_Uncompressed(Context, Block, Cursor, S, Length_Type(SI.Length));
      else
         Read_Compressed(Context, Block, Cursor, S, Length_Type(SI.Length));
      end if;
   end Read;

end Prefix_Compressed;

