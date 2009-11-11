with DB.IO.Blocks;
with DB.Compression.Gen_Prefix;

separate (DB.Types.Gen_Unbounded_Strings)
package body Prefix_Compressed is

   package Compression is new DB.Compression.Gen_Prefix
     (Item_Type       => Item_Type,
      String_Type     => String_Type,
      Index_Type      => Index_Type,
      Length_Type     => Length_Type,
      Empty_String    => Empty_String,
      To_Index        => To_Index,
      Length          => Length,
      Element         => Element,
      Substring       => Substring,
      "="             => "=",
      "&"             => "&");



   procedure Size_Of
     (Context : in out Context_Type;
      S       : in     String_Type;
      Size    :    out IO.Blocks.Size_Type) is
   begin
      if not Context.Initialized then
         Context.Initialized := True;
         Context.Previous    := S;
         Size                := Uncompressed.Size_Of(S);
      else
         declare
            function Size_Of is new IO.Blocks.Size_Of(Length_Type);
            function Size_Of is new IO.Blocks.Size_Of(Boolean);
            Diff  : constant Compression.Delta_Type
                  := Compression.Encode(Context.Previous, S);
            Empty : constant Boolean := Diff.Postfix.Length = 0;
            use type IO.Blocks.Size_Type;
         begin
            Size := Size_Of(Diff.Prefix_Length) + Size_Of(Empty);
            if not Empty then
               Size := Size + Uncompressed.Size_Of(Diff.Postfix);
            end if;
            Context.Previous := S;
         end;
      end if;
   end Size_Of;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type) is
   begin
      if not Context.Initialized then
         Context.Initialized := True;
         Context.Previous    := S;
         Uncompressed.Write(Block, Cursor, S);
      else
         declare
            procedure Write is new IO.Blocks.Write(Length_Type);
            procedure Write is new IO.Blocks.Write(Boolean);
            Diff  : constant Compression.Delta_Type
                  := Compression.Encode(Context.Previous, S);
            Empty : constant Boolean := Diff.Postfix.Length = 0;
         begin
            Write(Block, Cursor, Diff.Prefix_Length);
            Write(Block, Cursor, Empty);
            if not Empty then
               Uncompressed.Write(Block, Cursor, Diff.Postfix);
            end if;
            Context.Previous := S;
         end;
      end if;
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       :    out String_Type) is
   begin
      if not Context.Initialized then
         Uncompressed.Read(Block, Cursor, S);
         Context.Initialized := True;
         Context.Previous    := S;
      else
         declare
            procedure Read is new IO.Blocks.Read(Length_Type);
            procedure Read is new IO.Blocks.Read(Boolean);
            Diff  : Compression.Delta_Type;
            Empty : Boolean;
         begin
            Read(Block, Cursor, Diff.Prefix_Length);
            Read(Block, Cursor, Empty);
            if Empty then
               Diff.Postfix := Empty_String;
            else
               Uncompressed.Read(Block, Cursor, Diff.Postfix);
            end if;
            S := Compression.Decode(Context.Previous, Diff);
            Context.Previous := S;
         end;
      end if;
   end Read;

end Prefix_Compressed;

