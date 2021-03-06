-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;
with DB.Compression.Gen_Prefix;

separate (DB.Types.Gen_Strings.Gen_Unbounded)
package body Prefix_Compressed is

   package Compression is new DB.Compression.Gen_Prefix
     (Item_Type       => Item_Type,
      String_Type     => String_Type,
      Index_Type      => Index_Type,
      Length_Type     => Length_Type,
      Empty_String    => Empty_String,
      New_String      => New_String,
      To_Index        => To_Index,
      Length          => Length,
      Element         => Element,
      Substring       => Substring,
      "="             => "=");


   function New_Context
      return Context_Type is
   begin
      return Context_Type'(others => <>);
   end New_Context;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
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
      Block   : in     IO.Blocks.Base_Block_Type;
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

