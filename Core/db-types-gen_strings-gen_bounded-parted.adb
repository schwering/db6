-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Parted is

   Item_Size : constant IO.Blocks.Size_Type
             := IO.Blocks.Bits_To_Units(Item_Type'Size);

   function Size_Bound
     (S : String_Type)
      return IO.Blocks.Size_Type
   is
      use type DB.IO.Blocks.Size_Type;
   begin
      return Item_Size * IO.Blocks.Size_Type(S.Length);
   end Size_Bound;


   function Fold_Contexts
     (Left     : Context_Type;
      Appended : Context_Type)
      return Context_Type is
   begin
      return (Left.Length + Appended.Length, Left.First and Appended.First);
   end Fold_Contexts;


   procedure Read_Context
     (Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Context :    out Context_Type)
   is
      procedure Read is new IO.Blocks.Read(Length_Type);
   begin
      Read(Block, Cursor, Context.Length);
      Context.First := True;
   end Read_Context;


   procedure Write_Context
     (Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Context : in     Context_Type)
   is
      procedure Write is new IO.Blocks.Write(Length_Type);
   begin
      Write(Block, Cursor, Context.Length);
   end Write_Context;


   procedure Read_Part_Of_String
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in out String_Type;
      Done    :    out Boolean) is
   begin
      if Context.First then
         Context.Length := 0;
         Context.First  := False;
      end if;
      declare
         use type IO.Blocks.Size_Type;
         procedure Read is new IO.Blocks.Read(Item_Type);

         Block_Space  : constant Length_Type :=
            Length_Type(IO.Blocks.Remaining_Space(Block, Cursor) / Item_Size);
         Remaining    : constant Length_Type
                      := Context.Length - S.Length;
         Read_Amount  : constant Length_Type
                      := Length_Type'Min(Block_Space, Remaining);
         From         : constant Index_Type
                      := S.Length + 1;
         To           : constant Index_Type
                      := From + Read_Amount - 1;
      begin
         pragma Assert (Context.Length >= 0);
         pragma Assert (Remaining <= Context.Length);
         pragma Assert (From in 1    .. Context.Length);
         pragma Assert (To   in From .. Context.Length);
         for I in From .. To loop
            Read(Block, Cursor, S.Buffer(I));
         end loop;
         Done := Length(S) = Context.Length;
      end;
   end Read_Part_Of_String;


   procedure Write_Part_Of_String
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type;
      Done    :    out Boolean) is
   begin
      if Context.First then
         Context.Length := 0;
         Context.First  := False;
      end if;
      declare
         use type IO.Blocks.Size_Type;
         procedure Write is new IO.Blocks.Write(Item_Type);

         Block_Space  : constant Length_Type :=
            Length_Type(IO.Blocks.Remaining_Space(Block, Cursor) / Item_Size);
         Remaining    : constant Length_Type
                      := S.Length - Context.Length;
         Write_Amount : constant Length_Type
                      := Length_Type'Min(Block_Space, Remaining);
         From         : constant Index_Type
                      := Context.Length + 1;
         To           : constant Index_Type
                      := From + Write_Amount - 1;
      begin
         pragma Assert (Context.Length >= 0);
         pragma Assert (Remaining <= S.Length);
         pragma Assert (From in 1 .. S.Length);
         pragma Assert (To   in From .. S.Length);
         for I in From .. To loop
            Write(Block, Cursor, S.Buffer(I));
         end loop;
         Context.Length := Context.Length + Write_Amount;
         Done           := Length(S) = Context.Length;
      end;
   end Write_Part_Of_String;

end Parted;

