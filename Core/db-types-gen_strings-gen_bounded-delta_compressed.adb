-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks;
with DB.Compression.Gen_Levenshtein;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Delta_Compressed is

   package Compression is new DB.Compression.Gen_Levenshtein
     (Item_Type       => Item_Type,
      Item_Array_Type => Indefinite_Buffer_Type,
      Max_Length      => Max_Length);


   type Meta_Data_Type is
      record
         Actions_Empty : Boolean;
         Chars_Empty   : Boolean;
      end record;
   pragma Pack (Meta_Data_Type);


   procedure Size_Of
     (Context : in out Context_Type;
      S       : in     String_Type;
      Size    :    out IO.Blocks.Size_Type)
   is

      function Size_Of
        (Diff : Compression.Delta_Type)
         return IO.Blocks.Size_Type
      is
         use type IO.Blocks.Size_Type;

         function Size_Of_Meta_Data is new IO.Blocks.Size_Of(Meta_Data_Type);

         subtype Action_Range is Positive range 1 .. Diff.Action_Count;
         type Definite_Long_Action_Array_Type is
            new Compression.Long_Action_Array_Type(Action_Range);
         function Size_Of_Actions is
            new IO.Blocks.Size_Of_Array(Action_Range,
                                        Compression.Long_Action_Type,
                                        Definite_Long_Action_Array_Type);

         subtype Items_Range is Positive range 1 .. Diff.Char_Count;
         type Definite_Item_Array_Type is array (Items_Range) of Item_Type;
         pragma Pack (Definite_Item_Array_Type);
         function Size_Of_Items is
            new IO.Blocks.Size_Of_Array(Items_Range, Item_Type,
                                        Definite_Item_Array_Type);

         Meta : constant Meta_Data_Type := (Diff.Actions'Length = 0,
                                            Diff.Chars'Length = 0);
      begin
         Size := Size_Of_Meta_Data(Meta);
         if Diff.Actions'Length > 0 then
            Size := Size +
                 Size_Of_Actions(Definite_Long_Action_Array_Type(Diff.Actions),
                                 1, Diff.Actions'Length);
         end if;
         if Diff.Chars'Length > 0 then
            Size := Size + Size_Of_Items(Definite_Item_Array_Type(Diff.Chars),
                                         1, Diff.Chars'Length);
         end if;
         return Size;
      end Size_Of;

   begin
      if not Context.Initialized then
         Context.Initialized := True;
         Context.Previous    := S;
         Size                := Uncompressed.Size_Of(S);
      else
         declare
            Prev : constant Indefinite_Buffer_Type
                   := Context.Previous.Buffer(1 .. Context.Previous.Length);
            Cur  : constant Indefinite_Buffer_Type := S.Buffer(1 .. S.Length);
            Diff : constant Compression.Delta_Type
                   := Compression.Encode(Prev, Cur);
         begin
            Context.Previous := S;
            Size             := Size_Of(Diff);
         end;
      end if;
   end Size_Of;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type)
   is

      procedure Write
        (Block  : in out IO.Blocks.Base_Block_Type;
         Cursor : in out IO.Blocks.Cursor_Type;
         Diff   : in     Compression.Delta_Type)
      is
         use type IO.Blocks.Size_Type;

         procedure Write_Meta_Data is new IO.Blocks.Write(Meta_Data_Type);

         subtype Action_Range is Positive range 1 .. Diff.Action_Count;
         type Definite_Long_Action_Array_Type is
            new Compression.Long_Action_Array_Type(Action_Range);
         procedure Write_Actions is
            new IO.Blocks.Write_Array(Action_Range,
                                      Compression.Long_Action_Type,
                                      Definite_Long_Action_Array_Type);

         subtype Items_Range is Positive range 1 .. Diff.Char_Count;
         type Definite_Item_Array_Type is array (Items_Range) of Item_Type;
         pragma Pack (Definite_Item_Array_Type);
         procedure Write_Items is
            new IO.Blocks.Write_Array(Items_Range, Item_Type,
                                      Definite_Item_Array_Type);

         Meta : constant Meta_Data_Type := (Diff.Actions'Length = 0,
                                            Diff.Chars'Length = 0);
      begin
          Write_Meta_Data(Block, Cursor, Meta);
          if Diff.Actions'Length > 0 then
             Write_Actions(Block, Cursor,
                           Definite_Long_Action_Array_Type(Diff.Actions),
                           1, Diff.Actions'Length);
          end if;
          if Diff.Chars'Length > 0 then
             Write_Items(Block, Cursor, Definite_Item_Array_Type(Diff.Chars),
                         1, Diff.Chars'Length);
          end if;
      end Write;

   begin
      if not Context.Initialized then
         Context.Initialized := True;
         Context.Previous    := S;
         Uncompressed.Write(Block, Cursor, S);
      else
         declare
            Prev : constant Indefinite_Buffer_Type
                   := Context.Previous.Buffer(1 .. Context.Previous.Length);
            Cur  : constant Indefinite_Buffer_Type := S.Buffer(1 .. S.Length);
            Diff : constant Compression.Delta_Type
                   := Compression.Encode(Prev, Cur);
         begin
            Write(Block, Cursor, Diff);
            Context.Previous := S;
         end;
      end if;
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      subtype Action_Range is Positive range 1 .. Max_Length;
      type Definite_Long_Action_Array_Type is
         new Compression.Long_Action_Array_Type(Action_Range);
      subtype Item_Range is Positive range 1 .. Max_Length;
      type Definite_Item_Array_Type is array (Item_Range) of Item_Type;
      pragma Pack (Definite_Item_Array_Type);

      procedure Read_Diff
        (Block          : in     IO.Blocks.Base_Block_Type;
         Cursor         : in out IO.Blocks.Cursor_Type;
         Actions        :    out Definite_Long_Action_Array_Type;
         Actions_Length :    out Action_Range'Base;
         Items          :    out Definite_Item_Array_Type;
         Items_Length   :    out Item_Range'Base)
      is
         use type IO.Blocks.Size_Type;

         procedure Read_Meta_Data is new IO.Blocks.Read(Meta_Data_Type);

         procedure Read_Actions is
            new IO.Blocks.Read_Array(Action_Range,
                                     Compression.Long_Action_Type,
                                     Definite_Long_Action_Array_Type);
         procedure Read_Items is
            new IO.Blocks.Read_Array(Item_Range, Item_Type,
                                     Definite_Item_Array_Type);

         Meta : Meta_Data_Type;
      begin
         Read_Meta_Data(Block, Cursor, Meta);
         if Meta.Actions_Empty then
            Actions_Length := 0;
         else
            Read_Actions(Block, Cursor, Actions, 1, Actions_Length);
         end if;
         if Meta.Chars_Empty then
            Items_Length := 0;
         else
            Read_Items(Block, Cursor, Items, 1, Items_Length);
         end if;
      end Read_Diff;

   begin
      if not Context.Initialized then
         Uncompressed.Read(Block, Cursor, S);
         Context.Initialized := True;
         Context.Previous    := S;
      else
         declare
            Prev      : constant Indefinite_Buffer_Type
                      := Context.Previous.Buffer(1 .. Context.Previous.Length);
            Actions   : Definite_Long_Action_Array_Type;
            Actions_Length : Compression.Length_Type;
            Items     : Definite_Item_Array_Type;
            Items_Length   : Compression.Length_Type;
         begin
            Read_Diff(Block, Cursor,
                      Actions, Actions_Length,
                      Items, Items_Length);
            declare
                Diff : constant Compression.Delta_Type
                     := (Action_Count => Actions_Length,
                         Char_Count   => Items_Length,
                         Actions      => Compression.Long_Action_Array_Type
                            (Actions(1 .. Actions_Length)),
                         Chars        => Indefinite_Buffer_Type
                            (Items(1 .. Items_Length)));
                Cur  : constant Indefinite_Buffer_Type
                     := Compression.Decode(Prev, Diff);
             begin
                S.Buffer(Cur'Range) := Cur;
                S.Length            := Cur'Length;
                Context.Previous    := S;
             end;
         end;
      end if;
   end Read;

end Delta_Compressed;

