-- Abstract:
--
-- Bounded string type.
-- Pretty similar to Ada.Strings.Bounded.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;
with DB.Utils;

generic
   Max_Length : in Positive := 1024;
package DB.Types.Gen_Strings.Gen_Bounded is
   pragma Preelaborate;

   subtype Length_Type is Gen_Strings.Length_Type range 0 .. Max_Length;
   subtype Index_Type is Gen_Strings.Index_Type range 1 .. Length_Type'Last;

   type String_Type is private;

   Empty_String : constant String_Type;

   function Compare
     (Left, Right : String_Type)
      return Utils.Comparison_Result_Type;
   function "<" (Left, Right : String_Type) return Boolean;
   function "=" (Left, Right : String_Type) return Boolean;
   function "<=" (Left, Right : String_Type) return Boolean;
   function "&" (Left, Right : String_Type) return String_Type;
   function "&"
     (Left  : String_Type;
      Right : Indefinite_Buffer_Type)
      return String_Type;
   function "&"
     (Left  : Indefinite_Buffer_Type;
      Right : String_Type)
      return String_Type;

   function To_Index
     (L : Length_Type)
      return Index_Type;

   function New_String
     (Arr : Indefinite_Buffer_Type)
      return String_Type;

   function New_String
     (S        : String_Type;
      S_From   : Index_Type;
      S_Length : Length_Type;
      T        : String_Type)
      return String_Type;

   function Length
     (S : String_Type)
      return Length_Type;

   function Element
     (S : String_Type;
      I : Index_Type)
      return Item_Type;

   function Substring
     (S      : String_Type;
      From   : Index_Type;
      Length : Length_Type)
      return String_Type;

   function To_Buffer
     (S : String_Type)
      return Indefinite_Buffer_Type;

   function Short_Bound
     (Left  : String_Type)
      return String_Type;

   function Short_Delimiter
     (Left  : String_Type;
      Right : String_Type)
      return String_Type;


   package Uncompressed is
      type Context_Type is null record;

      Is_Context_Free_Serialization : constant Boolean := True;

      function New_Context
         return Context_Type;

      function Size_Bound
        (S : String_Type)
         return IO.Blocks.Size_Type;

      procedure Write
        (Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Write
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Read
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Skip
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

   private
      type Indefinite_Packed_Buffer_Type is
         array (Positive range <>) of Item_Type;
      pragma Pack (Indefinite_Packed_Buffer_Type);
      subtype Packed_Buffer_Type is Indefinite_Packed_Buffer_Type(Index_Type);

      pragma Inline (New_Context);
   end Uncompressed;


   package Prefix_Compressed is
      type Context_Type is
         record
            Initialized             : Boolean := False;
            Previous                : String_Type;
            Previous_Block_Position : IO.Blocks.Base_Position_Type;
         end record;

      Is_Context_Free_Serialization : constant Boolean := False;

      function New_Context
         return Context_Type;

      function Size_Bound
        (S : String_Type)
         return IO.Blocks.Size_Type;

      procedure Write
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

   private
      pragma Inline (New_Context);
   end Prefix_Compressed;


   package Delta_Compressed is
      type Context_Type is
         record
            Initialized : Boolean := False;
            Previous    : String_Type;
         end record;

      Is_Context_Free_Serialization : constant Boolean := False;

      function New_Context
         return Context_Type;

      procedure Write
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

   private
      pragma Inline (New_Context);
   end Delta_Compressed;


   package Parted is
      type Context_Type is
         record
            Length : Length_Type := 0;
            First  : Boolean     := True;
         end record;

      function New_Context
         return Context_Type;

      function Size_Bound
        (S : String_Type)
         return IO.Blocks.Size_Type;

      function Fold_Contexts
        (Left     : Context_Type;
         Appended : Context_Type)
         return Context_Type;

      function Context_Size_Bound
        (C : Context_Type)
         return IO.Blocks.Size_Type;

      procedure Read_Context
        (Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Context :    out Context_Type);

      procedure Write_Context
        (Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Context : in     Context_Type);

      procedure Read_Part_Of_String
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in out String_Type;
         Done    :    out Boolean);

      procedure Write_Part_Of_String
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type;
         Done    :    out Boolean);

   private
      pragma Inline (New_Context);
   end Parted;

private
   subtype Buffer_Type is Indefinite_Buffer_Type(Index_Type);
   type String_Type is
      record
         Buffer : Buffer_Type;
         Length : Length_Type;
      end record;

   Empty_String : constant String_Type
                := String_Type'(Length => 0, others => <>);

   pragma Inline (New_String);
   pragma Inline (To_Index);
   pragma Inline (Length);
   pragma Inline (Element);
   pragma Inline (Substring);

end DB.Types.Gen_Strings.Gen_Bounded;

