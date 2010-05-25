-- Abstract:
--
-- Unbounded string type.
-- Pretty similar to Ada.Strings.Unbounded.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

with DB.Blocks;

generic
package DB.Types.Gen_Strings.Gen_Unbounded is
   pragma Preelaborate;

   type String_Type is new Ada.Finalization.Controlled with private;

   Empty_String : constant String_Type;

   overriding procedure Initialize (String : in out String_Type);
   overriding procedure Adjust (String : in out String_Type);
   overriding procedure Finalize (String : in out String_Type);

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
     (Length : Length_Type)
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


   package Uncompressed is
      type Context_Type is null record;
      subtype Read_Context_Type is Context_Type;
      subtype Write_Context_Type is Context_Type;

      Is_Context_Free_Serialization : constant Boolean := True;

      function New_Read_Context
         return Read_Context_Type;

      function New_Write_Context
         return Write_Context_Type;

      function Size_Bound
        (S : String_Type)
         return Blocks.Size_Type;

      procedure Write
        (Block   : in out Blocks.Base_Block_Type;
         Cursor  : in out Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Write
        (Context : in out Write_Context_Type;
         Block   : in out Blocks.Base_Block_Type;
         Cursor  : in out Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Block   : in     Blocks.Base_Block_Type;
         Cursor  : in out Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Read
        (Context : in out Read_Context_Type;
         Block   : in     Blocks.Base_Block_Type;
         Cursor  : in out Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Skip
        (Context : in out Read_Context_Type;
         Block   : in     Blocks.Base_Block_Type;
         Cursor  : in out Blocks.Cursor_Type);

   private
      pragma Inline (New_Read_Context);
      pragma Inline (New_Write_Context);
      pragma Inline (Size_Bound);
      pragma Inline (Size_Bound);
      pragma Inline (Write);
      pragma Inline (Read);
      pragma Inline (Skip);
   end Uncompressed;

private
   type Refcount_Type is new Positive;

   type Bounded_String_Type (Length : Natural) is
      record
         Refcount : Refcount_Type;
         Buffer   : Indefinite_Buffer_Type(1 .. Length);
      end record;

   type Bounded_String_Ref_Type is access Bounded_String_Type;
   pragma Controlled (Bounded_String_Ref_Type);

   type String_Type is new Ada.Finalization.Controlled with
      record
         S : Bounded_String_Ref_Type;
      end record;

   Empty_String : constant String_Type
                := String_Type'(Ada.Finalization.Controlled with
                                S => null);

   pragma Inline (Initialize);
   pragma Inline (Adjust);
   pragma Inline (Finalize);
   pragma Inline ("<");
   pragma Inline ("=");
   pragma Inline ("&");
   pragma Inline (New_String);
   pragma Inline (To_Index);
   pragma Inline (Length);
   pragma Inline (Element);
   pragma Inline (Substring);

end DB.Types.Gen_Strings.Gen_Unbounded;

