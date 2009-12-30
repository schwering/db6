-- Abstract:
--
-- Unbounded string type.
-- Pretty similar to Ada.Strings.Unbounded.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;
with System.Storage_Elements;

with DB.IO.Blocks;

generic
package DB.Types.Gen_Strings.Gen_Unbounded is
   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   type String_Type is new Ada.Finalization.Controlled with private;

   Empty_String : constant String_Type;

   overriding procedure Initialize (String : in out String_Type);
   overriding procedure Adjust (String : in out String_Type);
   overriding procedure Finalize (String : in out String_Type);

   function "<" (Left, Right : String_Type) return Boolean;
   function "=" (Left, Right : String_Type) return Boolean;
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

      Is_Context_Free_Serialization : constant Boolean := True;

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
      pragma Inline (Uncompressed.Size_Bound);
      pragma Inline (Uncompressed.Write);
      pragma Inline (Uncompressed.Read);
      pragma Inline (Uncompressed.Skip);
   end Uncompressed;


   package Prefix_Compressed is
      type Context_Type is
         record
            Initialized : Boolean := False;
            Previous    : String_Type;
         end record;

      Is_Context_Free_Serialization : constant Boolean := False;

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
      pragma Inline (Prefix_Compressed.Write);
      pragma Inline (Prefix_Compressed.Read);
   end Prefix_Compressed;


   package Parted is
      type Context_Type is
         record
            Length : Length_Type := 0;
            First  : Boolean     := True;
         end record;

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
   end Parted;

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

