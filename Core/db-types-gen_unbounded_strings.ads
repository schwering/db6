with Ada.Finalization;

with DB.IO.Blocks;

generic
   type Item_Type is private;
   with function "<" (Left, Right : Item_Type) return Boolean is <>;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
   with function ">" (Left, Right : Item_Type) return Boolean is <>;
package DB.Types.Gen_Unbounded_Strings is
   pragma Preelaborate;

   subtype Length_Type is Natural;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   type Buffer_Type is array (Positive range <>) of Item_Type;

   type String_Type is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (String : in out String_Type);
   overriding procedure Adjust (String : in out String_Type);
   overriding procedure Finalize (String : in out String_Type);

   function "<" (Left, Right : String_Type) return Boolean;
   function "=" (Left, Right : String_Type) return Boolean;
   function "&" (Left, Right : String_Type) return String_Type;
   function "&" (Left : String_Type; Right : Buffer_Type) return String_Type;
   function "&" (Left : Buffer_Type; Right : String_Type) return String_Type;

   function To_Index
     (L : Length_Type)
      return Index_Type;

   function Empty_String
     return String_Type;

   function New_String
     (Arr : Buffer_Type)
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

   function To_String
     (S : String_Type)
      return Buffer_Type;


   package Uncompressed is
      type Context_Type is null record;

      Is_Context_Free_Serialization : constant Boolean := True;

      function Size_Of
        (S : String_Type)
         return IO.Blocks.Size_Type;

      procedure Size_Of
        (Context : in out Context_Type;
         S       : in     String_Type;
         Size    :    out IO.Blocks.Size_Type);

      procedure Write
        (Block   : in out IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Write
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Block   : in     IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Read
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

      procedure Skip
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

   private
      pragma Inline (Uncompressed.Size_Of);
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

      procedure Size_Of
        (Context : in out Context_Type;
         S       : in     String_Type;
         Size    :    out IO.Blocks.Size_Type);

      procedure Write
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       : in     String_Type);

      procedure Read
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         S       :    out String_Type);

   private
      pragma Inline (Prefix_Compressed.Size_Of);
      pragma Inline (Prefix_Compressed.Write);
      pragma Inline (Prefix_Compressed.Read);
   end Prefix_Compressed;

private
   type Buffer_Ref_Type is access Buffer_Type;
   pragma Controlled (Buffer_Ref_Type);
   type Refcount_Type is new Positive;
   type Refcount_Ref_Type is access Refcount_Type;
   pragma Controlled (Refcount_Ref_Type);
   type String_Type is new Ada.Finalization.Controlled with
      record
         Buffer   : Buffer_Ref_Type;
         Refcount : Refcount_Ref_Type;
      end record;

   pragma Inline (Initialize);
   pragma Inline (Adjust);
   pragma Inline (Finalize);
   pragma Inline ("<");
   pragma Inline ("=");
   pragma Inline ("&");
   pragma Inline (Empty_String);
   pragma Inline (New_String);
   pragma Inline (To_Index);
   pragma Inline (Length);
   pragma Inline (Element);
   pragma Inline (Substring);

end DB.Types.Gen_Unbounded_Strings;

