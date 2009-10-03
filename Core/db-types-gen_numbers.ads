with DB.IO.Blocks;

with System.Storage_Elements;

generic
   type P_Number_Type is (<>);
package DB.Types.Gen_Numbers is
   pragma Pure;

   type Number_Type is new P_Number_Type;

   type Context_Type is private;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Size_Of
     (Number : Number_Type)
      return IO.Blocks.Size_Type;

   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  : in     Number_Type);

   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  :    out Number_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);

   function To_Storage_Array
     (Number : Number_Type)
      return System.Storage_Elements.Storage_Array;

   function From_Storage_Array
     (Arr : System.Storage_Elements.Storage_Array)
      return Number_Type;

private
   type Context_Type is null record;

   pragma Inline (Size_Of);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Gen_Numbers;

