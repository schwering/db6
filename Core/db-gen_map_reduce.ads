-- Abstract:
--
-- An implementation of map / reduce that guarantees to handle the
-- elements in the right order.
--
-- References:
--
-- HCs paper, Google paper, Haskell?
--
-- Design Notes:
--
-- TODO
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks;
with DB.IO.Blocks.Gen_IO;

generic
   type In_Key_Type is private;
   type In_Value_Type is private;
   with procedure Input
          (Key     : out In_Key_Type;
           Value   : out In_Value_Type;
           Success : out Boolean);

   type Intermediate_Key_Type is private;
   type Intermediate_Value_Type is private;
   with function "=" (Left, Right : Intermediate_Key_Type) return Boolean is <>;
   with function "<=" (Left, Right : Intermediate_Key_Type)
           return Boolean is <>;
   with function Intermediate_Key_Size_Bound
          (Key : Intermediate_Key_Type)
           return IO.Blocks.Size_Type;
   with procedure Read_Intermediate_Key
          (Block  : in     IO.Blocks.Base_Block_Type;
           Cursor : in out IO.Blocks.Cursor_Type;
           Key    :    out Intermediate_Key_Type);
   with procedure Write_Intermediate_Key
          (Block  : in out IO.Blocks.Base_Block_Type;
           Cursor : in out IO.Blocks.Cursor_Type;
           Key    : in     Intermediate_Key_Type);
   with function Intermediate_Value_Size_Bound
          (Value : Intermediate_Value_Type)
           return IO.Blocks.Size_Type;
   with procedure Read_Intermediate_Value
          (Block  : in     IO.Blocks.Base_Block_Type;
           Cursor : in out IO.Blocks.Cursor_Type;
           Value  :    out Intermediate_Value_Type);
   with procedure Write_Intermediate_Value
          (Block  : in out IO.Blocks.Base_Block_Type;
           Cursor : in out IO.Blocks.Cursor_Type;
           Value  : in     Intermediate_Value_Type);
   with package Intermediate_Block_IO is new IO.Blocks.Gen_IO (<>);

   with procedure Map
          (Key   : in     In_Key_Type;
           Value : in     In_Value_Type;
           Emit  : access procedure (Key   : Intermediate_Key_Type;
                                     Value : Intermediate_Value_Type));

   type Out_Key_Type is private;
   type Out_Value_Type is private;
   with procedure Reduce
          (Key         : in     Intermediate_Key_Type;
           Next_Values : access procedure (Value : out Intermediate_Value_Type;
                                           Success : out Boolean);
           Out_Key     :    out Out_Key_Type;
           Out_Value   :    out Out_Value_Type);
   with procedure Output
          (Key   : in Out_Key_Type;
           Value : in Out_Value_Type);

   Map_Task_Count    : in Positive := 10;
   Reduce_Task_Count : in Positive := 10;
procedure DB.Gen_Map_Reduce;
pragma Preelaborate (DB.Gen_Map_Reduce);

