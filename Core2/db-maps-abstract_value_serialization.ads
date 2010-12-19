-- Abstract:
--
-- The IO implementation of the abstract Value_Type interface.
--
-- The Value_Wrapper_Type is intended to be the actual value type of the
-- B-trees. It holds and manages a pointer to a Value_Type'Class object.
--
-- We use the tag of this managed object to dispatch to the right IO procedures
-- (in fact, only the Read procedure is non-trivial):
--
-- The Write procedure writes the tag and then simply dispatches to the managed
-- object's Write procedure.
--
-- The Read procedure reads the tag and then uses this tag to create a new
-- object of the right type with Ada.Tags.Generic_Dispatching_Constructor. This
-- object then works as prototype to dispatch to the right Read procedure.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

with DB.Blocks;
with DB.Blocks.Gen_Values_Signature;

private
package DB.Maps.Abstract_Value_Serialization is
   pragma Elaborate_Body;

   subtype Value_Class_Type is Value_Type'Class;
   type Value_Class_Ref_Type is access Value_Class_Type'Class;

   type Value_Wrapper_Type is new Ada.Finalization.Controlled with
      record
         Ref : Value_Class_Ref_Type;
      end record;

   function New_Value_Wrapper
     (Value : Value_Class_Type)
      return Value_Wrapper_Type;

   overriding
   procedure Initialize (Value : in out Value_Wrapper_Type);

   overriding
   procedure Adjust (Value : in out Value_Wrapper_Type);

   overriding
   procedure Finalize (Value : in out Value_Wrapper_Type);

   type Read_Context_Type is null record;
   type Write_Context_Type is null record;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Value : Value_Wrapper_Type) return Blocks.Size_Type;

   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   :    out Value_Wrapper_Type);

   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   : in     Value_Wrapper_Type);

   package Values_Signature is new Blocks.Gen_Values_Signature
     (Value_Type         => Value_Wrapper_Type,
      Read_Context_Type  => Read_Context_Type,
      Write_Context_Type => Write_Context_Type);

end DB.Maps.Abstract_Value_Serialization;

