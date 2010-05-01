-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Check;
--with DB.Gen_Blob_Trees.Gen_Check;
with DB.Utils.Gen_Integer_Image;
with DB.Utils.Gen_String_Image;

procedure DB.Tables.Maps.Check
  (Map : in out Map_Type)
is
   function Key_To_String (Key : Key_Type) return String
   is
      function Row_Image is new
         Utils.Gen_String_Image(Types.Keys.Rows.String_Type);
      function Column_Image is new
         Utils.Gen_String_Image(Types.Keys.Columns.String_Type);
   begin
      return "("& Row_Image(Key.Row) &", "& Column_Image(Key.Column) &", "&
             Key.Time'Img &")";
   end Key_To_String;

   function Value_To_String
     (Value : Types.Values.Bounded.String_Type)
      return String
   is
      pragma Unreferenced (Value);
   begin
      return "(BoundedString)";
   end Value_To_String;

   function Value_To_String
     (Value : Types.Values.Unbounded.String_Type)
      return String
   is
      pragma Unreferenced (Value);
   begin
      return "(UnboundedString)";
   end Value_To_String;

   function Address_To_String is new
      Utils.Gen_Integer_Image(Block_IO_Impl.Address_Type);
begin
   case Map.Short is
      when True =>
         declare
            procedure Check is new BTrees.Gen_Check
              (Key_To_String      => Key_To_String,
               Value_To_String    => Value_To_String,
               Address_To_String  => Address_To_String);
         begin
            Check(Map.Short_Tree);
         end;
      when False =>
         declare
            procedure Check is new Blob_Trees.Gen_Check
              (Key_To_String   => Key_To_String,
               Value_To_String => Value_To_String,
               Address_To_String  => Address_To_String); -- XXX remove
         begin
            Check(Map.Long_Tree);
         end;
   end case;
end DB.Tables.Maps.Check;

