-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Check;

with DB.Utils.Gen_Integer_Image;

procedure DB.Tables.Maps.Gen_Check
  (Map : in out Map_Type)
is
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
         null;
   end case;
end DB.Tables.Maps.Gen_Check;

