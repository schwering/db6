-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Check;

with DB.Utils.Gen_Integer_Image;

procedure DB.Gen_Blob_Trees.Gen_Check
  (Tree : in out Tree_Type)
is
   function Value_To_String (V : BTree_Utils.Value_Type) return String
   is
      function Address_To_String is new
         Utils.Gen_Integer_Image(Heaps.Address_Type);
   begin
      if V.Direct then
         return Value_To_String(V.Value);
      else
         return Address_To_String(V.Address);
      end if;
   end Value_To_String;

   function Address_To_String is new
      Utils.Gen_Integer_Image(Block_IO.Address_Type);

   procedure Check is new BTrees.Gen_Check
     (Key_To_String      => Key_To_String,
      Value_To_String    => Value_To_String,
      Address_To_String  => Address_To_String);
begin
   Check(Tree.BTree);
end DB.Gen_Blob_Trees.Gen_Check;

