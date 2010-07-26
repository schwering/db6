-- Abstract:
--
-- Tag maps can be used to have shorter (in count of bytes) representations of
-- tags when the considered set of tags is limited.
-- The assigned TID (type identifier) to a tag depends on 
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags;

package DB.Maps.Tag_Map is

   type TID_Type is private;

   procedure Register_Tag (Tag : Ada.Tags.Tag);

   procedure Seal;

   function To_TID (Tag : Ada.Tags.Tag) return TID_Type;
   function To_Tag (TID : TID_Type) return Ada.Tags.Tag;

private
   type TID_Type is mod 2**3;
   for TID_Type'Size use 8;

   Max_Tag_Length : constant := 128;

   type Item_Type is
      record
         Valid : Boolean;
         Str   : String (1 .. Max_Tag_Length);
         Len   : Natural;
      end record;

   function "<" (I, J : Item_Type) return Boolean;
   function "=" (I, J : Item_Type) return Boolean;
   function "=" (I : Item_Type; S : String) return Boolean;

   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   type Node_Type is
      record
         Item : Item_Type;
         Prev : Node_Ref_Type;
         Next : Node_Ref_Type;
      end record;

   Sealed : Boolean       := False;
   Head   : Node_Ref_Type := null;
   Tail   : Node_Ref_Type := null;

   Map : array (TID_Type) of Item_Type;

end DB.Maps.Tag_Map;

