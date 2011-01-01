-- Abstract:
--
-- Tag maps can be used to have shorter (in count of bytes) representations of
-- tags when the considered set of tags is limited.
-- The assigned Tid (type identifier) to a tag depends on 
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Tags;

package DB.Maps.Tag_Map is
   pragma Elaborate_Body;

   type Tid_Type is range 1 .. 8; -- mod 2**3;
   for Tid_Type'Size use 8;

   procedure Register (Tag : Ada.Tags.Tag);

   procedure Seal;

   function To_Tid (Tag : Ada.Tags.Tag) return Tid_Type;
   function To_Tag (Tid : Tid_Type) return Ada.Tags.Tag;

   procedure Clear;

private
   Max_Tag_Length : constant := 128;

   type Item_Type is
      record
         Valid : Boolean;
         Tag   : Ada.Tags.Tag;
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

   type Map_Type is array (Tid_Type) of Item_Type;

   Sealed : Boolean       := False;
   Head   : Node_Ref_Type := null;
   Tail   : Node_Ref_Type := null;
   Map    : Map_Type;

end DB.Maps.Tag_Map;

