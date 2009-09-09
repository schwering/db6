with Ada.Text_IO; use Ada.Text_IO;

with Gen_Containers;
with Gen_Containers.Gen_Vectors;

with Util.Gen_Refcounters;
with Util.Gen_Events;

procedure Vectors
is
   type Item_Type is new Integer;
   function Copy (Item : Item_Type) return Item_Type is begin return Item; end;
   procedure Finalize (Item : in out Item_Type) is null;

   package Containers is new Gen_Containers(Item_Type);
   package Vectors is new Containers.Gen_Vectors;
begin
   Put_Line("Huhu");
end;

