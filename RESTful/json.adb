with Ada.Unchecked_Deallocation;

package body JSON is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ref_Type);

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset) is
   begin
      null;
   end Read;


   procedure Close (Resource : in out Stream_Type) is
   begin
      null;
   end Close;


   function End_Of_File (Resource : Stream_Type) return Boolean is
   begin
      return Resource.Final;
   end End_Of_File;

end JSON;

