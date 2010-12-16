with Ada.Exceptions;

package REST.Error_Log is

   procedure Push (Msg : in String);
   procedure Push (Exc : in Ada.Exceptions.Exception_Occurrence);
   function Has return Boolean;
   function Top return String;
   procedure Pop;

private
   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   type Node_Type (Length : Natural) is
      record
         Message : String (1 .. Length);
         Next    : Node_Ref_Type := null;
      end record;

   Head : Node_Ref_Type := null;
   Tail : Node_Ref_Type := null;

end REST.Error_Log;

