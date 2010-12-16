with Ada.Unchecked_Deallocation;

package body REST.Error_Log is

   procedure Push (Msg : in String) is
      Node : constant Node_Ref_Type := new Node_Type'
        (Length  => Msg'Length,
         Message => Msg,
         Next    => null);
   begin
      if Head = null then
         pragma Assert (Tail = null);
         Head := Node;
         Tail := Node;
      else
         Tail.Next := Node;
         Tail      := Node;
      end if;
   end Push;


   procedure Push (Exc : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Push (Ada.Exceptions.Exception_Information (Exc));
   end Push;


   function Has return Boolean is
   begin
      return Head /= null;
   end Has;


   function Top return String is
   begin
      if Head = null then
         raise Constraint_Error;
      end if;

      return Head.Message;
   end Top;


   procedure Pop
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Type, Node_Ref_Type);
       Node : Node_Ref_Type := Head;
   begin
      if Head = null then
         raise Constraint_Error;
      end if;

      if Head = Tail then
         Tail := null;
      end if;
      Head := Head.Next;
      Free (Node);
   end Pop;

end REST.Error_Log;

