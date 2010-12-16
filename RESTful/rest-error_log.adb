with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body REST.Error_Log is

   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   type Node_Type (Length : Natural) is
      record
         Message : String (1 .. Length);
         Next    : Node_Ref_Type := null;
      end record;


   protected Logger is
      procedure Push (Msg : in String);
      function Has return Boolean;
      function Top return String;
      procedure Pop;

   private
      Head : Node_Ref_Type := null;
      Tail : Node_Ref_Type := null;
   end Logger;


   procedure Push (Msg : in String) is
      package C renames Ada.Calendar;
      package CF renames Ada.Calendar.Formatting;

      function Strip (S : String) return String is
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip;

      Now     : constant C.Time := C.Clock;
      Now_Str : constant String :=
         Strip (C.Year_Number'Image (CF.Year (Now))) & "-" &
         Strip (C.Month_Number'Image (CF.Month (Now))) & "-" &
         Strip (C.Day_Number'Image (CF.Day (Now))) & " " &
         Strip (CF.Hour_Number'Image (CF.Hour (Now))) & ":" &
         Strip (CF.Minute_Number'Image (CF.Minute (Now))) & ":" &
         Strip (CF.Second_Number'Image (CF.Second (Now)));
   begin
      --Logger.Push (Msg);
      Ada.Text_IO.Put_Line ("Error caught ("& Now_Str &"):");
      Ada.Text_IO.Put_Line (Msg);
      Ada.Text_IO.New_Line;
   end Push;


   procedure Push (Exc : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Push (Ada.Exceptions.Exception_Information (Exc));
   end Push;


   function Has return Boolean is
   begin
      return Logger.Has;
   end Has;


   function Top return String is
   begin
      return Logger.Top;
   end Top;


   procedure Pop is
   begin
      Logger.Pop;
   end Pop;


   protected body Logger is

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

   end Logger;

end REST.Error_Log;

