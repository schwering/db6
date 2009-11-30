-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

procedure DB.Gen_BTrees.Gen_Random_Map_Reduce
  (Tree               : in out Tree_Type;
   Transaction        : in out Transaction_Type'Class;
   Cursor             : in out Cursor_Type;
   Element            :    out Element_Type;
   State              :    out Result_Type;
   Concurrency_Degree : in     Positive := Default_Concurrency_Degree)
is
   task type Task_Type is
      entry Start;
      entry Is_Done (Element : out Element_Type; State : out Result_Type);
   end Task_Type;

   task body Task_Type
   is
      Element : Element_Type := Neutral_Element;
      Key     : Key_Type;
      Value   : Value_Type;
      State   : Result_Type;
   begin
      accept Start;
      loop
         Next(Tree, Transaction, Cursor, Key, Value, State);
         exit when State /= Success;
         Reduce(Element, Map(Key, Value));
      end loop;
      accept Is_Done (Element : out Element_Type; State : out Result_Type) do
         Element := Task_Type.Element;
         State   := Task_Type.State;
      end Is_Done;
   end Task_Type;

   Tasks : array (1 .. Concurrency_Degree) of Task_Type;
begin
   Element := Neutral_Element;
   State   := Success;

   for I in Tasks'Range loop
      Tasks(I).Start;
   end loop;

   -- Tasks work now

   for I in Tasks'Range loop
      declare
         Step_Element : Element_Type;
         Step_State   : Result_Type;
      begin
         Tasks(I).Is_Done(Step_Element, Step_State);
         Reduce(Element, Step_Element);
         if Step_State /= Success then
            State := Step_State;
         end if;
      end;
   end loop;
end DB.Gen_BTrees.Gen_Random_Map_Reduce;

