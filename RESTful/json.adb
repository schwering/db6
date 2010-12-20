with Ada.Unchecked_Deallocation;

package body JSON is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ref_Type);


   ----------
   -- Operations to manipulate the content of the stream.

   procedure Enqueue
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     Value_Type)
   is
      Key_Ref : constant String_Ref_Type := new String'(Key);
   begin
      Queues.Enqueue
        (Resource.Queue,
         Item_Type'(Is_Key_Value => True,
                    Key_Value    => (Key => Key_Ref, Value => Value)));
   end Enqueue;


   procedure Enqueue
     (Resource : in out Stream_Type;
      Marker   : in     Marker_Type) is
   begin
      Queues.Enqueue
        (Resource.Queue,
         Item_Type'(Is_Key_Value => False, Marker => Marker));
   end Enqueue;


   procedure Put_Float
     (Resource : in out Stream_Type;
      Key      : in     String;
      Float    : in     Float_Value_Type)
   is
      N : constant Number_Type := (Is_Real => True, Real => Float.Value);
   begin
      Enqueue (Resource, Key, Value_Type'(JSON_Number, N));
   end Put_Float;


   procedure Put_Integer
     (Resource : in out Stream_Type;
      Key      : in     String;
      Integer  : in     Integer_Value_Type)
   is
      N : constant Number_Type := (Is_Real => False, Int => Integer.Value);
   begin
      Enqueue (Resource, Key, Value_Type'(JSON_Number, N));
   end Put_Integer;


   procedure Put_String
     (Resource : in out Stream_Type;
      Key      : in     String;
      Str      : in     String_Value_Type)
   is
      Str_Ref : constant String_Ref_Type := new String'(Str.Image);
   begin
      Enqueue (Resource, Key, Value_Type'(JSON_String, Str_Ref));
   end Put_String;


   procedure Put_Null
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Enqueue (Resource, Key, Value_Type'(Kind => JSON_Null));
   end Put_Null;


   procedure Put_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Enqueue (Resource, Key, Value_Type'(Kind => JSON_Object));
   end Put_Object;


   procedure Put_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Enqueue (Resource, Key, Value_Type'(Kind => JSON_Array));
   end Put_Array;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      Enqueue (Resource, JSON_Object_End);
   end End_Object;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      Enqueue (Resource, JSON_Array_End);
   end End_Array;


   ----------
   -- Operations to read the stream.

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      Item    : Item_Type;
      Success : Boolean;

      procedure Emit (S : String) is
      begin
         null;
      end Emit;

   begin
      if not Resource.Init then
         Resource.Init := True;
         Emit ("{");
         return;
      end if;

      Last := Buffer'First;

      if Buffer'Length = 0 then
         return;
      end if;

      if Resource.Buffer /= null then
         declare
            Buffer : constant String := Resource.Buffer.all;
         begin
            Free (Resource.Buffer);
            Emit (Buffer);
            return;
         end;
      end if;

      -- At this point, the Buffer is guaranteed to be null, i.e. empty.

      Queues.Dequeue (Resource.Queue, Success, Item);

      if not Success then
         Resource.Final := True;
         Emit ("}");
         return;
      end if;

      case Item.Is_Key_Value is
         when True =>
            Emit (Item.Key_Value.Key.all);
            Free (Item.Key_Value.Key.all);
            case Item.Key_Value.Value.Kind is
               when 
            end case;
         when False =>
            case Item.Marker is
               when JSON_Array_End    => Emit ("}");
               when JSON_Object_End   => Emit ("]");
            end case;
      end case;
   end Read;


   procedure Close (Resource : in out Stream_Type) is
   begin
      if Resource.Buffer /= null then
         Free (Resource.Buffer);
      end if;
      loop
         declare
            Item    : Item_Type;
            Success : Boolean;
         begin
            Queues.Dequeue (Resource.Queue, Success, Item);
            exit when not Success;
            if Item.Is_Key_Value then
               Free (Item.Key_Value.Key);
               if Item.Key_Value.Value.Kind = JSON_String then
                  Free (Item.Key_Value.Value.S);
               end if;
            end if;
         end;
      end loop;
   end Close;


   function End_Of_File (Resource : Stream_Type) return Boolean is
   begin
      return Resource.Final;
   end End_Of_File;

end JSON;

