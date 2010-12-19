-- Abstract:
--
-- Serialization of database entries to JSON format.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

with AWS.Resources.Streams;

private with DB.DSA.Utils.Gen_Queues;

package JSON is
   pragma Elaborate_Body;

   package AS renames Ada.Streams;
   package ARS renames AWS.Resources.Streams;

   type Stream_Type is new ARS.Stream_Type with private;

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset);

   procedure Close (Resource : in out Stream_Type);

   function End_Of_File (Resource : Stream_Type) return Boolean;

   procedure Reset (Resource : in out Stream_Type) is null;

   procedure Set_Index
     (Resource : in out Stream_Type;
      To       : in     AS.Stream_Element_Offset) is null;


private
   type Marker_Type is (JSON_Array_Begin,  JSON_Array_End,
                        JSON_Object_Begin, JSON_Object_End);

   type JSON_Type is (JSON_Number, JSON_String, JSON_Boolean, JSON_Null);

   type String_Ref_Type is access String;

   type Number_Type (Real : Boolean := False) is
      record
         case Real is
            when False => I : Long_Integer;
            when True  => R : Long_Float;
         end case;
      end record;

   type Value_Type (Kind : JSON_Type := JSON_Null) is
      record
         case Kind is
            when JSON_Boolean =>
               B : Boolean;
            when JSON_Number =>
               N : Number_Type;
            when JSON_String =>
               S : String_Ref_Type;
            when JSON_Null =>
               null;
         end case;
      end record;

   type Key_Value_Type is
      record
         Key   : String_Ref_Type;
         Value : Value_Type;
      end record;

   type Item_Kind_Type is (Key_Value, Marker);

   type Item_Type (Is_Key_Value : Boolean := True) is
      record
         case Is_Key_Value is
            when True  => Key_Value : Key_Value_Type;
            when False => Marker    : Marker_Type;
         end case;
      end record;

   package Queues is new DB.DSA.Utils.Gen_Queues
     (Queue_Size => 100,
      Item_Type  => Item_Type);

   type Stream_Type is new ARS.Stream_Type with
      record
         Queue : Queues.Queue_Type;
         Final : Boolean := False;
      end record;

end JSON;

