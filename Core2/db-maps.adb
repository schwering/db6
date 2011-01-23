-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Interfaces.C;

with DB.Maps.Bounded;
with DB.Maps.Covering;

package body DB.Maps is

   function New_Map
     (Implementation   : in Implementation_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class is
   begin
      case Implementation is
         when BTree =>
            return Bounded.New_Map (Allow_Duplicates);
         when Multi =>
            return Covering.New_Map (Allow_Duplicates);
      end case;
   end New_Map;


   function New_Map_Ref
     (Implementation   : in Implementation_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Ref_Type is
   begin
      case Implementation is
         when BTree =>
            return new Bounded.Map_Type'(Bounded.New_Map (Allow_Duplicates));
         when Multi =>
            return new Covering.Map_Type'
              (Covering.New_Map (Allow_Duplicates));
      end case;
   end New_Map_Ref;


   function New_Map
     (Max_Key_Size     : in Blocks.Size_Type;
      Max_Value_Size   : in Blocks.Size_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class
   is
      use type Blocks.Size_Type;
   begin
      if Max_Key_Size <= Bounded.Max_Key_Size (Max_Value_Size) then
         return Bounded.New_Map (Allow_Duplicates);
      else
         raise Program_Error;
      end if;
   end New_Map;


   function New_Cursor_Ref
     (Map           : Map_Type;
      Thread_Safe   : Boolean;
      Lower_Bound   : Bound_Type;
      Upper_Bound   : Bound_Type;
      Column_Regexp : String := "")
      return Cursor_Ref_Type
   is
      function New_Cursor_Ref_Fix
        (Map           : Map_Type'Class;
         Thread_Safe   : Boolean;
         Lower_Bound   : Bound_Type;
         Upper_Bound   : Bound_Type;
         Column_Regexp : String)
         return Cursor_Ref_Type is
      begin
         return new Cursor_Type'Class'(New_Cursor
           (Map, Thread_Safe, Lower_Bound, Upper_Bound, Column_Regexp));
      end New_Cursor_Ref_Fix;
   begin
      return New_Cursor_Ref_Fix
        (Map, Thread_Safe, Lower_Bound, Upper_Bound, Column_Regexp);
   end New_Cursor_Ref;


   function Positive_Infinity_Bound return Bound_Type is
   begin
      return Bound_Type'(Concrete => False, Infinity => Positive_Infinity);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound return Bound_Type is
   begin
      return Bound_Type'(Concrete => False, Infinity => Negative_Infinity);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Keys.Key_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Concrete   => True,
                         Comparison => Comparison,
                         Key        => Key);
   end New_Bound;


   function Row_To_String (Row : Keys.Rows.String_Type) return String is
   begin
      return String (DB.Types.Keys.Rows.To_Buffer (Row));
   end Row_To_String;


   function Column_To_String
     (Column : Keys.Columns.String_Type)
      return String is
   begin
      return String (DB.Types.Keys.Columns.To_Buffer (Column));
   end Column_To_String;


   function String_To_Row (S : String) return Keys.Rows.String_Type is
   begin
      return DB.Types.Keys.Rows.New_String
        (Keys.Rows.Indefinite_Buffer_Type (S));
   end String_To_Row;


   function String_To_Column (S : String) return Keys.Columns.String_Type is
   begin
      return DB.Types.Keys.Columns.New_String
         (Keys.Columns.Indefinite_Buffer_Type (S));
   end String_To_Column;


   function Strings_To_Key (Row : String; Column : String) return Keys.Key_Type
   is
      function now return Interfaces.C.int;
      pragma Import (C, now, "db_maps_now");

      Seconds : constant Interfaces.C.int := now;
   begin
      return (String_To_Row (Row),
              String_To_Column (Column),
              Keys.Times.Number_Type (Seconds));
   end Strings_To_Key;

end DB.Maps;

