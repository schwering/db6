-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Prefix is

   function New_Read_Context return Read_Context_Type is
   begin
      return (null record);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return (Has_Pred => False, others => <>);
   end New_Write_Context;


   function Size_Bound (S : String_Type) return Blocks.Size_Type
   is
      function Size_Of_Boolean is new Blocks.Size_Of (Boolean);
      function Size_Of_String is
         new Blocks.Size_Of_Array (Index_Type, Item_Type, Buffer_Type);
      use type Blocks.Size_Type;
   begin
      return Size_Of_Boolean (True) + Size_Of_String (S.Buffer, 1, S.Length);
   end Size_Bound;


   type Info_Type is
      record
         Position : Blocks.Base_Position_Type;
         Length   : Length_Type;
      end record;
   pragma Pack (Info_Type);


   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      procedure Write_Boolean is new Blocks.Write (Boolean);
      procedure Write_String is
         new Blocks.Write_Array (Index_Type, Item_Type, Buffer_Type);
   begin
      if not Context.Has_Pred then
         Write_Boolean (Block, Cursor, False);
         Context := (Has_Pred => True,
                     Position => Blocks.Position (Cursor),
                     Pred     => S);
         Write_String (Block, Cursor, S.Buffer, 1, S.Length);
      else
         declare
            function Common_Length (S, T : String_Type) return Length_Type
            is
               Min : constant Length_Type :=
                  Length_Type'Min (S.Length, T.Length);
            begin
               for I in 1 .. Min loop
                  if S.Buffer (I) /= T.Buffer (I) then
                     return I - 1;
                  end if;
               end loop;
               return Min;
            end Common_Length;

            use type Blocks.Size_Type;
            function Size_Of is new Blocks.Size_Of (Info_Type);
            procedure Write_Info is new Blocks.Write (Info_Type);
            Length : constant Length_Type := Common_Length (Context.Pred, S);
            Info   : constant Info_Type := (Context.Position, Length);
            subtype Safed is Indefinite_Buffer_Type (1 .. Length);
            function Size_Of is new Blocks.Size_Of (Safed);
         begin
            if Size_Of (Safed' (others => <>)) <= Size_Of (Info) then
               Write_Boolean (Block, Cursor, False);
               Context := (Has_Pred => True,
                           Position => Blocks.Position (Cursor),
                           Pred     => S);
               Write_String (Block, Cursor, S.Buffer, 1, S.Length);
            else
               declare
                  Succ : constant String_Type := Substring (S, Length + 1);
               begin
                  Write_Boolean (Block, Cursor, True);
                  Write_Info (Block, Cursor, Info);
                  Write_String (Block, Cursor, Succ.Buffer, 1, Succ.Length);
                  pragma Assert (Context.Pred & Succ = S);
               end;
            end if;
         end;
      end if;
   end Write;


   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      procedure Read_Boolean is new Blocks.Read (Boolean);
      procedure Read_Info is new Blocks.Read (Info_Type);
      procedure Read_String is
         new Blocks.Read_Array (Index_Type, Item_Type, Buffer_Type);
      Has_Pred : Boolean;
   begin
      Read_Boolean (Block, Cursor, Has_Pred);
      if not Has_Pred then
         Read_String (Block, Cursor, S.Buffer, 1, S.Length);
      else
         declare
            Info : Info_Type;
         begin
            Read_Info (Block, Cursor, Info);
            declare
               Cursor2 : Blocks.Cursor_Type :=
                 Blocks.New_Cursor (Info.Position);
               Pred    : String_Type;
               Succ    : String_Type;
            begin
               Read_String (Block, Cursor2, Pred.Buffer, 1, Pred.Length);
               Read_String (Block, Cursor, Succ.Buffer, 1, Succ.Length);
               S := Substring (Pred, 1, Info.Length) & Succ;
            end;
         end;
      end if;
   end Read;


   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      S : String_Type;
   begin
      Read (Context, Block, Cursor, S);
   end Skip;

end Prefix;

