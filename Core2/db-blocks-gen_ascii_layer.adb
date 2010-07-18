-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Blocks.Gen_ASCII_Layer is

   function Size_Of_Count return Size_Type
   is
      function Size_Of is new Blocks.Size_Of (Count_Type);
   begin
      return Size_Of (Count_Type'First);
   end Size_Of_Count;


   function Offset_Of_Count
     (Block : Base_Block_Type)
      return Base_Position_Type is
   begin
      return Block'First + Base_Position_Type (Size_Of_Count) - 1;
   end Offset_Of_Count;


   function Count (Block : Base_Block_Type) return Count_Type
   is
      procedure Read_Count is new Blocks.Read_At (Count_Type);
      Count : Count_Type;
   begin
      Read_Count (Block      => Block,
                  From_Index => Block'First,
                  To_Index   => Offset_Of_Count (Block),
                  Item       => Count);
      return Count;
   end Count;


   procedure Set_Count (Block : in out Base_Block_Type; Count : in Count_Type)
   is
      procedure Write_Count is new Blocks.Write_At (Count_Type);
   begin
      Write_Count (Block      => Block,
                   From_Index => Block'First,
                   To_Index   => Offset_Of_Count (Block),
                   Item       => Count);
      pragma Assert (Gen_ASCII_Layer.Count (Block) = Count);
   end Set_Count;


   function Max_Count (Block : Base_Block_Type) return Count_Type is
   begin
      return Block'Length - Count_Type (Size_Of_Count);
   end Max_Count;


   function Char
     (Block : Base_Block_Type;
      Index : Count_Type)
      return Character
   is
      pragma Precondition (Index > 0);
      pragma Precondition (Index <= Max_Count (Block));
      procedure Read_Char is new Blocks.Read_At (Character);
      Char : Character;
   begin
      Read_Char (Block      => Block,
                 From_Index => Offset_Of_Count (Block) +
                               Base_Position_Type (Index),
                 To_Index   => Offset_Of_Count (Block) +
                               Base_Position_Type (Index),
                 Item       => Char);
      return Char;
   end Char;


   procedure Set_Char
     (Block : in out Base_Block_Type;
      Index : in     Count_Type;
      Char  : in     Character)
   is
      pragma Precondition (Index > 0);
      pragma Precondition (Index <= Max_Count (Block));
      procedure Write_Char is new Blocks.Write_At (Character);
   begin
      Write_Char (Block      => Block,
                  From_Index => Offset_Of_Count (Block) +
                                Base_Position_Type (Index),
                  To_Index   => Offset_Of_Count (Block) +
                                Base_Position_Type (Index),
                  Item       => Char);
      pragma Assert (Gen_ASCII_Layer.Char (Block, Index) = Char);
   end Set_Char;



   procedure Flush_Block (File : in out File_Type)
   is
      pragma Precondition (File.Mode = Write_Only);
   begin
      Set_Count (File.Block, File.Count);
      Block_IO.Write (File.Block_File, File.Address, File.Block);
      File.Address := Block_IO.Succ (File.Address);
      File.Count   := 0;
      Reset (File.Block);
   end Flush_Block;


   procedure Finish_Creation (File : in out File_Type) is
   begin
      File.Address := Block_IO.First_Address;
      File.Count   := 0;
      File.Mode    := Write_Only;
      Reset (File.Block);
      Flush_Block (File);
      File.Address := Block_IO.First_Address;
   end Finish_Creation;


   procedure Finish_Writing (File : in out File_Type) is
   begin
      Flush_Block (File);
      Flush_Block (File); -- to get a finalizing null block
   end Finish_Writing;


   procedure Create
     (ID    : in  String;
      File  : out File_Type) is
   begin
      Block_IO.Create (ID, File.Block_File);
      Finish_Creation (File);
   end Create;


   procedure Create_And_Open_Temporary
     (ID    : in  String;
      File  : out File_Type) is
   begin
      Block_IO.Create_And_Open_Temporary (ID, File.Block_File);
      Finish_Creation (File);
   end Create_And_Open_Temporary;


   procedure Open
     (ID    : in  String;
      File  : out File_Type) is
   begin
      Block_IO.Open (ID, File.Block_File);
      File.Address := Block_IO.First_Address;
      File.Count   := 0;
      File.Mode    := Read_Only;
      Block_IO.Read (File.Block_File, File.Address, File.Block);
   end;


   procedure Close
     (File : in out File_Type) is
   begin
      if File.Mode = Write_Only then
         Finish_Writing (File);
      end if;
      Block_IO.Close (File.Block_File);
   end Close;


   procedure Set_Mode
     (File : in out File_Type;
      Mode : in     Mode_Type) is
   begin
      if File.Mode = Write_Only and Mode /= Write_Only then
         Finish_Writing (File);
      end if;
      File.Address := Block_IO.First_Address;
      File.Count   := 0;
      File.Mode    := Mode;
      if File.Mode = Read_Only then
         Block_IO.Read (File.Block_File, File.Address, File.Block);
      end if;
   end Set_Mode;


   procedure Read
     (File : in out File_Type;
      Char :    out Character;
      EOF  :    out Boolean)
   is
      pragma Precondition (File.Mode = Read_Only);
   begin
      if Count (File.Block) = 0 then -- End of file
         EOF := True;
         return;
      end if;

      if File.Count = Count (File.Block) then
         File.Address := Block_IO.Succ (File.Address);
         File.Count   := 0;
         Block_IO.Read (File.Block_File, File.Address, File.Block);
      end if;

      if Count (File.Block) = 0 then -- End of file
         EOF := True;
         return;
      end if;

      pragma Assert (File.Count < Count (File.Block));
      File.Count := File.Count + 1;
      Char := Gen_ASCII_Layer.Char (File.Block, File.Count);
      EOF  := False;
   end Read;


   procedure Write
     (File : in out File_Type;
      Char : in     Character) is
   begin
      if File.Count = Max_Count (File.Block) then
         Flush_Block (File);
      end if;
      File.Count := File.Count + 1;
      Set_Char (File.Block, File.Count, Char);
      pragma Assert (Gen_ASCII_Layer.Char (File.Block, File.Count) = Char);
   end Write;

end DB.Blocks.Gen_ASCII_Layer;

