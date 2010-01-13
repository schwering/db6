with Ada.Text_IO; use Ada.Text_IO;

with DB.IO.Blocks;

package body IO_Dispatcher.Gen_Simple_Jobs is

   procedure Insert
   is
      KV    : constant Key_Value_Type := Random_Entry;
      State : State_Type := Success;
   begin
      Check_Key_Value(KV);
      P_Insert(Object, Get_Key(KV), Get_Value(KV), State);
      if State /= Success then
         Put_Line("Insertion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Insert;


   procedure Delete
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Success;
   begin
      P_Delete(Object, Get_Key(KV), Val, State);
      if State /= Success or else Get_Value(KV) /= Val then
         Put_Line("Deletion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Delete;


   procedure Search
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Success;
   begin
      P_Look_Up(Object, Get_Key(KV), Val, State);
      if State /= Success or else Get_Value(KV) /= Val then
         Put_Line("Look up failed "& State'Img);
         Put_Line("Key   = """& Key_To_String(Get_Key(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Get_Value(KV)) &"""");
         Put_Line("Value = """& Value_To_String(Val) &"""");
         Put_Line("Equal = "& Boolean'Image(Get_Value(KV) = Val));
         raise Stop_Now;
      end if;
   end Search;


   procedure Antisearch
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      State : State_Type := Failure;
   begin
      P_Look_Up(Object, Get_Key(KV), Val, State);
      if State /= Failure then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Antisearch;


   procedure Count
   is
      Count : Count_Type;
   begin
      P_Count(Object, Count);
      Put_Line("Count:"& Count'Img);
   end Count;


   procedure Stats
   is
      generic
         type Number_Type is (<>);
         with function "/" (L, R : Number_Type) return Number_Type is <>;
         with function "<=" (L, R : Number_Type) return Boolean is <>;
         Step    : in Number_Type;
         Minimum : in Number_Type;
         Units   : in String := "kmgt";
      function Gen_Format (I : Number_Type) return String;
      function Gen_Format (I : Number_Type) return String
      is
         Unit : Character := ' ';
         J    : Number_Type := I;
      begin
         for I in Units'Range loop
            exit when J / Step <= Minimum;
            J    := J / Step;
            Unit := Units(I);
         end loop;
         if Unit /= ' ' then
            return Number_Type'Image(J) & Unit;
         else
            return Number_Type'Image(J);
         end if;
      end Gen_Format;

      use type Count_Type;
      function Format_Number is new Gen_Format(Count_Type, "/", "<=", 1000, 1);
      function Format_Number is new Gen_Format(Natural, "/", "<=", 1000, 1);
      function Format_Bytes is new Gen_Format(Long_Integer, "/", "<=", 1024, 1);

      Count                                    : Count_Type;
      Height, Blocks, Free_Blocks, Used_Blocks : Natural;
      Max_Degree, Min_Degree, Avg_Degree       : Natural;
      Waste, Waste_Per_Block, Bytes            : Long_Integer;
      Relative_Waste_Per_Block                 : Float;
   begin
      P_Stats(Object                 => Object,
              Height                 => Height,
              Blocks                 => Blocks,
              Free_Blocks            => Free_Blocks,
              Max_Degree             => Max_Degree,
              Min_Degree             => Min_Degree,
              Avg_Degree             => Avg_Degree,
              Bytes_Wasted_In_Blocks => Waste,
              Bytes_In_Blocks        => Bytes);
      P_Count(Object, Count);
      Used_Blocks              := Blocks - Free_Blocks;
      if Used_Blocks > 0 then
         Waste_Per_Block       := Waste / Long_Integer(Used_Blocks);
      else
         Waste_Per_Block       := 0;
      end if;
      Relative_Waste_Per_Block := Float(Waste_Per_Block)
                                / Float(DB.IO.Blocks.Block_Size);
      -- The lines contain the following
      -- "OK" Count Height Blocks Used_Blocks Free_Blocks Max_Deg Avg_Dev\
      -- Min_Deg Total_Waste Total_Sizes Waste_per_Block Rel_Waste_per_Block
      Put("OK");
      Put(" Count ="& Format_Number(Count));
      Put(" Height ="& Format_Number(Height));
      Put(" Blks ="& Format_Number(Blocks));
      Put(" UsedBlks ="& Format_Number(Used_Blocks));
      Put(" FreeBlks ="& Format_Number(Free_Blocks));
      Put(" MaxDegree ="& Format_Number(Max_Degree));
      Put(" AvgDegree ="& Format_Number(Avg_Degree));
      Put(" MinDegree ="& Format_Number(Min_Degree));
      Put(" Waste(Bytes) ="& Format_Bytes(Waste));
      Put(" Size(Bytes) ="& Format_Bytes(Bytes));
      Put(" Waste/Blk ="& Format_Bytes(Waste_Per_Block));
      Put(" RelWaste/Blk ="& Float'Image(Relative_Waste_Per_Block));
      New_Line;
   end Stats;


   procedure Check is
   begin
      P_Check(Object);
   end Check;

end IO_Dispatcher.Gen_Simple_Jobs;

