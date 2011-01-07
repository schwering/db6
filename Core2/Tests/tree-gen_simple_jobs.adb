with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Tags;

with DB.Blocks;

package body Tree.Gen_Simple_Jobs is

   use type DB.Maps.State_Type;

   procedure Insert
   is
      KV    : constant Key_Value_Type := Next_Entry;
      State : DB.Maps.State_Type := DB.Maps.Success;
   begin
      Map.Insert(Types.Key(KV), Types.Value(KV), State);
      if State /= DB.Maps.Success then
         Put_Line("Insertion failed "& State'Img);
         raise Stop_Now;
      end if;
      declare
         Val   : DB.Maps.Value_Type'Class := Null_Value;
         State : DB.Maps.State_Type := DB.Maps.Failure;
      begin
         Map.Search(Types.Key(KV), Val, State);
         if State /= DB.Maps.Success or else
            not Types.Value(KV).Equals(Val) then
            Put_Line("Look up failed "& State'Img);
            Put_Line("Key   = """& To_String(Types.Key(KV)) &"""");
            Put_Line("Value = """& To_String(Types.Value(KV)) &"""");
            Put_Line("Value = """& To_String(Val) &"""");
            raise Stop_Now;
         end if;
      end;
   end Insert;


   procedure Replace
   is
      KV    : constant Key_Value_Type := Next_Entry;
      State : DB.Maps.State_Type := DB.Maps.Success;
   begin
      Map.Replace(Types.Key(KV), Types.Value(KV), State);
      if State /= DB.Maps.Success then
         Put_Line("Replacement failed "& State'Img);
         raise Stop_Now;
      end if;
      declare
         Val   : DB.Maps.Value_Type'Class := Null_Value;
         State : DB.Maps.State_Type := DB.Maps.Failure;
      begin
         Map.Search(Types.Key(KV), Val, State);
         if State /= DB.Maps.Success or else
            not Types.Value(KV).Equals(Val) then
            Put_Line("Look up failed "& State'Img);
            Put_Line("Key   = """& To_String(Types.Key(KV)) &"""");
            Put_Line("Value = """& To_String(Types.Value(KV)) &"""");
            Put_Line("Value = """& To_String(Val) &"""");
            raise Stop_Now;
         end if;
      end;
   end Replace;


   procedure Append
   is
      KV    : constant Key_Value_Type := Next_Entry;
      State : DB.Maps.State_Type := DB.Maps.Success;
   begin
      Map.Append(Types.Key(KV), Types.Value(KV), State);
      if State /= DB.Maps.Success then
         Put_Line("Append failed "& State'Img);
         raise Stop_Now;
      end if;
      declare
         Val   : DB.Maps.Value_Type'Class := Null_Value;
         State : DB.Maps.State_Type := DB.Maps.Failure;
      begin
         Map.Search(Types.Key(KV), Val, State);
         if State /= DB.Maps.Success or else
            not Types.Value(KV).Equals(Val) then
            Put_Line("Look up failed "& State'Img);
            Put_Line("Key   = """& To_String(Types.Key(KV)) &"""");
            Put_Line("Value = """& To_String(Types.Value(KV)) &"""");
            Put_Line("Value = """& To_String(Val) &"""");
            raise Stop_Now;
         end if;
      end;
   end Append;


   procedure Delete
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : DB.Maps.Value_Type'Class := Null_Value;
      State : DB.Maps.State_Type := DB.Maps.Success;
   begin
      Map.Delete(Types.Key(KV), Val, State);
      if State /= DB.Maps.Success or else
         not Types.Value(KV).Equals(Val) then
         Put_Line("Deletion failed "& State'Img);
         Put_Line("Key   = """& To_String(Types.Key(KV)) &"""");
         Put_Line("Value = """& To_String(Types.Value(KV)) &"""");
         Put_Line("Value = """& To_String(Val) &"""");
         raise Stop_Now;
      end if;
   end Delete;


   procedure Search
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : DB.Maps.Value_Type'Class := Null_Value;
      State : DB.Maps.State_Type := DB.Maps.Success;
   begin
      Map.Search(Types.Key(KV), Val, State);
      if State /= DB.Maps.Success or else
         not Types.Value(KV).Equals(Val) then
         Put_Line("Look up failed "& State'Img);
         Put_Line("Key   = """& To_String(Types.Key(KV)) &"""");
         Put_Line("Value = """& To_String(Types.Value(KV)) &"""");
         Put_Line("Value = """& To_String(Val) &"""");
         Put_Line("Equal = "& Boolean'Image(Types.Value(KV).Equals(Val)));
         raise Stop_Now;
      end if;
   end Search;


   procedure Antisearch
   is
      use type DB.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Next_Entry;
      Val   : DB.Maps.Value_Type'Class := Null_Value;
      State : DB.Maps.State_Type := DB.Maps.Failure;
   begin
      Map.Search(Types.Key(KV), Val, State);
      if State /= DB.Maps.Failure then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Antisearch;


   procedure Count
   is
      Cnt : DB.Maps.Count_Type;
   begin
      Map.Count(Cnt);
      Put_Line("Count:"& Cnt'Img);
   end Count;


   procedure Stats
   is
      use type DB.Maps.Level_Type;
      Last_Level : DB.Maps.Level_Type := DB.Maps.Level_Type'Last;

      function Sqrt (A : DB.Maps.Average_Type) return DB.Maps.Average_Type
      is
         package Elementary_Functions is new
         Ada.Numerics.Generic_Elementary_Functions(Float);
      begin
         return DB.Maps.Average_Type(Elementary_Functions.Sqrt(Float(A)));
      end Sqrt;

      procedure Emit (Level : in DB.Maps.Level_Type;
                      Key   : in String;
                      Value : in DB.Maps.Data_Type)
      is
         function Trim (S : String) return String is
         begin return Ada.Strings.Fixed.Trim(S, Ada.Strings.Both); end;

         function Img (L : DB.Maps.Level_Type) return String is
         begin return Trim(L'Img); end;

         function Img (A : DB.Maps.Absolute_Type) return String is
         begin return Trim(A'Img); end;

         function Img (A : DB.Maps.Average_Type) return String is
         begin return Trim(A'Img); end;

         type Percent_Type is delta 0.1 digits 5;
      begin
         if Level /= Last_Level then
            if Last_Level /= DB.Maps.Level_Type'Last then
               Put("    ],");
               New_Line;
               Put("  },");
            end if;
            New_Line;
            Put("  {");
            New_Line;
            Put("    ""level"": "& Img(Level) &", ");
            New_Line;
            Put("    ""values"": [");
            New_Line;
         end if;
         Put("      { ""key"": """& Key &""", ");
         case Value.Compound is
            when True =>
               Put("""avg"": "& Img(Value.Avg) &", "&
                   """dev"": "& Img(Sqrt(Value.Var)) &", "&
                   """max"": "& Img(Value.Max) &", "&
                   """min"": "& Img(Value.Min));
            when False =>
               Put("""val"": "& Img(Value.Val));
         end case;
         if Key = "Count" then
            Put(", ""mb"":"&
                DB.Maps.Absolute_Type'Image
                   (Value.Val * DB.Maps.Absolute_Type(DB.Blocks.Block_Size) /
                    1024**2));
         elsif Key ="Size" or Key = "Waste" then
            Put(", ""pct"":"&
                Percent_Type'Image(Percent_Type(
                  Float(Value.Avg) / Float(DB.Blocks.Block_Size) * 100.0)));
         end if;
         Put("},");
         New_Line;
         Last_Level := Level;
      end Emit;

   begin
      Put("[");
      Map.Stats(Emit'Access);
      Put_Line("    ]");
      Put_Line("  }");
      Put_Line("]");
   end Stats;


   procedure Check is
   begin
      Map.Check;
   end Check;

end Tree.Gen_Simple_Jobs;

