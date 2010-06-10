-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers.Ordered_Maps;

with DB.Blocks;

package body DB.DSA.Gen_BTrees.Gen_Stats is

   procedure Make_Stats
     (Tree : in out Tree_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type))
   is
      use Nodes;

      type Info_Type is
         record
            Level       : Level_Type;
            Count       : Absolute_Type := 0;
            Degree_Sum  : Absolute_Type := 0;
            Degree_Sqs  : Absolute_Type := 0;
            Degree_Min  : Absolute_Type := Absolute_Type'Last;
            Degree_Max  : Absolute_Type := Absolute_Type'First;
            Size_Sum    : Absolute_Type := 0;
            Size_Sqs    : Absolute_Type := 0;
            Size_Min    : Absolute_Type := Absolute_Type'Last;
            Size_Max    : Absolute_Type := Absolute_Type'First;
            Waste_Sum   : Absolute_Type := 0;
            Waste_Sqs   : Absolute_Type := 0;
            Waste_Min   : Absolute_Type := Absolute_Type'Last;
            Waste_Max   : Absolute_Type := Absolute_Type'First;
         end record;

      package Maps is new Ada.Containers.Ordered_Maps(Level_Type, Info_Type);

      procedure Handle (N : in Node_Type; I : in out Info_Type) is
         procedure Set
           (Sum : in out Absolute_Type;
            Sqs : in out Absolute_Type;
            Min : in out Absolute_Type;
            Max : in out Absolute_Type;
            Val : in     Absolute_Type) is
         begin
            Sum := Sum + Val;
            Sqs := Sqs + Val * Val;
            Min := Absolute_Type'Min(Min, Val);
            Max := Absolute_Type'Max(Max, Val);
         end;

         Degree : constant Absolute_Type := Absolute_Type(Nodes.Degree(N));
         Size   : constant Absolute_Type := Absolute_Type(Nodes.Size_Of(N));
         Waste  : constant Absolute_Type :=
            Absolute_Type(Blocks.Block_Size) - Size;
      begin
         I.Count := I.Count + 1;
         Set(I.Degree_Sum, I.Degree_Sqs, I.Degree_Min, I.Degree_Max, Degree);
         Set(I.Size_Sum,   I.Size_Sqs,   I.Size_Min,   I.Size_Max,   Size);
         Set(I.Waste_Sum,  I.Waste_Sqs,  I.Waste_Min,  I.Waste_Max,  Waste);
      end Handle;

      Map : Maps.Map := Maps.Empty_Map;

      procedure Handle (N : in Node_Type)
      is
         L : constant Level_Type := Level_Type(Level(N));
         C : constant Maps.Cursor := Maps.Find(Map, L);
         I : Info_Type;
      begin
         if Maps.Has_Element(C) then
            I := Maps.Element(C);
         else
            I := (Level => L, others => <>);
            Maps.Insert(Map, L, I);
         end if;
         Handle(N, I);
         Maps.Replace(Map, L, I);
      end Handle;

      N_A : Nodes.Valid_Address_Type :=
         Nodes.Valid_Address_Type(Block_IO.First_Address);
   begin
      loop
         declare
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, N_A, N);
            Handle(N);
            N_A := Nodes.Valid_Address_Type(Block_IO.Succ
                        (Block_IO.Valid_Address_Type(N_A)));
         exception
            when IO_Error =>
               exit;
         end;
      end loop;

      declare
         procedure Call_Emit (Cursor : Maps.Cursor)
         is
            L : constant Natural := Natural(Maps.Key(Cursor));
            I : constant Info_Type := Maps.Element(Cursor);

            function Avg (Sum : Absolute_Type)
               return Average_Type is
            begin
               return Average_Type(Sum) / Average_Type(I.Count);
            end Avg;

            function Var (Sqs : Absolute_Type; Sum : Absolute_Type)
               return Average_Type is
            begin
               return Average_Type(Sqs) / Average_Type(I.Count) -
                      Avg(Sum) * Avg(Sum);
            end Var;
         begin
            Emit(L, "Count", Data_Type'(Compound => False, Val => I.Count));
            Emit(L, "Degree", Data_Type'(Compound => True,
                                         Avg => Avg(I.Degree_Sum),
                                         Var => Var(I.Degree_Sqs, I.Degree_Sum),
                                         Min => I.Degree_Min,
                                         Max => I.Degree_Max));
            Emit(L, "Size", Data_Type'(Compound => True,
                                       Avg => Avg(I.Size_Sum),
                                       Var => Var(I.Size_Sqs, I.Size_Sum),
                                       Min => I.Size_Min,
                                       Max => I.Size_Max));
            Emit(L, "Waste", Data_Type'(Compound => True,
                                        Avg => Avg(I.Waste_Sum),
                                        Var => Var(I.Waste_Sqs, I.Waste_Sum),
                                        Min => I.Waste_Min,
                                        Max => I.Waste_Max));
         end;
      begin
         Maps.Iterate(Map, Call_Emit'Access);
      end;
   end Make_Stats;

end DB.DSA.Gen_BTrees.Gen_Stats;

