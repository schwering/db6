package body DB.Compression.Levenshtein is

   function Min
     (X, Y, Z : Integer)
      return Integer
   is begin
      if X <= Y and X <= Z then
         return X;
      elsif Y <= Z then
         return Y;
      else
         return Z;
      end if;
   end Min;


   function Distance_Matrix
     (S, T : Item_Array_Type)
      return Distance_Matrix_Type
   is
      D : Distance_Matrix_Type(0 .. S'Length, 0 .. T'Length)
        := (others => (others => 0));
   begin
      for I in D'Range(1) loop
         D(I, 0) := I;
      end loop;
      for J in D'Range(2) loop
         D(0, J) := J;
      end loop;

      for I in S'Range loop
         for J in T'Range loop
            if not (S(I) = T(J)) then
               D(I, J) := Min(D(I-1, J)   + 1,  -- deletion
                              D(I,   J-1) + 1,  -- insertion
                              D(I-1, J-1) + 1); -- substitution
            else
               D(I, J) := Min(D(I-1, J)   + 1,  -- deletion
                              D(I,   J-1) + 1,  -- insertion
                              D(I-1, J-1));     -- no substitution
            end if;
         end loop;
      end loop;
      return D;
   end Distance_Matrix;


   --procedure Print
     --(D : Distance_Matrix_Type)
   --is begin
      --for X in D'Range(1) loop
         --for Y in D'Range(2) loop
            --Put(Distance_Type'Image(D(X, Y)));
         --end loop;
         --New_Line;
      --end loop;
   --end Print;


   function Distance
     (S, T : Item_Array_Type)
      return Distance_Type
   is
      D : constant Distance_Matrix_Type := Distance_Matrix(S, T);
   begin
      return D(D'Last(1), D'Last(2));
   end Distance;


   function Encode
     (S, T : Item_Array_Type)
      return Delta_Type
   is

      function Action_Sequence
        (Old_Str, New_Str : Item_Array_Type)
         return Action_Array_Type
      is
         function DD
           (I, J : Invalid_Matrix_Index_Type;
            D    : Distance_Matrix_Type)
            return Distance_Type
         is begin
            if I in D'Range(1) and J in D'Range(2) then
               return D(I, J);
            else
               return Distance_Type'Last;
            end if;
         end DD;

         function DD_Diff
           (I, J : Invalid_Matrix_Index_Type;
            D    : Distance_Matrix_Type;
            S, T : Item_Array_Type)
            return Distance_Type
         is begin
            if I in D'Range(1) and J in D'Range(2) then
               if (S'First + I in S'Range and T'First + J in T'Range)
               and then not (S(S'First + I) = T(T'First + J)) then
                  return D(I, J) + 1;
               else
                  return D(I, J);
               end if;
            else
               return Distance_Type'Last;
            end if;
         end DD_Diff;

         D    : constant Distance_Matrix_Type
              := Distance_Matrix(Old_Str, New_Str);
         S    : Action_Array_Type(1 .. D'Length(1) + D'Length(2))
              := (others => Keep);
         Prev : Action_Type := Keep;
         K : Length_Type    := S'Last;
         I : Invalid_Matrix_Index_Type := D'Last(1);
         J : Invalid_Matrix_Index_Type := D'Last(2);
      begin
         loop
            if I = 0 and J = 0 then
               return S(K + 1 .. S'Last);
            end if;
            declare
               S_Repl : constant Distance_Type
                      := DD_Diff(I-1, J-1, D, Old_Str, New_Str);
               S_Ins  : constant Distance_Type := DD(I, J-1, D);
               S_Del  : constant Distance_Type := DD(I-1, J, D);
               M      : constant Distance_Type := Min(S_Repl, S_Ins, S_Del);
            begin
               if M = S_Repl then
                  if not (Old_Str(Old_Str'First + I - 1)
                        = New_Str(New_Str'First + J - 1)) then
                     S(K) := Replace;
                  else
                     S(K) := Keep;
                  end if;
                  I := I - 1;
                  J := J - 1;
               elsif M = S_Ins and Prev = Insert then
                  S(K) := Insert;
                  J := J - 1;
               elsif M = S_Del then
                  S(K) := Delete;
                  I := I - 1;
               else
                  S(K) := Insert;
                  J := J - 1;
               end if;
               Prev := S(K);
               K := K - 1;
            end;
         end loop;
      end Action_Sequence;


      function Needed_Action_Count
        (S : Action_Array_Type)
         return Length_Type
      is
         Count : Length_Type := 0;
         Prev  : Action_Type := Keep;
      begin
         for I in S'Range loop
            if Prev /= S(I) and S(I) /= Keep then
               Count := Count + 1;
            end if;
            Prev := S(I);
         end loop;
         return Count;
      end Needed_Action_Count;


      function Needed_Char_Count
        (S : Action_Array_Type)
         return Length_Type
      is
         Count : Length_Type := 0;
      begin
         for I in S'Range loop
            if S(I) = Replace or S(I) = Insert then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Needed_Char_Count;

      Seq          : constant Action_Array_Type := Action_Sequence(S, T);
      Action_Count : constant Length_Type := Needed_Action_Count(Seq);
      Char_Count   : constant Length_Type := Needed_Char_Count(Seq);
      Del          : Delta_Type(Action_Count => Action_Count,
                                Char_Count   => Char_Count);
      Action_Index : Invalid_Index_Type := Del.Actions'First;
      Char_Index   : Invalid_Index_Type := Del.Chars'First;
      S_Index      : Invalid_Index_Type := S'First;
      T_Index      : Invalid_Index_Type := T'First;
      A            : Action_Type;
      Prev         : Action_Type := Keep;
   begin
      for K in Seq'Range loop
         A := Seq(K);
         case A is
            when Keep =>
               S_Index := S_Index      + 1;
               T_Index := T_Index      + 1;
            when Replace =>
               if Prev /= A then
                  Del.Actions(Action_Index).Action := Replace;
                  Del.Actions(Action_Index).Index  := S_Index;
                  Del.Actions(Action_Index).Length := 1;
               else
                  Action_Index          := Action_Index - 1;
                  Del.Actions(Action_Index).Length
                                        := Del.Actions(Action_Index).Length + 1;
               end if;
               Del.Chars(Char_Index) := T(T_Index);
               Action_Index          := Action_Index + 1;
               Char_Index            := Char_Index   + 1;
               S_Index               := S_Index      + 1;
               T_Index               := T_Index      + 1;
            when Insert =>
               if Prev /= A then
                  Del.Actions(Action_Index).Action := Insert;
                  Del.Actions(Action_Index).Index  := S_Index;
                  Del.Actions(Action_Index).Length := 1;
               else
                  Action_Index          := Action_Index - 1;
                  Del.Actions(Action_Index).Length
                                        := Del.Actions(Action_Index).Length + 1;
               end if;
               Del.Chars(Char_Index)        := T(T_Index);
               Char_Index                   := Char_Index   + 1;
               Action_Index                 := Action_Index + 1;
               T_Index                      := T_Index      + 1;
            when Delete =>
               if Prev /= A then
                  Del.Actions(Action_Index).Action := Delete;
                  Del.Actions(Action_Index).Index  := S_Index;
                  Del.Actions(Action_Index).Length := 1;
               else
                  Action_Index          := Action_Index - 1;
                  Del.Actions(Action_Index).Length
                                        := Del.Actions(Action_Index).Length + 1;
               end if;
               Action_Index := Action_Index + 1;
               S_Index      := S_Index      + 1;
         end case;
         Prev := A;
      end loop;
      return Del;
   end Encode;


   function Decode
     (S : Item_Array_Type;
      D : Delta_Type)
      return Item_Array_Type
   is
      function Offset
        (D : Delta_Type)
         return Integer
      is
         L : Integer := 0;
      begin
         for I in D.Actions'Range loop
            case D.Actions(I).Action is
               when Keep    => null;
               when Replace => null;
               when Insert  => L := L + D.Actions(I).Length;
               when Delete  => L := L - D.Actions(I).Length;
            end case;
         end loop;
         return L;
      end Offset;

      I           : Invalid_Index_Type := S'Last;
      T           : Item_Array_Type(1 .. S'Length + Offset(D));
      J           : Invalid_Index_Type := T'Last;
      Chars_Index : Invalid_Index_Type := D.Chars'Last;

      procedure Add_From_S (From, To : Invalid_Index_Type)
      is begin
         T(J - (To - From) .. J) := S(From .. To);
         J := J - (To - From) - 1;
      end Add_From_S;

      procedure Add_From_Chars (From, To : Invalid_Index_Type)
      is begin
         T(J - (To - From) .. J) := D.Chars(From .. To);
         J := J - (To - From) - 1;
      end Add_From_Chars;

   begin
      for K in reverse D.Actions'Range loop
         case D.Actions(K).Action is
            when Replace =>
               Add_From_S(From => D.Actions(K).Index + D.Actions(K).Length,
                          To   => I);
               Add_From_Chars(From => Chars_Index - D.Actions(K).Length + 1,
                              To   => Chars_Index);
               Chars_Index := Chars_Index - D.Actions(K).Length;
               I           := D.Actions(K).Index - 1;
            when Insert =>
               Add_From_S(From => D.Actions(K).Index,
                          To   => I);
               Add_From_Chars(From => Chars_Index - D.Actions(K).Length + 1,
                              To   => Chars_Index);
               Chars_Index := Chars_Index - D.Actions(K).Length;
               I           := D.Actions(K).Index - 1;
            when Delete =>
               Add_From_S(From => D.Actions(K).Index + D.Actions(K).Length,
                          To   => I);
               I           := D.Actions(K).Index - 1;
            when Keep => -- never occurs
               null;
         end case;
      end loop;
      Add_From_S(From => 1,
                 To   => I);
      return T;
   end Decode;

end DB.Compression.Levenshtein;

