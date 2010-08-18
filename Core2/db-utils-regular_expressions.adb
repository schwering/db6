-- Abstract:
--
-- see spec
--
-- Copyright 1998-2008, AdaCore
-- Copyright 2010 Christoph Schwering

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . R E G E X P                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2008, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with DB.Utils.Growing_Arrays;

package body DB.Utils.Regular_Expressions is

   Open_Paren    : constant Character := '(';
   Close_Paren   : constant Character := ')';
   Open_Bracket  : constant Character := '[';
   Close_Bracket : constant Character := ']';

   type State_Index is new Natural;
   --  Convention: 0 is kind of a default sink (just initialize the table with
   --  0 and then add the transitions).

   type Column_Index is new Natural;
   --  Convention: 0 represents `all the other characters' (in the mapping, for
   --  example).

   type Regexp_Array is array
     (State_Index range <>, Column_Index range <>) of State_Index;
   --  First index is for the state number
   --  Second index is for the character type
   --  Contents is the new State

   type Regexp_Array_Access is access Regexp_Array;
   --  Use this type through the functions Set below, so that it
   --  can grow dynamically depending on the needs.

   type Mapping is array (Character'Range) of Column_Index;
   --  Mapping between characters and column in the Regexp_Array. Those
   --  characters that are not explicitly mapped are assigned to column 0, i.e.
   --  the `all the other characters' transition.

   type Reverse_Mapping is array (Column_Index range <>) of Character;
   --  Reverse mapping between columns and characters. Those characters that are
   --  mapped to the `all the other characters' column in the original mapping
   --  are not contained in the reverse mapping, of course.

   type Boolean_Array is array (State_Index range <>) of Boolean;

   type Regexp_Value
     (Alphabet_Size : Column_Index;
      Num_States    : State_Index) is
   record
      Map         : Mapping;
      States      : Regexp_Array (1 .. Num_States, 0 .. Alphabet_Size);
      Is_Final    : Boolean_Array (0 .. Num_States);
      Start_State : State_Index;
   end record;
   --  Deterministic finite-state machine

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set
     (Table  : in out Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index;
      Value  : State_Index);
   --  Sets a value in the table. If the table is too small, reallocate it
   --  dynamically so that (State, Column) is a valid index in it.

   function Get
     (Table  : Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index)
      return   State_Index;
   --  Returns the value in the table at (State, Column).
   --  If this index does not exist in the table, returns 0

   procedure Free is new Ada.Unchecked_Deallocation
     (Regexp_Array, Regexp_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Regexp_Value, Regexp_Access);

   function Create_Secondary_Table
     (Map           : Mapping;
      Alphabet_Size : Column_Index;
      First_Table   : Regexp_Array_Access;
      Num_States    : State_Index;
      Start_State   : State_Index;
      End_State     : State_Index)
      return        Regexp;
   --  Creates the definitive table representing the regular expression
   --  This is actually a transformation of the primary table First_Table,
   --  where every state is grouped with the states in its 'no-character'
   --  columns. The transitions between the new states are then recalculated
   --  and if necessary some new states are created.
   --
   --  Note that the resulting finite-state machine is not optimized in
   --  terms of the number of states : it would be more time-consuming to
   --  add a third pass to reduce the number of states in the machine, with
   --  no speed improvement...

   function Create_Reverse_Mapping (R : Regexp) return Reverse_Mapping;
   --  Creates a reverse mapping of a DFA, i.e. Column -> Character.

   procedure Create_Mapping
     (L_Rev_Map     : Reverse_Mapping;
      R_Rev_Map     : Reverse_Mapping;
      Alphabet_Size : out Column_Index;
      Map           : out Mapping);
   --  Creates the united mapping of L_Rev and R_Rev.

   generic
      with function Left_Is_Final (L : Regexp; S : State_Index) return Boolean;
      with function Right_Is_Final (R : Regexp; S : State_Index) return Boolean;
   function Gen_Product_DFA (L, R : Regexp) return Regexp;
   --  Calculates the product DFA.

   ------------
   -- Adjust --
   ------------

   procedure Adjust (R : in out Regexp) is
   begin
      if R.Refcount /= null then
         R.Refcount.all := R.Refcount.all + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (R : in out Regexp) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Natural_Access);
   begin
      if R.Refcount /= null then
         R.Refcount.all := R.Refcount.all - 1;
         if R.Refcount.all = 0 then
            Free (R.Refcount);
            if R.R /= null then
               Free (R.R);
            end if;
         end if;
      end if;
   end Finalize;

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern : String;
      Glob    : Boolean := False)
      return Regexp
   is
      S : String renames Pattern;
      --  The pattern which is really compiled (when the pattern is case
      --  insensitive, we convert this string to lower-cases

      Map : Mapping := (others => 0);
      --  Mapping between characters and columns in the tables

      Alphabet_Size : Column_Index := 0;
      --  Number of significant characters in the regular expression.
      --  This total does not include special operators, such as *, (, ...

      procedure Create_Mapping;
      --  Creates a mapping between characters in the regexp and columns
      --  in the tables representing the regexp. Test that the regexp is
      --  well-formed Modifies Alphabet_Size and Map

      procedure Create_Primary_Table
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index);
      --  Creates the first version of the regexp (this is a non deterministic
      --  finite state machine, which is unadapted for a fast pattern
      --  matching algorithm). We use a recursive algorithm to process the
      --  parenthesis sub-expressions.
      --
      --  Table : at the end of the procedure : Column 0 is for any character
      --  ('.') and the last columns are for no character (closure)
      --  Num_States is set to the number of states in the table
      --  Start_State is the number of the starting state in the regexp
      --  End_State is the number of the final state when the regexp matches

      procedure Create_Primary_Table_Glob
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index);
      --  Same function as above, but it deals with the second possible
      --  grammar for 'globbing pattern', which is a kind of subset of the
      --  whole regular expression grammar.

      procedure Raise_Exception (M : String; Index : Integer);
      pragma No_Return (Raise_Exception);
      --  Raise an exception, indicating an error at character Index in S

      --------------------
      -- Create_Mapping --
      --------------------

      procedure Create_Mapping is

         procedure Add_In_Map (C : Character);
         --  Add a character in the mapping, if it is not already defined

         ----------------
         -- Add_In_Map --
         ----------------

         procedure Add_In_Map (C : Character) is
         begin
            if Map (C) = 0 then
               Alphabet_Size := Alphabet_Size + 1;
               Map (C) := Alphabet_Size;
            end if;
         end Add_In_Map;

         J                 : Integer := S'First;
         Parenthesis_Level : Integer := 0;
         Curly_Level       : Integer := 0;
         Last_Open         : Integer := S'First - 1;

      --  Start of processing for Create_Mapping

      begin
         while J <= S'Last loop
            case S (J) is
               when Open_Bracket =>
                  J := J + 1;

                  if S (J) = '^' then
                     J := J + 1;
                  end if;

                  if S (J) = ']' or S (J) = '-' then
                     J := J + 1;
                  end if;

                  --  The first character never has a special meaning

                  loop
                     if J > S'Last then
                        Raise_Exception
                          ("Ran out of characters while parsing ", J);
                     end if;

                     exit when S (J) = Close_Bracket;

                     if S (J) = '-'
                       and then S (J + 1) /= Close_Bracket
                     then
                        declare
                           Start : constant Integer := J - 1;

                        begin
                           J := J + 1;

                           if S (J) = '\' then
                              J := J + 1;
                           end if;

                           for Char in S (Start) .. S (J) loop
                              Add_In_Map (Char);
                           end loop;
                        end;
                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        Add_In_Map (S (J));
                     end if;

                     J := J + 1;
                  end loop;

                  --  A close bracket must follow a open_bracket,
                  --  and cannot be found alone on the line

               when Close_Bracket =>
                  Raise_Exception
                    ("Incorrect character ']' in regular expression", J);

               when '\' =>
                  if J < S'Last  then
                     J := J + 1;
                     Add_In_Map (S (J));

                  else
                     --  \ not allowed at the end of the regexp

                     Raise_Exception
                       ("Incorrect character '\' in regular expression", J);
                  end if;

               when Open_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level + 1;
                     Last_Open := J;
                  else
                     Add_In_Map (Open_Paren);
                  end if;

               when Close_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level - 1;

                     if Parenthesis_Level < 0 then
                        Raise_Exception
                          ("')' is not associated with '(' in regular "
                           & "expression", J);
                     end if;

                     if J = Last_Open + 1 then
                        Raise_Exception
                          ("Empty parenthesis not allowed in regular "
                           & "expression", J);
                     end if;

                  else
                     Add_In_Map (Close_Paren);
                  end if;

               when '.' =>
                  if Glob then
                     Add_In_Map ('.');
                  end if;

               when '{' =>
                  if not Glob then
                     Add_In_Map (S (J));
                  else
                     Curly_Level := Curly_Level + 1;
                  end if;

               when '}' =>
                  if not Glob then
                     Add_In_Map (S (J));
                  else
                     Curly_Level := Curly_Level - 1;
                  end if;

               when '*' | '?' =>
                  if not Glob then
                     if J = S'First then
                        Raise_Exception
                          ("'*', '+', '?' and '|' operators cannot be in "
                           & "first position in regular expression", J);
                     end if;
                  end if;

               when '|' | '+' =>
                  if not Glob then
                     if J = S'First then

                        --  These operators must apply to a sub-expression,
                        --  and cannot be found at the beginning of the line

                        Raise_Exception
                          ("'*', '+', '?' and '|' operators cannot be in "
                           & "first position in regular expression", J);
                     end if;

                  else
                     Add_In_Map (S (J));
                  end if;

               when others =>
                  Add_In_Map (S (J));
            end case;

            J := J + 1;
         end loop;

         --  A closing parenthesis must follow an open parenthesis

         if Parenthesis_Level /= 0 then
            Raise_Exception
              ("'(' must always be associated with a ')'", J);
         end if;

         if Curly_Level /= 0 then
            Raise_Exception
              ("'{' must always be associated with a '}'", J);
         end if;
      end Create_Mapping;

      --------------------------
      -- Create_Primary_Table --
      --------------------------

      procedure Create_Primary_Table
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index)
      is
         Empty_Char : constant Column_Index := Alphabet_Size + 1;

         Current_State : State_Index := 0;
         --  Index of the last created state

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index);
         --  Add a empty-character transition from State to To_State

         procedure Create_Repetition
           (Repetition : Character;
            Start_Prev : State_Index;
            End_Prev   : State_Index;
            New_Start  : out State_Index;
            New_End    : in out State_Index);
         --  Create the table in case we have a '*', '+' or '?'.
         --  Start_Prev .. End_Prev should indicate respectively the start and
         --  end index of the previous expression, to which '*', '+' or '?' is
         --  applied.

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index);
         --  Fill the table for the regexp Simple.
         --  This is the recursive procedure called to handle () expressions
         --  If End_State = 0, then the call to Create_Simple creates an
         --  independent regexp, not a concatenation
         --  Start_Index .. End_Index is the starting index in the string S.
         --
         --  Warning: it may look like we are creating too many empty-string
         --  transitions, but they are needed to get the correct regexp.
         --  The table is filled as follow ( s means start-state, e means
         --  end-state) :
         --
         --  regexp   state_num | a b * empty_string
         --  -------  ------------------------------
         --    a          1 (s) | 2 - - -
         --               2 (e) | - - - -
         --
         --    ab         1 (s) | 2 - - -
         --               2     | - - - 3
         --               3     | - 4 - -
         --               4 (e) | - - - -
         --
         --    a|b        1     | 2 - - -
         --               2     | - - - 6
         --               3     | - 4 - -
         --               4     | - - - 6
         --               5 (s) | - - - 1,3
         --               6 (e) | - - - -
         --
         --    a*         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1,4
         --               4 (e) | - - - 3
         --
         --    (a)        1 (s) | 2 - - -
         --               2 (e) | - - - -
         --
         --    a+         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1
         --               4 (e) | - - - 3
         --
         --    a?         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1,4
         --               4 (e) | - - - -
         --
         --    .          1 (s) | 2 2 2 -
         --               2 (e) | - - - -

         function Next_Sub_Expression
           (Start_Index : Integer;
            End_Index   : Integer)
            return        Integer;
         --  Returns the index of the last character of the next sub-expression
         --  in Simple. Index cannot be greater than End_Index.

         --------------------
         -- Add_Empty_Char --
         --------------------

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index)
         is
            J : Column_Index := Empty_Char;

         begin
            while Get (Table, State, J) /= 0 loop
               J := J + 1;
            end loop;

            Set (Table, State, J, To_State);
         end Add_Empty_Char;

         -----------------------
         -- Create_Repetition --
         -----------------------

         procedure Create_Repetition
           (Repetition : Character;
            Start_Prev : State_Index;
            End_Prev   : State_Index;
            New_Start  : out State_Index;
            New_End    : in out State_Index)
         is
         begin
            New_Start := Current_State + 1;

            if New_End /= 0 then
               Add_Empty_Char (New_End, New_Start);
            end if;

            Current_State := Current_State + 2;
            New_End   := Current_State;

            Add_Empty_Char (End_Prev, New_End);
            Add_Empty_Char (New_Start, Start_Prev);

            if Repetition /= '+' then
               Add_Empty_Char (New_Start, New_End);
            end if;

            if Repetition /= '?' then
               Add_Empty_Char (New_End, New_Start);
            end if;
         end Create_Repetition;

         -------------------
         -- Create_Simple --
         -------------------

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index)
         is
            J          : Integer := Start_Index;
            Last_Start : State_Index := 0;

         begin
            Start_State := 0;
            End_State   := 0;
            while J <= End_Index loop
               case S (J) is
                  when Open_Paren =>
                     declare
                        J_Start    : constant Integer := J + 1;
                        Next_Start : State_Index;
                        Next_End   : State_Index;

                     begin
                        J := Next_Sub_Expression (J, End_Index);
                        Create_Simple (J_Start, J - 1, Next_Start, Next_End);

                        if J < End_Index
                          and then (S (J + 1) = '*' or else
                                    S (J + 1) = '+' or else
                                    S (J + 1) = '?')
                        then
                           J := J + 1;
                           Create_Repetition
                             (S (J),
                              Next_Start,
                              Next_End,
                              Last_Start,
                              End_State);

                        else
                           Last_Start := Next_Start;

                           if End_State /= 0 then
                              Add_Empty_Char (End_State, Last_Start);
                           end if;

                           End_State := Next_End;
                        end if;
                     end;

                  when '|' =>
                     declare
                        Start_Prev : constant State_Index := Start_State;
                        End_Prev   : constant State_Index := End_State;
                        Start_J    : constant Integer     := J + 1;
                        Start_Next : State_Index := 0;
                        End_Next   : State_Index := 0;

                     begin
                        J := Next_Sub_Expression (J, End_Index);

                        --  Create a new state for the start of the alternative

                        Current_State := Current_State + 1;
                        Last_Start := Current_State;
                        Start_State := Last_Start;

                        --  Create the tree for the second part of alternative

                        Create_Simple (Start_J, J, Start_Next, End_Next);

                        --  Create the end state

                        Add_Empty_Char (Last_Start, Start_Next);
                        Add_Empty_Char (Last_Start, Start_Prev);
                        Current_State := Current_State + 1;
                        End_State := Current_State;
                        Add_Empty_Char (End_Prev, End_State);
                        Add_Empty_Char (End_Next, End_State);
                     end;

                  when Open_Bracket =>
                     Current_State := Current_State + 1;

                     declare
                        Next_State : State_Index := Current_State + 1;

                     begin
                        J := J + 1;

                        if S (J) = '^' then
                           J := J + 1;

                           Next_State := 0;

                           for Column in 0 .. Alphabet_Size loop
                              Set (Table, Current_State, Column,
                                   Value => Current_State + 1);
                           end loop;
                        end if;

                        --  Automatically add the first character

                        if S (J) = '-' or S (J) = ']' then
                           Set (Table, Current_State, Map (S (J)),
                                Value => Next_State);
                           J := J + 1;
                        end if;

                        --  Loop till closing bracket found

                        loop
                           exit when S (J) = Close_Bracket;

                           if S (J) = '-'
                             and then S (J + 1) /= ']'
                           then
                              declare
                                 Start : constant Integer := J - 1;

                              begin
                                 J := J + 1;

                                 if S (J) = '\' then
                                    J := J + 1;
                                 end if;

                                 for Char in S (Start) .. S (J) loop
                                    Set (Table, Current_State, Map (Char),
                                         Value => Next_State);
                                 end loop;
                              end;

                           else
                              if S (J) = '\' then
                                 J := J + 1;
                              end if;

                              Set (Table, Current_State, Map (S (J)),
                                   Value => Next_State);
                           end if;
                           J := J + 1;
                        end loop;
                     end;

                     Current_State := Current_State + 1;

                     --  If the next symbol is a special symbol

                     if J < End_Index
                       and then (S (J + 1) = '*' or else
                                 S (J + 1) = '+' or else
                                 S (J + 1) = '?')
                     then
                        J := J + 1;
                        Create_Repetition
                          (S (J),
                           Current_State - 1,
                           Current_State,
                           Last_Start,
                           End_State);

                     else
                        Last_Start := Current_State - 1;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Last_Start);
                        end if;

                        End_State := Current_State;
                     end if;

                  when '*' | '+' | '?' | Close_Paren | Close_Bracket =>
                     Raise_Exception
                       ("Incorrect character in regular expression :", J);

                  when others =>
                     Current_State := Current_State + 1;

                     --  Create the state for the symbol S (J)

                     if S (J) = '.' then
                        for K in 0 .. Alphabet_Size loop
                           Set (Table, Current_State, K,
                                Value => Current_State + 1);
                        end loop;

                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        Set (Table, Current_State, Map (S (J)),
                             Value => Current_State + 1);
                     end if;

                     Current_State := Current_State + 1;

                     --  If the next symbol is a special symbol

                     if J < End_Index
                       and then (S (J + 1) = '*' or else
                                 S (J + 1) = '+' or else
                                 S (J + 1) = '?')
                     then
                        J := J + 1;
                        Create_Repetition
                          (S (J),
                           Current_State - 1,
                           Current_State,
                           Last_Start,
                           End_State);

                     else
                        Last_Start := Current_State - 1;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Last_Start);
                        end if;

                        End_State := Current_State;
                     end if;

               end case;

               if Start_State = 0 then
                  Start_State := Last_Start;
               end if;

               J := J + 1;
            end loop;
         end Create_Simple;

         -------------------------
         -- Next_Sub_Expression --
         -------------------------

         function Next_Sub_Expression
           (Start_Index : Integer;
            End_Index   : Integer)
            return        Integer
         is
            J              : Integer := Start_Index;
            Start_On_Alter : Boolean := False;

         begin
            if S (J) = '|' then
               Start_On_Alter := True;
            end if;

            loop
               exit when J = End_Index;
               J := J + 1;

               case S (J) is
                  when '\' =>
                     J := J + 1;

                  when Open_Bracket =>
                     loop
                        J := J + 1;
                        exit when S (J) = Close_Bracket;

                        if S (J) = '\' then
                           J := J + 1;
                        end if;
                     end loop;

                  when Open_Paren =>
                     J := Next_Sub_Expression (J, End_Index);

                  when Close_Paren =>
                     return J;

                  when '|' =>
                     if Start_On_Alter then
                        return J - 1;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            return J;
         end Next_Sub_Expression;

      --  Start of Create_Primary_Table

      begin
         Table.all := (others => (others => 0));
         Create_Simple (S'First, S'Last, Start_State, End_State);
         Num_States := Current_State;
      end Create_Primary_Table;

      -------------------------------
      -- Create_Primary_Table_Glob --
      -------------------------------

      procedure Create_Primary_Table_Glob
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index)
      is
         Empty_Char : constant Column_Index := Alphabet_Size + 1;

         Current_State : State_Index := 0;
         --  Index of the last created state

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index);
         --  Add a empty-character transition from State to To_State

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index);
         --  Fill the table for the S (Start_Index .. End_Index).
         --  This is the recursive procedure called to handle () expressions

         --------------------
         -- Add_Empty_Char --
         --------------------

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index)
         is
            J : Column_Index := Empty_Char;

         begin
            while Get (Table, State, J) /= 0 loop
               J := J + 1;
            end loop;

            Set (Table, State, J,
                 Value => To_State);
         end Add_Empty_Char;

         -------------------
         -- Create_Simple --
         -------------------

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index)
         is
            J          : Integer := Start_Index;
            Last_Start : State_Index := 0;

         begin
            Start_State := 0;
            End_State   := 0;

            while J <= End_Index loop
               case S (J) is

                  when Open_Bracket =>
                     Current_State := Current_State + 1;

                     declare
                        Next_State : State_Index := Current_State + 1;

                     begin
                        J := J + 1;

                        if S (J) = '^' then
                           J := J + 1;
                           Next_State := 0;

                           for Column in 0 .. Alphabet_Size loop
                              Set (Table, Current_State, Column,
                                   Value => Current_State + 1);
                           end loop;
                        end if;

                        --  Automatically add the first character

                        if S (J) = '-' or S (J) = ']' then
                           Set (Table, Current_State, Map (S (J)),
                                Value => Current_State);
                           J := J + 1;
                        end if;

                        --  Loop till closing bracket found

                        loop
                           exit when S (J) = Close_Bracket;

                           if S (J) = '-'
                             and then S (J + 1) /= ']'
                           then
                              declare
                                 Start : constant Integer := J - 1;
                              begin
                                 J := J + 1;

                                 if S (J) = '\' then
                                    J := J + 1;
                                 end if;

                                 for Char in S (Start) .. S (J) loop
                                    Set (Table, Current_State, Map (Char),
                                         Value => Next_State);
                                 end loop;
                              end;

                           else
                              if S (J) = '\' then
                                 J := J + 1;
                              end if;

                              Set (Table, Current_State, Map (S (J)),
                                   Value => Next_State);
                           end if;
                           J := J + 1;
                        end loop;
                     end;

                     Last_Start := Current_State;
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Last_Start);
                     end if;

                     End_State := Current_State;

                  when '{' =>
                     declare
                        End_Sub          : Integer;
                        Start_Regexp_Sub : State_Index;
                        End_Regexp_Sub   : State_Index;
                        Create_Start     : State_Index := 0;

                        Create_End : State_Index := 0;
                        --  Initialized to avoid junk warning

                     begin
                        while S (J) /= '}' loop

                           --  First step : find sub pattern

                           End_Sub := J + 1;
                           while S (End_Sub) /= ','
                             and then S (End_Sub) /= '}'
                           loop
                              End_Sub := End_Sub + 1;
                           end loop;

                           --  Second step : create a sub pattern

                           Create_Simple
                             (J + 1,
                              End_Sub - 1,
                              Start_Regexp_Sub,
                              End_Regexp_Sub);

                           J := End_Sub;

                           --  Third step : create an alternative

                           if Create_Start = 0 then
                              Current_State := Current_State + 1;
                              Create_Start := Current_State;
                              Add_Empty_Char (Create_Start, Start_Regexp_Sub);
                              Current_State := Current_State + 1;
                              Create_End := Current_State;
                              Add_Empty_Char (End_Regexp_Sub, Create_End);

                           else
                              Current_State := Current_State + 1;
                              Add_Empty_Char (Current_State, Create_Start);
                              Create_Start := Current_State;
                              Add_Empty_Char (Create_Start, Start_Regexp_Sub);
                              Add_Empty_Char (End_Regexp_Sub, Create_End);
                           end if;
                        end loop;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Create_Start);
                        end if;

                        End_State := Create_End;
                        Last_Start := Create_Start;
                     end;

                  when '*' =>
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Current_State);
                     end if;

                     Add_Empty_Char (Current_State, Current_State + 1);
                     Add_Empty_Char (Current_State, Current_State + 3);
                     Last_Start := Current_State;

                     Current_State := Current_State + 1;

                     for K in 0 .. Alphabet_Size loop
                        Set (Table, Current_State, K,
                             Value => Current_State + 1);
                     end loop;

                     Current_State := Current_State + 1;
                     Add_Empty_Char (Current_State, Current_State + 1);

                     Current_State := Current_State + 1;
                     Add_Empty_Char (Current_State,  Last_Start);
                     End_State := Current_State;

                  when others =>
                     Current_State := Current_State + 1;

                     if S (J) = '?' then
                        for K in 0 .. Alphabet_Size loop
                           Set (Table, Current_State, K,
                                Value => Current_State + 1);
                        end loop;

                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        --  Create the state for the symbol S (J)

                        Set (Table, Current_State, Map (S (J)),
                             Value => Current_State + 1);
                     end if;

                     Last_Start := Current_State;
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Last_Start);
                     end if;

                     End_State := Current_State;

               end case;

               if Start_State = 0 then
                  Start_State := Last_Start;
               end if;

               J := J + 1;
            end loop;
         end Create_Simple;

      --  Start of processing for Create_Primary_Table_Glob

      begin
         Table.all := (others => (others => 0));
         Create_Simple (S'First, S'Last, Start_State, End_State);
         Num_States := Current_State;
      end Create_Primary_Table_Glob;

      ---------------------
      -- Raise_Exception --
      ---------------------

      procedure Raise_Exception (M : String; Index : Integer) is
      begin
         raise Error_In_Regexp with M & " at offset " & Index'Img;
      end Raise_Exception;

   --  Start of processing for Compile

   begin
      --  Special case for the empty string: it always matches, and the
      --  following processing would fail on it.
      if S = "" then
         return (Ada.Finalization.Controlled with
                 R => new Regexp_Value'
                      (Alphabet_Size => 0,
                       Num_States    => 1,
                       Map           => (others => 0),
                       States        => (others => (others => 1)),
                       Is_Final      => (0 => False, 1 => True),
                       Start_State   => 1),
                 Refcount => new Natural'(1));
      end if;

      Create_Mapping;

      --  Creates the primary table

      declare
         Table       : Regexp_Array_Access;
         Num_States  : State_Index;
         Start_State : State_Index;
         End_State   : State_Index;
         R           : Regexp;

      begin
         Table := new Regexp_Array (1 .. 100,
                                    0 .. Alphabet_Size + 10);
         if not Glob then
            Create_Primary_Table (Table, Num_States, Start_State, End_State);
         else
            Create_Primary_Table_Glob
              (Table, Num_States, Start_State, End_State);
         end if;

         --  Creates the secondary table

         R := Create_Secondary_Table
           (Map, Alphabet_Size, Table, Num_States, Start_State, End_State);
         Free (Table);
         return R;
      end;
   end Compile;

   ---------
   -- Get --
   ---------

   function Get
     (Table  : Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index) return State_Index
   is
   begin
      if State <= Table'Last (1)
        and then Column <= Table'Last (2)
      then
         return Table (State, Column);
      else
         return 0;
      end if;
   end Get;

   -----------
   -- Match --
   -----------

   function Match (S : String; R : Regexp) return Boolean is
      Current_State : State_Index := R.R.Start_State;

   begin
      if R.R = null then
         raise Constraint_Error;
      end if;

      for Char in S'Range loop

         Current_State := R.R.States (Current_State, R.R.Map (S (Char)));

         if Current_State = 0 then
            return False;
         end if;

      end loop;

      return R.R.Is_Final (Current_State);
   end Match;

   ---------
   -- Set --
   ---------

   procedure Set
     (Table  : in out Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index;
      Value  : State_Index)
   is
      New_Lines   : State_Index;
      New_Columns : Column_Index;
      New_Table   : Regexp_Array_Access;

   begin
      if State <= Table'Last (1)
        and then Column <= Table'Last (2)
      then
         Table (State, Column) := Value;
      else
         --  Doubles the size of the table until it is big enough that
         --  (State, Column) is a valid index

         New_Lines := Table'Last (1) * (State / Table'Last (1) + 1);
         New_Columns := Table'Last (2) * (Column / Table'Last (2) + 1);
         New_Table := new Regexp_Array (Table'First (1) .. New_Lines,
                                        Table'First (2) .. New_Columns);
         New_Table.all := (others => (others => 0));

         for J in Table'Range (1) loop
            for K in Table'Range (2) loop
               New_Table (J, K) := Table (J, K);
            end loop;
         end loop;

         Free (Table);
         Table := New_Table;
         Table (State, Column) := Value;
      end if;
   end Set;

   ----------------------------
   -- Create_Secondary_Table --
   ----------------------------

   function Create_Secondary_Table
     (Map           : Mapping;
      Alphabet_Size : Column_Index;
      First_Table   : Regexp_Array_Access;
      Num_States    : State_Index;
      Start_State   : State_Index;
      End_State     : State_Index) return Regexp
   is
      pragma Warnings (Off, Num_States);

      type Meta_State is array (1 .. First_Table'Last) of Boolean;
      pragma Pack (Meta_State);
      type Meta_State_Array is array (State_Index range <>) of Meta_State;
      type Meta_State_Array_Access is access Meta_State_Array;
      type Boolean_Array_Access is access Boolean_Array;

      Table       : Regexp_Array_Access     := null;
      Meta_States : Meta_State_Array_Access := null;
      Is_Final    : Boolean_Array_Access    := null;

      Temp_State_Not_Null : Boolean;

      Current_State : State_Index := 1;
      Nb_State      : State_Index := 1;


      --procedure Array_Set is new Growing_Arrays.Gen_Array_Set
        --(Index_Type        => State_Index,
         --Item_Type         => Meta_State,
         --Array_Type        => Meta_State_Array,
         --Array_Access_Type => Meta_State_Array_Access,
         --Default_Item      => Meta_State'(others => False));

      --procedure Array_Set is new Growing_Arrays.Gen_Array_Set
        --(Index_Type        => State_Index,
         --Item_Type         => Boolean,
         --Array_Type        => Boolean_Array,
         --Array_Access_Type => Boolean_Array_Access,
         --Default_Item      => False);

      --procedure Array_Set is new Growing_Arrays.Gen_Array_2d_Set
        --(First_Index_Type  => State_Index,
         --Second_Index_Type => Column_Index,
         --Item_Type         => State_Index,
         --Array_Type        => Regexp_Array,
         --Array_Access_Type => Regexp_Array_Access,
         --Default_Item      => 0);

      procedure Closure
        (State : in out Meta_State;
         Item  :        State_Index);
      --  Compute the closure of the state (that is every other state which
      --  has a empty-character transition) and add it to the state

      -------------
      -- Closure --
      -------------

      procedure Closure
        (State : in out Meta_State;
         Item  : State_Index)
      is
      begin
         if State (Item) then
            return;
         end if;

         State (Item) := True;

         for Column in Alphabet_Size + 1 .. First_Table'Last (2) loop
            if First_Table (Item, Column) = 0 then
               return;
            end if;

            Closure (State, First_Table (Item, Column));
         end loop;
      end Closure;

   --  Start of processing for Create_Secondary_Table

   begin
      Table := new Regexp_Array (1 .. First_Table'Last + 100, 0 .. Alphabet_Size);
      Table.all := (others => (others => 0));

      Meta_States := new Meta_State_Array (1 .. First_Table'Last + 100);
      Meta_States.all := (others => (others => False));

      Is_Final := new Boolean_Array (0 .. First_Table'Last + 100);
      Is_Final.all := (others => False);

      --  Create a new state

      Closure (Meta_States (Current_State), Start_State);

      while Current_State <= Nb_State loop

         --  If this new meta-state includes the primary table end state,
         --  then this meta-state will be a final state in the regexp

         if Meta_States (Current_State)(End_State) then
            Is_Final (Current_State) := True;
            --Array_Set (Is_Final, Current_State, True);
         end if;

         --  For every character in the regexp, calculate the possible
         --  transitions from Current_State

         for Column in 0 .. Alphabet_Size loop
            Meta_States (Nb_State + 1) := (others => False);
            --Array_Set (Meta_States, Nb_State + 1, (others => False));
            Temp_State_Not_Null := False;

            for K in Meta_States (Current_State)'Range loop
               if Meta_States (Current_State)(K)
                 and then First_Table (K, Column) /= 0
               then
                  Closure (Meta_States (Nb_State + 1), First_Table (K, Column));
                  Temp_State_Not_Null := True;
               end if;
            end loop;

            --  If at least one transition existed

            if Temp_State_Not_Null then

               --  Check if this new state corresponds to an old one

               for K in 1 .. Nb_State loop
                  if Meta_States (K) = Meta_States (Nb_State + 1) then
                     Table (Current_State, Column) := K;
                     --Array_Set (Table, Current_State, Column, K);
                     exit;
                  end if;
               end loop;

               --  If not, create a new state

               if Table (Current_State, Column) = 0 then
                  Nb_State := Nb_State + 1;
                  Table (Current_State, Column) := Nb_State;
                  --Array_Set (Table, Current_State, Column, Nb_State);
               end if;
            end if;
         end loop;

         Current_State := Current_State + 1;
      end loop;


      declare
         procedure Free is new Ada.Unchecked_Deallocation
           (Meta_State_Array, Meta_State_Array_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Boolean_Array, Boolean_Array_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Regexp_Array, Regexp_Array_Access);

         R : Regexp_Access;
      begin
         R := new Regexp_Value (Alphabet_Size => Alphabet_Size,
                                Num_States    => Nb_State);
         R.Map         := Map;
         R.Is_Final    := Is_Final (Is_Final'First .. Nb_State);
         R.Start_State := 1;

         for State in 1 .. Nb_State loop
            for K in 0 .. Alphabet_Size loop
               R.States (State, K) := Table (State, K);
            end loop;
         end loop;

         Free (Meta_States);
         Free (Is_Final);
         Free (Table);

         return (Ada.Finalization.Controlled with
                 R        => R,
                 Refcount => new Natural'(1));
      end;
   end Create_Secondary_Table;

   ------------------
   -- Empty_Regexp --
   ------------------

   function Empty_Regexp return Regexp is
   begin
      return (Ada.Finalization.Controlled with
              R => new Regexp_Value'
                   (Alphabet_Size => 0,
                    Num_States    => 1,
                    Map           => (others => 0),
                    States        => (others => (others => 1)),
                    Is_Final      => (0 => False, 1 => False),
                    Start_State   => 1),
              Refcount => new Natural'(1));
   end Empty_Regexp;

   ----------------------------
   -- Create_Reverse_Mapping --
   ----------------------------

   function Create_Reverse_Mapping (R : Regexp) return Reverse_Mapping
   is
      Rev_Map : Reverse_Mapping (1 .. R.R.Alphabet_Size);
   begin
      for C in R.R.Map'Range loop
         if R.R.Map (C) /= 0 then
            Rev_Map (R.R.Map (C)) := C;
         end if;
      end loop;
      return Rev_Map;
   end Create_Reverse_Mapping;

   --------------------
   -- Create_Mapping --
   --------------------

   procedure Create_Mapping
     (L_Rev_Map     : Reverse_Mapping;
      R_Rev_Map     : Reverse_Mapping;
      Alphabet_Size : out Column_Index;
      Map           : out Mapping)
   is
      procedure Add_In_Map (C : Character);
      --  Add a character in the mapping, if it is not already defined

      procedure Add_In_Map (C : Character) is
      begin
         if Map (C) = 0 then
            Alphabet_Size := Alphabet_Size + 1;
            Map (C) := Alphabet_Size;
         end if;
      end Add_In_Map;
   begin
      Alphabet_Size := 0;
      Map           := (others => 0);
      for J in L_Rev_Map'Range loop
         Add_In_Map (L_Rev_Map (J));
      end loop;
      for J in R_Rev_Map'Range loop
         Add_In_Map (R_Rev_Map (J));
      end loop;
   end Create_Mapping;

   ---------------------
   -- Gen_Product_DFA --
   ---------------------

   function Gen_Product_DFA (L, R : Regexp) return Regexp
   is
      function Trans (S, T : State_Index) return State_Index;
      --  Transforms the states S, T of L, R to a state (S, T) of the product
      --  DFA.

      pragma Inline (Trans);

      function Trans (S, T : State_Index) return State_Index is
      begin
         return S * (R.R.Num_States + 1) + (T + 1);
      end Trans;

      P : Regexp_Access := null;
   begin
      declare
         L_Rev_Map     : constant Reverse_Mapping := Create_Reverse_Mapping (L);
         R_Rev_Map     : constant Reverse_Mapping := Create_Reverse_Mapping (R);
         Num_States    : constant State_Index :=
            (L.R.Num_States + 1) * (R.R.Num_States + 1);
         Map           : Mapping;
         Alphabet_Size : Column_Index;
      begin
         Create_Mapping (L_Rev_Map, R_Rev_Map, Alphabet_Size, Map);
         P := new Regexp_Value (Alphabet_Size => Alphabet_Size,
                                Num_States    => Num_States);
         P.Map := Map;
      end;

      declare
         L_Map    : Mapping       renames L.R.Map;
         R_Map    : Mapping       renames R.R.Map;
         Map      : Mapping       renames P.Map;
         L_States : Regexp_Array  renames L.R.States;
         R_States : Regexp_Array  renames R.R.States;
         States   : Regexp_Array  renames P.States;
         Is_Final : Boolean_Array renames P.Is_Final;
      begin
         States := (others => (others => 0));
         for C in Character'Range loop
            for S in L_States'Range (1) loop
               for T in R_States'Range (1) loop
                  pragma Assert (Trans (S, T) in States'Range (1));
                  States (Trans (S, T), Map (C)) :=
                     Trans (L_States (S, L_Map (C)),
                            R_States (T, R_Map (C)));
               end loop;
            end loop;
         end loop;

         Is_Final := (others => False);
         for S in 0 .. L_States'Last (1) loop
            for T in 0 .. R_States'Last (1) loop
               Is_Final (Trans (S, T)) := Left_Is_Final (L, S) and
                                          Right_Is_Final (R, T);
            end loop;
         end loop;

         P.Start_State := Trans (L.R.Start_State, R.R.Start_State);
      end;

      return (Ada.Finalization.Controlled with
              R        => P,
              Refcount => new Natural'(1));
   exception
      when others =>
         if P /= null then
            Free (P);
         end if;
         raise;
   end Gen_Product_DFA;

   -----------
   -- Union --
   -----------

   function Union (L, R : Regexp) return Regexp
   is
      procedure Add_DFA
        (R       : Regexp;
         Rev_Map : Reverse_Mapping;
         Offset  : State_Index);
      --  Adds the DFA R to the union NFA. The states of the DFA R are
      --  transformed by adding Offset to avoid clashes with the states already
      --  in the union NFA.

      L_Rev_Map       : constant Reverse_Mapping := Create_Reverse_Mapping (L);
      R_Rev_Map       : constant Reverse_Mapping := Create_Reverse_Mapping (R);
      Alphabet_Size   : Column_Index;
      Map             : Mapping;
      NFA             : Regexp_Array_Access := null;
      NFA_Start_State : State_Index;
      NFA_End_State   : State_Index;
      Epsilon_Column  : Column_Index;

      procedure Add_DFA
        (R       : Regexp;
         Rev_Map : Reverse_Mapping;
         Offset  : State_Index)
      is
         function T (State : State_Index) return State_Index;
         --  Transforms a state from the input DFA to the union NFA.

         function T (Column : Column_Index) return Column_Index;
         --  Transforms a column from the input DFA to the union NFA.

         pragma Inline (T);

         function T (State : State_Index) return State_Index is
         begin
            return Offset + State;
         end T;

         function T (Column : Column_Index) return Column_Index is
         begin
            if Column = 0 then
               return 0;
            else
               pragma Assert (Column in Rev_Map'Range);
               return Map (Rev_Map (Column));
            end if;
         end T;

         DFA          : Regexp_Array renames R.R.States;
         DFA_Is_Final : Boolean_Array renames R.R.Is_Final;
      begin
         pragma Assert (NFA_Start_State not in
                        T (DFA'First (1)) .. T (DFA'Last (1)));
         pragma Assert (NFA_End_State not in
                        T (DFA'First (1)) .. T (DFA'Last (1)));

         for Next_Epsilon_Column in Epsilon_Column .. Column_Index'Last loop
            if NFA (NFA_Start_State, Next_Epsilon_Column) = 0 then
               NFA (NFA_Start_State, Next_Epsilon_Column) :=
                  T (R.R.Start_State);
               exit;
            end if;
         end loop;

         for State in DFA'Range (1) loop
            for Column in DFA'Range (2) loop
               if DFA (State, Column) /= 0 then
                  pragma Assert (T (State) in NFA'Range (1));
                  pragma Assert (T (Column) in NFA'Range (2));
                  NFA (T (State), T (Column)) := T (DFA (State, Column));
               end if;
            end loop;

            if DFA_Is_Final (State) then
               NFA (T (State), Epsilon_Column) := NFA_End_State;
            end if;
         end loop;
      end Add_DFA;

   begin
      Create_Mapping (L_Rev_Map, R_Rev_Map, Alphabet_Size, Map);
      NFA := new Regexp_Array (1 .. L.R.Num_States + R.R.Num_States + 2,
                               0 .. Alphabet_Size + 2);
      --  To comply with Create_Secondary_Table, the first column of NFA
      --  denotes the transitions for all characters ('.') (not used here)
      --  and the last columns denote epsilon transitions. Since each state of
      --  the DFAs needs one epsilon transition (iff it's final) and the start
      --  state of the NFA needs two epsilon transitions (to the start states of
      --  the DFAs), we need two extra columns, hence the Alphabet_Size + 2.
      NFA.all := (others => (others => 0));
      NFA_Start_State := NFA'Last (1) - 1;
      NFA_End_State   := NFA'Last (1);
      Epsilon_Column  := Alphabet_Size + 1;
      Add_DFA (L, L_Rev_Map, Offset => 0);
      Add_DFA (R, R_Rev_Map, Offset => L.R.Num_States);
      declare
         R : Regexp;
      begin
         R := Create_Secondary_Table (Map, Alphabet_Size, NFA, NFA'Length (1),
                                      NFA_Start_State, NFA_End_State);
         Free (NFA);
         return R;
      end;
   exception
      when others =>
         if NFA /= null then
            Free (NFA);
         end if;
         raise;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   function Intersection (L, R : Regexp) return Regexp
   is
      function Is_Final (R : Regexp; S : State_Index) return Boolean;
      pragma Inline (Is_Final);

      function Is_Final (R : Regexp; S : State_Index) return Boolean is
      begin
         return R.R.Is_Final (S);
      end Is_Final;

      function Intersection_DFA is new Gen_Product_DFA
        (Left_Is_Final  => Is_Final,
         Right_Is_Final => Is_Final);
   begin
      return Intersection_DFA (L, R);
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   function Difference (L, R : Regexp) return Regexp
   is
      function Is_Final (R : Regexp; S : State_Index) return Boolean;
      pragma Inline (Is_Final);

      function Is_Not_Final (R : Regexp; S : State_Index) return Boolean;
      pragma Inline (Is_Not_Final);

      function Is_Final (R : Regexp; S : State_Index) return Boolean is
      begin
         return R.R.Is_Final (S);
      end Is_Final;

      function Is_Not_Final (R : Regexp; S : State_Index) return Boolean is
      begin
         return not R.R.Is_Final (S);
      end Is_Not_Final;

      function Difference_DFA is new Gen_Product_DFA
        (Left_Is_Final  => Is_Final,
         Right_Is_Final => Is_Not_Final);
   begin
      return Difference_DFA (L, R);
   end Difference;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (R : Regexp) return Boolean
   is
      type Mark is (Unmarked, Marked, Visited);
      --  States are either Unmarked or Marked or, for performance reasons,
      --  Visited.

      procedure Mark_Next_State
        (S           : State_Index;
         C           : Column_Index;
         Have_Marked : out Boolean;
         Final       : out Boolean);
      --  Marks T := delta(S, C), if T is currently unmarked. Have_Marked is set
      --  to True iff the state was Unmarked previously. Final is set to True
      --  iff T is final.

      Marks : array (0 .. R.R.Num_States) of Mark;

      procedure Mark_Next_State
        (S           : State_Index;
         C           : Column_Index;
         Have_Marked : out Boolean;
         Final       : out Boolean)
      is
         T : constant State_Index := R.R.States (S, C);
      begin
         if Marks (T) = Unmarked then
            Marks (T)   := Marked;
            Have_Marked := True;
            Final       := R.R.Is_Final (T);
         else
            Have_Marked := False;
            Final       := False;
         end if;
      end Mark_Next_State;

   begin
      Marks := (others => Unmarked);
      Marks (R.R.Start_State) := Marked;
      loop
         declare
            Something_Marked : Boolean := False;
         begin
            for S in R.R.States'Range (1) loop
               if Marks (S) = Marked then
                  Marks (S) := Visited;
                  for C in R.R.States'Range (2) loop
                     declare
                        Have_Marked : Boolean;
                        Final       : Boolean;
                     begin
                        Mark_Next_State (S, C, Have_Marked, Final);
                        Something_Marked := Something_Marked or Have_Marked;
                        if Have_Marked and Final then
                           return False;
                        end if;
                     end;
                  end loop;
               end if;
            end loop;

            if not Something_Marked then
               --  No further states can be reached, in particular no final,
               --  therefore the product DFA accepts nothing at all.
               return True;
            end if;
         end;
      end loop;
   end Is_Empty;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (L, R : Regexp) return Boolean is
   begin
      return Is_Empty (Difference (L, R));
   end Is_Subset;

end DB.Utils.Regular_Expressions;

