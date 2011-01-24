-- Abstract:
--
-- A copy of GNAT.Regexp from AdaCore.
--
-- Additions:
--  * Empty_Regexp           (\emptyset)
--  * Union                  (Lang(L) \cup Lang(R))
--  * Intersection           (Lang(L) \cap Lang(R))
--  * Difference             (Lang(L) \setminus Lang(R))
--  * Is_Empty               (Lang(R) = \emptyset?)
--  * Is_Subset              (Lang(L) \subseteq Lang(R)?)
--
-- Internal Modifications:
--  * The zero state has a final-flag.
--  * Each DFA has a specific start state (formerly, start state was 1).
--  * Gen_Product_DFA, which builds a product DFA.
--  * The DFAs use reference-counting.
--  * The power set construction is top-level now and grows the DFA dynamically.
--
-- I had to copy the whole package because one cannot (and shouldn't) make child
-- packages of system packages (System.Regexp) nor renamed packages
-- (GNAT.Regexp).
--
-- Copyright 1998-2008, AdaCore
-- Copyright 2010 Christoph Schwering

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . R E G E X P                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2008, AdaCore                     --
--                     Copyright (C) 2010 Christoph Schwering               --
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

--  Simple Regular expression matching

--  This package provides a simple implementation of a regular expression
--  pattern matching algorithm, using a subset of the syntax of regular
--  expressions copied from familiar Unix style utilities.

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.Regexp (file g-regexp.ads).

private with Ada.Finalization;

package DB.Utils.Regexps is
   pragma Preelaborate;

   --  The regular expression must first be compiled, using the Compile
   --  function, which creates a finite state matching table, allowing
   --  very fast matching once the expression has been compiled.

   --  The following is the form of a regular expression, expressed in Ada
   --  reference manual style BNF is as follows

   --     regexp ::= term

   --     regexp ::= term | term          -- alternation (term or term ...)

   --     term ::= item

   --     term ::= item item ...          -- concatenation (item then item)

   --     item ::= elmt                   -- match elmt
   --     item ::= elmt *                 -- zero or more elmt's
   --     item ::= elmt +                 -- one or more elmt's
   --     item ::= elmt ?                 -- matches elmt or nothing

   --     elmt ::= nchr                   -- matches given character
   --     elmt ::= [nchr nchr ...]        -- matches any character listed
   --     elmt ::= [^ nchr nchr ...]      -- matches any character not listed
   --     elmt ::= [char - char]          -- matches chars in given range
   --     elmt ::= .                      -- matches any single character
   --     elmt ::= ( regexp )             -- parens used for grouping

   --     char ::= any character, including special characters
   --     nchr ::= any character except \()[].*+?^ or \char to match char
   --     ... is used to indication repetition (one or more terms)

   --  See also regexp(1) man page on Unix systems for further details

   --  A second kind of regular expressions is provided. This one is more
   --  like the wild card patterns used in file names by the Unix shell (or
   --  DOS prompt) command lines. The grammar is the following:

   --     regexp ::= term

   --     term   ::= elmt

   --     term   ::= elmt elmt ...     -- concatenation (elmt then elmt)
   --     term   ::= *                 -- any string of 0 or more characters
   --     term   ::= ?                 -- matches any character
   --     term   ::= [char char ...]   -- matches any character listed
   --     term   ::= [char - char]     -- matches any character in given range
   --     term   ::= {elmt, elmt, ...} -- alternation (matches any of elmt)

   --  Important note : This package was mainly intended to match regular
   --  expressions against file names. The whole string has to match the
   --  regular expression. If only a substring matches, then the function
   --  Match will return False.

   type Regexp is private;
   --  Private type used to represent a regular expression

   subtype Regexp_Type is Regexp;
   --  In the DB-package, there's we want to have the _Type suffix.

   Error_In_Regexp : exception;
   --  Exception raised when an error is found in the regular expression

   function Compile (Pattern : String; Glob : Boolean := False) return Regexp;
   --  Compiles a regular expression S. If the syntax of the given
   --  expression is invalid (does not match above grammar), Error_In_Regexp
   --  is raised. If Glob is True, the pattern is considered as a 'globbing
   --  pattern', that is a pattern as given by the second grammar above.
   --  As a special case, if Pattern is the empty string it will always
   --  match.

   function Match (S : String; R : Regexp) return Boolean;
   --  True if S matches R, otherwise False. Raises Constraint_Error if
   --  R is an uninitialized regular expression value.

   function Empty_Regexp return Regexp;
   --  Regular regular expression that accepts nothing.

   function Union (L, R : Regexp) return Regexp;
   --  Creates a new regular expression that accepts the union of the languages
   --  of L and R. This function is faster than re-compiling (S1)|(S2) where S1,
   --  S2 are the string representations of L and R, because it directly works
   --  on the state machines.

   function Intersection (L, R : Regexp) return Regexp;
   --  Creates a new regular expression that accepts the intersection of the
   --  languages of L and R. The intersection is accepted by the product DFA.

   function Difference (L, R : Regexp) return Regexp;
   --  Creates a new regular expression that accepts the intersection of the
   --  languages of L and R. The intersection is accepted by the product DFA.
   --  The difference-automaton is the same like the product-automaton but with
   --  different set of final states: a state is final iff it is final in A but
   --  not in B.

   function Is_Empty (R : Regexp) return Boolean;
   --  Returns True iff the regular expression accepts nothing at all.
   --  This is done by a (not very efficient) marking algorithm.

   function Is_Subset (L, R : Regexp) return Boolean;
   --  Determines whether the language accepted by L is a subset of
   --  (or equal to) the language accepted by R.
   --  Note that A is a subset of B
   --       iff  for all x: x in A => x in B
   --       iff  (A \ B) is empty
   --  Hence it suffices to check whether (Lang(L) \ Lang(R)) is empty.
   --  The function is the same as Is_Empty (Difference (L, R)).

   function Draw (N : String; R : Regexp) return String;
   --  Draws the DFA for debugging purposes.

private
   type Regexp_Value;

   type Natural_Access is access Natural;
   type Regexp_Access is access Regexp_Value;

   type Regexp is new Ada.Finalization.Controlled with record
      R        : Regexp_Access  := null;
      Refcount : Natural_Access := null;
   end record;

   pragma Finalize_Storage_Only (Regexp);

   procedure Finalize (R : in out Regexp);
   --  Free the memory occupied by R

   procedure Adjust (R : in out Regexp);
   --  Called after an assignment (do a copy of the Regexp_Access.all)

end DB.Utils.Regexps;
