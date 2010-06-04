-- Abstract:
--
-- This is an adjusted and extended version of GNAT.Regexp:
--
-- The documentation from GNAT.Regexp:
-- Simple Regular expression matching
-- This package provides a simple implementation of a regular expression
-- pattern matching algorithm, using a subset of the syntax of regular
-- expressions copied from familiar Unix style utilities.
--
-- Changes to GNAT.Regexp:
-- It's adjusted in the sense that I've added _Type-suffixes.
-- Furthermore, I've removed the case-insensitivity-option.
--
-- Additions to GNAT.Regexp:
-- I've added a Difference function that determines the difference of two
-- regular expressions, i.e. for two DFAs A_1 and A_2 it computes a DFA A such
-- that L(A) = L(A_2) \ L(A_1). In fact, then
--    A = (Q, Sigma, delta, q_0, F)
-- where
--    Q   = Q_1 x Q_2
--    q_0 = (q_0^1, q_0^2)
--    F   = { (q_1, q_2 | q_1 in F_1 and q_2 not in F_2 }
--    delta((q_1, q_2), a) = (delta_1(q_1, a), delta_2(q_2, a)).
-- Another thing I've added is an Is_Empty function that checks whether a DFA
-- accepts no words.
-- A combination of Difference and Is_Empty is the Is_Subset function:
-- Is_Subset(A, B) holds iff the difference of A and B is empty.
--
-- Copyright 1999-2008 AdaCore
-- Copyright 2010 Christoph Schwering

with Ada.Finalization;

package DB.Utils.Regular_Expressions is

   -- The regular expression must first be compiled, using the Compile
   -- function, which creates a finite state matching table, allowing
   -- very fast matching once the expression has been compiled.
   --
   -- The following is the form of a regular expression, expressed in Ada
   -- reference manual style BNF is as follows
   --
   --    regexp ::= term
   --
   --    regexp ::= term | term          -- alternation (term or term ...)
   --
   --    term ::= item
   --
   --    term ::= item item ...          -- concatenation (item then item)
   --
   --    item ::= elmt                   -- match elmt
   --    item ::= elmt *                 -- zero or more elmt's
   --    item ::= elmt +                 -- one or more elmt's
   --    item ::= elmt ?                 -- matches elmt or nothing
   --
   --    elmt ::= nchr                   -- matches given character
   --    elmt ::= [nchr nchr ...]        -- matches any character listed
   --    elmt ::= [^ nchr nchr ...]      -- matches any character not listed
   --    elmt ::= [char - char]          -- matches chars in given range
   --    elmt ::= .                      -- matches any single character
   --    elmt ::= ( regexp )             -- parens used for grouping
   --
   --    char ::= any character, including special characters
   --    nchr ::= any character except \()[].*+?^ or \char to match char
   --    ... is used to indication repetition (one or more terms)
   --
   -- See also regexp(1) man page on Unix systems for further details
   --
   -- A second kind of regular expressions is provided. This one is more
   -- like the wild card patterns used in file names by the Unix shell (or
   -- DOS prompt) command lines. The grammar is the following:
   --
   --    regexp ::= term
   --
   --    term   ::= elmt
   --
   --    term   ::= elmt elmt ...     -- concatenation (elmt then elmt)
   --    term   ::= *                 -- any string of 0 or more characters
   --    term   ::= ?                 -- matches any character
   --    term   ::= [char char ...]   -- matches any character listed
   --    term   ::= [char - char]     -- matches any character in given range
   --    term   ::= {elmt, elmt, ...} -- alternation (matches any of elmt)
   --
   -- Important note: This package was mainly intended to match regular
   -- expressions against file names. The whole string has to match the
   -- regular expression. If only a substring matches, then the function
   -- Match will return False.

   type Regexp_Type is private;
   -- Private type used to represent a regular expression

   Error_In_Regexp : exception;
   -- Exception raised when an error is found in the regular expression

   function Compile
     (Pattern : String;
      Glob    : Boolean := False)
      return Regexp_Type;
   -- Compiles a regular expression S. If the syntax of the given
   -- expression is invalid (does not match above grammar), Error_In_Regexp
   -- is raised. If Glob is True, the pattern is considered as a 'globbing
   -- pattern', that is a pattern as given by the second grammar above.
   -- As a special case, if Pattern is the empty string it will always
   -- match.

   function Match (S : String; R : Regexp_Type) return Boolean;
   -- True if S matches R, otherwise False. Raises Constraint_Error if
   -- R is an uninitialized regular expression value.

private
   type Regexp_Value;

   type Regexp_Access is access Regexp_Value;

   type Regexp_Type is new Ada.Finalization.Controlled with record
      R : Regexp_Access := null;
   end record;

   pragma Finalize_Storage_Only (Regexp_Type);

   procedure Finalize (R : in out Regexp_Type);
   -- Free the memory occupied by R

   procedure Adjust (R : in out Regexp_Type);
   -- Called after an assignment (do a copy of the Regexp_Access.all)

end DB.Utils.Regular_Expressions;

