-- Abstract:
--
-- Tasks that run over cursors.
--
-- URL_Path is intended to include in some standardized format the table, the
-- column regular expression, the lower bound row and the upper bound row.
--
-- The user is required to call Release if he doesn't need the cursor anymore
-- to put it back into the cache. Together with Next, this mechanism cares about
-- finalization of cursors.
--
--
-- XXX TODO Make somehow the Clean_Task a daemon-thread.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

private with Ada.Calendar;
private with Ada.Containers.Ordered_Multisets;
with Ada.Strings.Unbounded;

with DB.Types.Keys;
with DB.Types.Values;
private with DB.Utils.Regexps;

package REST.Maps.Cursors is
   use Ada.Strings.Unbounded;

   type Cursor_Type is limited private;
   type Cursor_Ref_Type is access Cursor_Type;

   function Bound
     (Row       : String;
      Inclusive : Boolean;
      Lower     : Boolean)
      return DB.Maps.Bound_Type;

   function New_Cursor
     (Map               : not null DB.Maps.Map_Ref_Type;
      URL_Path          : Unbounded_String;
      Offset            : Natural;
      Lower             : DB.Maps.Bound_Type;
      Upper             : DB.Maps.Bound_Type;
      Has_Column_Regexp : Boolean;
      Column_Regexp     : String)
      return Cursor_Ref_Type;

   procedure Release (Cursor : in not null Cursor_Ref_Type);
   -- Releasing the cursor means that the cursor is managed by the package
   -- again. Subsequent New_Cursor calls might return exactly the cursor. After
   -- some time, the cursor will be finalized.

   procedure Next
     (Cursor  : in out Cursor_Ref_Type;
      Process : access procedure
                        (Key   : in DB.Types.Keys.Key_Type;
                         Value : in DB.Types.Values.Value_Type);
      EOF     :    out Boolean);
   -- Iterates over the next elements which have the equivalent row value from
   -- Cursor. For each Key/Value pair of these, Process is called; so for each
   -- Process call, all arguments K and K' have K.Row = K'.Row. Iff Cursor
   -- reaches its end during this procedure, EOF is set to True, which doesn't
   -- indicate an error but only that subsequent Next procedures are not
   -- necessary. Furthermore, in this case the resources held by Cursor are
   -- freed and Cursor is set to null.

private
   Timeout : constant Duration := 60.0;

   type Cursor_Self_Ref_Type is access all Cursor_Type;

   type Cursor_Type is limited
      record
         Search_Only       : Boolean;
         URL_Path          : Unbounded_String;
         Offset            : Natural := 0;
         Cursor            : DB.Maps.Cursor_Ref_Type;
         Has_Column_Regexp : Boolean;
         Column_Regexp     : DB.Utils.Regexps.Regexp_Type;
         Reuse_Last        : Boolean := False;
         Has_Last          : Boolean := False;
         Last_Key          : DB.Types.Keys.Key_Type;
         Last_Value        : DB.Types.Values.Value_Type;
         Last_Used         : Ada.Calendar.Time;
         Released          : Boolean := False; -- for assertions / debugging
      end record;
   
   function Less (A, B : Cursor_Ref_Type) return Boolean;
   function Equal (A, B : Cursor_Ref_Type) return Boolean;

   procedure Finalize (Cursor : in out Cursor_Ref_Type);

   package Multisets is new Ada.Containers.Ordered_Multisets
     (Cursor_Ref_Type, Less, Equal);

   protected Cursors is
      procedure Put (Cursor : in not null Cursor_Ref_Type);
      -- Adds Cursor to the cache (Map).

      procedure Get
        (URL_Path : in  Unbounded_String;
         Offset   : in  Natural;
         Cursor   : out Cursor_Ref_Type);
      -- Extracts a cursor from the cache (Map) and sets it to Cursor.
      -- The cursor must match URL_Path, i.e. the bounds must be correct, and
      -- the Offset must match.

      procedure Clean;

   private
      Map : Multisets.Set;
   end Cursors;

end REST.Maps.Cursors;

