-- Abstract:
--
-- Tasks that run over cursors.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with DB.Types.Keys;
with DB.Types.Values;
private with DB.Locks.Mutexes;
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
     (Map           : not null DB.Maps.Map_Ref_Type;
      URL_Path      : Unbounded_String;
      Offset        : Natural;
      Lower         : DB.Maps.Bound_Type;
      Upper         : DB.Maps.Bound_Type;
      Column_Regexp : String)
      return Cursor_Ref_Type;

   procedure Release_Cursor (Cursor : in not null Cursor_Ref_Type);

   procedure Next
     (Cursor  : in out Cursor_Ref_Type;
      Key     :    out DB.Types.Keys.Key_Type;
      Value   :    out DB.Types.Values.Value_Type;
      Success :    out Boolean);
   -- Returns the next Key/Value pair from the cursor and sets Success to True
   -- if it succeeds, or finalizes Cursors, sets Cursor to null and Success to
   -- False.

   procedure Push_Back
     (Cursor : in not null Cursor_Ref_Type;
      Key    : in          DB.Types.Keys.Key_Type;
      Value  : in          DB.Types.Values.Value_Type);
   -- Pushes back the given Key/Value pair to the cursor.
   -- No second Push_Back may be issued directly after one; in between, there
   -- must be one or more Next calls.

   procedure Finalize (Cursor : in out Cursor_Ref_Type);
   -- Finalizes all resources acquired by the cursor.

private
   Timeout : constant := 60;

   type Cursor_Self_Ref_Type is access all Cursor_Type;

   task type Killer_Task_Type is
      entry Start (Cursor : in Cursor_Ref_Type);
      entry Stop;
   end Killer_Task_Type;

   type Cursor_Type is limited
      record
         URL_Path      : Unbounded_String;
         Cursor        : DB.Maps.Cursor_Ref_Type;
         Offset        : Natural := 0;
         Column_Regexp : DB.Utils.Regexps.Regexp_Type;
         Has_Last      : Boolean := False;
         Last_Key      : DB.Types.Keys.Key_Type;
         Last_Value    : DB.Types.Values.Value_Type;
         Released      : Boolean := False; -- for assertions / debugging
         Killer        : Killer_Task_Type;
      end record;
   

   package Vectors is new Ada.Containers.Vectors
     (Positive, Cursor_Ref_Type);

   package Maps is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Vectors.Vector, "=" => Vectors."=");


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

      procedure Kill (Cursor : in out Cursor_Ref_Type; Success : out Boolean);
      -- Looks whether Cursor is currently in the cache and if so, finalizes it
      -- and sets Success to True.

      procedure Kill_All;

   private
      Map : Maps.Map;
   end Cursors;


   Trash_Size : constant := 10;

   type Cursor_Ref_Array_Type is array (1 .. Trash_Size) of Cursor_Ref_Type;

   protected Trash is
      entry Throw_Away (Cursor : in not null Cursor_Ref_Type);
      entry Empty;
   private
      Cursors : Cursor_Ref_Array_Type := (others => null);
   end Trash;

   task Cleaner_Task;

end REST.Maps.Cursors;

