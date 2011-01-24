-- Abstract:
--
-- see spec
--
-- Copyright 2011 Christoph Schwering

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with DB.DSA.Utils.Gen_Small_LRU_Caches;

package body DB.Utils.Regexps.Cache is
   use Ada.Strings.Unbounded;

   function Hash (S : Unbounded_String) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
      H : Utils.Hash_Type := 0;
   begin
      for I in 1 .. Length (S) loop
         H := H + Character'Pos (Element (S, I)) * 31**I;
      end loop;
      return H;
   end Hash;


   package Caches is new DSA.Utils.Gen_Small_LRU_Caches
     (Key_Type    => Unbounded_String,
      Value_Type  => Regexp_Type,
      Hash        => Hash,
      "="         => "=",
      Slot_Count  => 10);


   Cache : Caches.Cache_Type;


   function Compile (Pattern : String; Glob : Boolean := False) return Regexp
   is
      U_Pattern : constant Unbounded_String := To_Unbounded_String (Pattern);
      Regexp    : Regexp_Type;
      Found     : Boolean;
   begin
      Caches.Get (Cache, U_Pattern, Regexp, Found);
      if Found then
         return Regexp;
      else
         declare
            Regexp : constant Regexps.Regexp_Type :=
              Regexps.Compile (Pattern, Glob);
         begin
            Caches.Put (Cache, U_Pattern, Regexp);
            return Regexp;
         end;
      end if;
   end Compile;

end DB.Utils.Regexps.Cache;

