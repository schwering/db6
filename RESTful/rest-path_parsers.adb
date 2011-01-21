-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

package body REST.Path_Parsers is

   procedure Next (Path : in String; Iterator : in out Iterator_Type) is
   begin
      if Iterator.I = Initial then

         if Path'Length = 0 or else Path (Path'First) /= '/' then
            Iterator := Final;
         else
            Iterator.I := Path'First;
         end if;

      else

         for I in Iterator.I + 1 .. Path'Last - 1 loop
            if Path (I) = '/' then
               Iterator.I := I;
               return;
            end if;
         end loop;

         Iterator := Final;

      end if;
   end Next;


   function Value (Path : String; Iterator : Iterator_Type) return String is
   begin
      if Iterator.I = Initial then
         raise URL_Path_Error;
      end if;
      if Iterator = Final then
         raise URL_Path_Error;
      end if;
      if Path (Iterator.I) /= '/' then
         raise URL_Path_Error;
      end if;

      for I in Iterator.I + 1 .. Path'Last loop
         if Path (I) = '/' then
            return Path (Iterator.I + 1 .. I - 1);
         end if;
      end loop;
      return Path (Iterator.I + 1 .. Path'Last);
   end Value;


   function Is_Final (Iterator : Iterator_Type) return Boolean is
   begin
      return Iterator = Final;
   end Is_Final;


   function Element
     (URL     : AWS.URL.Object;
      N       : Positive;
      Default : String := "")
      return String is
   begin
      return Element (AWS.URL.Pathname (URL), N, Default);
   end Element;


   function Element
     (Path    : String;
      N       : Positive;
      Default : String := "")
      return String
   is
      Iterator : Iterator_Type;
      Index    : Natural := 0;
   begin
      loop
         Next (Path, Iterator);
         exit when Is_Final (Iterator);
         Index := Index + 1;
         if Index = N then
            return Value (Path, Iterator);
         end if;
      end loop;
      return Default;
   end Element;

end REST.Path_Parsers;

