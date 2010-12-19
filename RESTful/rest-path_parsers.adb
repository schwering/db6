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
         raise Constraint_Error;
      end if;
      if Iterator = Final then
         raise Constraint_Error;
      end if;
      if Path (Iterator.I) /= '/' then
         raise Constraint_Error;
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

end REST.Path_Parsers;

