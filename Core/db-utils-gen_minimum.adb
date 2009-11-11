function DB.Utils.Gen_Minimum
  (M : Number_Type;
   N : Number_Type)
   return Number_Type is
begin
   if M < N then
      return M;
   else
      return N;
   end if;
end DB.Utils.Gen_Minimum;

