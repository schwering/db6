function DB.Util.Gen_Maximum
  (M : Number_Type;
   N : Number_Type)
   return Number_Type
is begin
   if M > N then
      return M;
   else
      return N;
   end if;
end DB.Util.Gen_Maximum;

