-- Abstract:
--
-- Root package of the REST interface.
--
-- Copyright 2010--2011 Christoph Schwering

with DB.Types.Keys;
with DB.Types.Times;

package REST is

   URL_Path_Error             : exception;
   Stream_Error               : exception;
   Malformed_Input_Data_Error : exception;
   Insertion_Error            : exception;
   Deletion_Error             : exception;

   Count_Param          : constant String := "n";
   Offset_Param         : constant String := "o";
   Reverse_Param        : constant String := "rev";
   From_Excl_Param      : constant String := "excl1";
   To_Excl_Param        : constant String := "excl2";
   Yes_Value            : constant String := "y";

   Everything_Regexp    : constant String := ".*";

   Infinity_Row         : constant String := "_inf";
   Next_URL_Key         : constant String := "_succ";

   Default_Count        : constant Positive := 5;

   SSL                  : constant Boolean := False;
   Certificate_Filename : constant String := "SSL/aws.pem";

   function Make_Key
     (Row, Column : String;
      Time        : DB.Types.Times.Time_Type)
      return DB.Types.Keys.Key_Type;

   function Make_Key
     (Row : String;
      Max : Boolean := False)
      return DB.Types.Keys.Key_Type;

   function Img (N : Integer) return String;

end REST;

