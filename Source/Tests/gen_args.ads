with Gen_Jobs;
with Random;

generic
   with package Jobs is new Gen_Jobs(<>);
package Gen_Args is

   Parse_Error : exception;

   function File_Name
      return String;

   function Init_Offset
      return Random.Count_Type;

   function Create_Jobs_From_Command_Line
     (Map : Jobs.Map_Type)
      return Jobs.Long_Job_Type;

end Gen_Args;

