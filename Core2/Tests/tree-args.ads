with DB.Maps;

with Tree.Jobs;
with Tree.Types;

private
package Tree.Args is

   Parse_Error : exception;

   function File_Name
      return String;

   function Implementation
      return DB.Maps.Implementation_Type;

   function Generator
      return Types.Generator_Type;

   function Init_Offset
      return Types.Count_Type;

   function Create_Jobs_From_Command_Line
     (Map : Jobs.Map_Type)
      return Jobs.Long_Job_Type;

   function Pop_Argument (I : Positive) return String;
   procedure Undo_Pop;

end Tree.Args;

