with IO_Dispatcher.Jobs;
with IO_Dispatcher.Test_Data;

private
package IO_Dispatcher.Args is

   Parse_Error : exception;

   function File_Name
      return String;

   function Generator
      return Test_Data.Generator_Type;

   function Init_Offset
      return Test_Data.Count_Type;

   function Create_Jobs_From_Command_Line
     (Map : Jobs.Map_Type)
      return Jobs.Long_Job_Type;

   function Pop_Argument (I : Positive) return String;
   procedure Undo_Pop;

end IO_Dispatcher.Args;

