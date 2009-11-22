with IO_Dispatcher.Jobs;
with IO_Dispatcher.Random;

private
package IO_Dispatcher.Args is

   Parse_Error : exception;

   function File_Name
      return String;

   function Init_Offset
      return Random.Count_Type;

   function Create_Jobs_From_Command_Line
     (Map : Jobs.Map_Type)
      return Jobs.Long_Job_Type;

   function Pop_Argument (I : Positive) return String;
   procedure Undo_Pop;

end IO_Dispatcher.Args;

