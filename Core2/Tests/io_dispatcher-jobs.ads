with IO_Dispatcher.Random;

private
package IO_Dispatcher.Jobs is

   type Job_Type is private;
   type Long_Job_Type is array (Positive range <>) of Job_Type;

   type Short_Job_Type is access procedure;
   type Description_Type is private;

   type Entry_Type is
      record
         Description : Description_Type;
         Short_Job   : Short_Job_Type;
      end record;
   type Map_Type is array (Positive range <>) of Entry_Type;

   function To_Description (S : String) return Description_Type;
   function To_String (D : Description_Type) return String;
   function "=" (D, E : Description_Type) return Boolean;

   function Add
     (Job_Map     : Map_Type;
      Description : String;
      Short_Job   : Short_Job_Type)
      return Map_Type;

   function New_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Reset                     : in Boolean  := True)
      return Job_Type;

   function New_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Short_Job_Execution_Count : in Random.Count_Type;
      Concurrency_Degree        : in Positive;
      Reset                     : in Boolean)
      return Job_Type;

   procedure Execute_Jobs
     (Jobs : in Long_Job_Type);

   procedure Execute_Job
     (Job : in Job_Type);

   procedure Execute_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Short_Job_Execution_Count : in Random.Count_Type;
      Concurrency_Degree        : in Positive;
      Reset                     : in Boolean);

private
   type Description_Type is
      record
         Len : Natural;
         Str : String(1 .. 128);
      end record;

   type Job_Type is
      record
         Description               : Description_Type;
         Short_Job                 : Short_Job_Type;
         Short_Job_Execution_Count : Random.Count_Type;
         Concurrency_Degree        : Positive;
         Reset                     : Boolean;
      end record;

end IO_Dispatcher.Jobs;

