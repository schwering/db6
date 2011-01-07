with Ada.Unchecked_Conversion;

with Tree.Jobs;
with Tree.Types; use Tree.Types;

with DB.Maps;

private
generic
   with function Next_Entry return Key_Value_Type;

   Map        : in out DB.Maps.Map_Type'Class;
   Null_value : in out Types.Value_Type'Class;
package Tree.Gen_Simple_Jobs is

   Stop_Now : exception;

   procedure Insert;
   procedure Replace;
   procedure Append;
   procedure Delete;
   procedure Search;
   procedure Antisearch;
   procedure Count;
   procedure Stats;
   procedure Check;

   Job_Map : constant Jobs.Map_Type;

private
   -- keep in sync with Jobs.Short_Job_Type
   type Short_Job_Type is not null access procedure;

   Insert_Access     : Short_Job_Type := Insert'Access;
   Replace_Access    : Short_Job_Type := Replace'Access;
   Append_Access     : Short_Job_Type := Append'Access;
   Delete_Access     : Short_Job_Type := Delete'Access;
   Search_Access     : Short_Job_Type := Search'Access;
   Antisearch_Access : Short_Job_Type := Antisearch'Access;
   Count_Access      : Short_Job_Type := Count'Access;
   Stats_Access      : Short_Job_Type := Stats'Access;
   Check_Access      : Short_Job_Type := Check'Access;

   function Convert is new Ada.Unchecked_Conversion
     (Short_Job_Type, Jobs.Short_Job_Type);

   Job_Map : constant Jobs.Map_Type
           := ((Jobs.To_Description("Insert"),     Convert(Insert_Access)),
               (Jobs.To_Description("Replace"),    Convert(Replace_Access)),
               (Jobs.To_Description("Append"),     Convert(Append_Access)),
               (Jobs.To_Description("Delete"),     Convert(Delete_Access)),
               (Jobs.To_Description("Search"),     Convert(Search_Access)),
               (Jobs.To_Description("Antisearch"), Convert(Antisearch_Access)),
               (Jobs.To_Description("Count"),      Convert(Count_Access)),
               (Jobs.To_Description("Stats"),      Convert(Stats_Access)),
               (Jobs.To_Description("Check"),      Convert(Check_Access)));

end Tree.Gen_Simple_Jobs;

