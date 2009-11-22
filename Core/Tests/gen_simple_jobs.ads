with Ada.Unchecked_Conversion;

with Jobs;

generic
   type Object_Type (<>) is limited private;
   type Key_Type is private;
   type Value_Type (<>) is private;

   type Key_Value_Type is private;
   with function Random_Entry return Key_Value_Type;
   with function Get_Key (KV : Key_Value_Type) return Key_Type;
   with function Get_Value (KV : Key_Value_Type) return Value_Type;

   with function Key_To_String (K : Key_Type) return String;
   with function Value_To_String (V : Value_Type) return String;

   with function "=" (Left, Right : Value_Type) return Boolean;

   with procedure Check_Key_Value (KV : Key_Value_Type);

   type Count_Type is range <>;
   type Result_Type is (<>);

   Object     : in out Object_Type;
   Null_value : in Value_Type;
   Success    : in Result_Type;
   Failure    : in Result_Type;

   with procedure P_Insert
          (Object   : in out Object_Type; 
           Key      : in     Key_Type;
           Value    : in     Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
   with procedure P_Delete
          (Object   : in out Object_Type; 
           Key      : in     Key_Type;
           Value    :    out Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
   with procedure P_Look_Up
          (Object   : in out Object_Type; 
           Key      : in     Key_Type;
           Value    :    out Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
   with procedure P_Count
          (Object : in out Object_Type;
           Count  :    out Count_Type);
   with procedure P_Make_Stats
          (Object                 : in out Object_Type;
           Height                 :    out Natural;
           Blocks                 :    out Natural;
           Free_Blocks            :    out Natural;
           Max_Degree             :    out Natural;
           Avg_Degree             :    out Natural;
           Min_Degree             :    out Natural;
           Bytes_Wasted_In_Blocks :    out Long_Integer;
           Bytes_In_Blocks        :    out Long_Integer);
   with procedure P_Check
          (Object : in out Object_Type);
package Gen_Simple_Jobs is

   Stop_Now : exception;

   procedure Insert;
   procedure Delete;
   procedure Search;
   procedure Antisearch;
   procedure Make_Stats;
   procedure Check;

   Job_Map : constant Jobs.Map_Type;

private
   -- keep in sync with Jobs.Short_Job_Type
   type Short_Job_Type is not null access procedure;

   Insert_Access     : Short_Job_Type := Insert'Access;
   Delete_Access     : Short_Job_Type := Delete'Access;
   Search_Access     : Short_Job_Type := Search'Access;
   Antisearch_Access : Short_Job_Type := Antisearch'Access;
   Make_Stats_Access : Short_Job_Type := Make_Stats'Access;
   Check_Access      : Short_Job_Type := Check'Access;

   function Convert is new Ada.Unchecked_Conversion
     (Short_Job_Type, Jobs.Short_Job_Type);

   Job_Map : constant Jobs.Map_Type
           := ((Jobs.To_Description("Insert"),     Convert(Insert_Access)),
               (Jobs.To_Description("Delete"),     Convert(Delete_Access)),
               (Jobs.To_Description("Search"),     Convert(Search_Access)),
               (Jobs.To_Description("Antisearch"), Convert(Antisearch_Access)),
               (Jobs.To_Description("Make_Stats"), Convert(Make_Stats_Access)),
               (Jobs.To_Description("Check"),      Convert(Check_Access)));

end Gen_Simple_Jobs;

