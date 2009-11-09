with Ada.Unchecked_Conversion;

with Jobs;

with DB.Types.Keys;
with DB.Types.Values;

generic
   type Object_Type (<>) is limited private;
   type Count_Type is range <>;
   type Result_Type is (<>);
   Object : in out Object_Type;
   Success : in Result_Type;
   Failure : in Result_Type;
   with procedure P_Insert
          (Object   : in out Object_Type; 
           Key      : in     DB.Types.Keys.Key_Type;
           Value    : in     DB.Types.Values.Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
   with procedure P_Delete
          (Object   : in out Object_Type; 
           Key      : in     DB.Types.Keys.Key_Type;
           Value    :    out DB.Types.Values.Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
   with procedure P_Look_Up
          (Object   : in out Object_Type; 
           Key      : in     DB.Types.Keys.Key_Type;
           Value    :    out DB.Types.Values.Value_Type;
           Position :    out Count_Type;
           State    :    out Result_Type);
package Gen_Simple_Jobs is

   Stop_Now : exception;

   procedure Insert;
   procedure Delete;
   procedure Search;
   procedure Antisearch;

   Job_Map : constant Jobs.Map_Type;

private
   -- keep in sync with Jobs.Short_Job_Type
   type Short_Job_Type is not null access procedure;

   Insert_Access     : Short_Job_Type := Insert'Access;
   Delete_Access     : Short_Job_Type := Delete'Access;
   Search_Access     : Short_Job_Type := Search'Access;
   Antisearch_Access : Short_Job_Type := Antisearch'Access;

   function Convert is new Ada.Unchecked_Conversion
     (Short_Job_Type, Jobs.Short_Job_Type);

   Job_Map : constant Jobs.Map_Type
           := ((Jobs.To_Description("Insert"),     Convert(Insert_Access)),
               (Jobs.To_Description("Delete"),     Convert(Delete_Access)),
               (Jobs.To_Description("Search"),     Convert(Search_Access)),
               (Jobs.To_Description("Antisearch"), Convert(Antisearch_Access)));

end Gen_Simple_Jobs;

