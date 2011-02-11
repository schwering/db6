-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.DSA.Gen_Heaps is

   procedure Create
     (Heap : in out Heap_Type;
      ID   : in     String) is
   begin
      null;
   end Create;


   procedure Create_Temporary
     (Heap : in out Heap_Type;
      ID   : in     String) is
   begin
      null;
   end Create_Temporary;


   procedure Open
     (Heap : in out Heap_Type;
      ID   : in     String) is
   begin
      null;
   end Open;


   procedure Finalize
     (Heap : in out Heap_Type) is
   begin
      null;
   end Finalize;


   procedure Read
     (Heap    : in out Heap_Type;
      Address : in     Valid_Address_Type;
      Item    :    out Item_Type;
      State   :    out State_Type) is
   begin
      null;
   end Read;


   procedure Append
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address : in     Valid_Address_Type;
      State   :    out State_Type) is
   begin
      null;
   end Append;


   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Valid_Address_Type;
      State   :    out State_Type) is
   begin
      null;
   end Delete;

end DB.DSA.Gen_Heaps;

