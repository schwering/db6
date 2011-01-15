-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body REST.Input_Formats is

   procedure Parser
     (Resource : in out Stream_Type;
      Request  : in     AWS.Status.Data)
   is
      procedure Parser_Fix
        (Resource : in out Stream_Type'Class;
         Request  : in     AWS.Status.Data)
      is
         Buffer : AWS.Status.Stream_Element_Array (1 .. 128);
         Last   : AWS.Status.Stream_Element_Offset;
      begin
         loop
            AWS.Status.Read_Body (Request, Buffer, Last);
            exit when Last not in Buffer'Range;
            Resource.Write (Buffer, Last);
         end loop;
         Resource.Close;
      exception
         when others =>
            Resource.Close;
            raise;
      end Parser_Fix;

   begin
      Parser_Fix (Resource, Request);
   end Parser;

end REST.Input_Formats;

