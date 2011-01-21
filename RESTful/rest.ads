-- Abstract:
--
-- Root package of the REST interface.
--
-- Copyright 2010--2011 Christoph Schwering

package REST is
   pragma Pure;

   URL_Path_Error             : exception;
   Stream_Error               : exception;
   Malformed_Input_Data_Error : exception;
   Insertion_Error            : exception;

end REST;

