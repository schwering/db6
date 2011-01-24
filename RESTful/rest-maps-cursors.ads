-- Abstract:
--
-- Tasks that run over cursors.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AWS.URL;

with DB.DSA.Utils.Gen_Queues;

package REST.Maps.Cursors is

   type Cursor_Type is
      record
         URL    : AWS.URL.Object;
         Offset : Natural := 0;
      end record;

   type Cursor_Ref_Type is access Cursor_Type;

   function Bound
     (Row       : String;
      Inclusive : Boolean;
      Lower     : Boolean)
      return DB.Maps.Bound_Type;

end REST.Maps.Cursors;

