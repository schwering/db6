-- Abstract:
--
-- IO signature package.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic

   pragma Warnings (Off); -- Disable `unreferenced' warnings

   type File_Type is limited private;
   -- File handle.

   type Address_Type is private;
   -- General addresses.

   type Valid_Address_Type is private;
   -- Valid addresses.

   Invalid_Address : in Address_Type;
   -- The invalid address usable at many places similar to a null-pointer.

   with procedure Create
     (ID   : in  String;
      File : out File_Type);
   -- Creates a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode.

   with procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type);
   -- Creates a temporary File. The exact semantic is that the file cannot be
   -- opened ever again, after the return of Create_And_Open_Temporary next
   -- calls with the same ID should succeed and File becomes invalid after the
   -- next Close.
   -- If this procedure fails, an IO_Error is raised. The File is opened in
   -- read/write-mode.
   -- This operation might be unsupported in which case IO_Error must be raised.

   with procedure Open
     (ID   : in  String;
      File : out File_Type);
   -- Opens a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode in general (this might not be needed if the implementor
   -- is sure that nobody ever calls Write of his Gen_IO implementation).

   with procedure Close
     (File : in out File_Type);
   -- Properly closes a File. If this fails, an IO_Error is raised.

   with function First
      return Valid_Address_Type;
   -- Returns the first valid address of all files.

   with function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type;
   -- Returns the successor of Address. In a simple case, this might be
   -- Address + 1.

   with function "<"
     (A, B : Valid_Address_Type)
      return Boolean;
   -- Strict less operation for valid addresses. X < Succ(X) must always hold.

   with function "="
     (A, B : Valid_Address_Type)
      return Boolean;
   -- Equality relation for valid addresses.

   with function Image
     (A : in Valid_Address_Type)
      return String;
   -- Converts a valid address into a string. Just for debugging purposes.

   with function To_Address
     (Address : Valid_Address_Type)
      return Address_Type;
   -- Casts or converts Address to an address. Is allowed for all valid
   -- addresses.

   with function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type;
   -- Casts or converts Address to a valid address. If Address is invalid,
   -- the behaviour is undefined, should be checked with Is_Valid_Address
   -- in advance.

   with function Is_Valid_Address
     (Address : Address_Type)
      return Boolean;
   -- Checks whether Address is valid or not.

   with procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type);
   -- Reads a Block from File at position Address. There must exist some
   -- data on Address, otherwise an IO_Error is raised.

   with procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type);
   -- Reads a Block from File at position Address. There must exist some
   -- data on Address, otherwise an IO_Error is raised.

   with procedure Write_New_Block
     (File    : in out File_Type;
      Address :    out Valid_Address_Type;
      Block   : in     Block_Type);
   -- Writes Block to some new, previously unused Address.
   -- If this fails for whatever reason, an IO_Error is raised.

   with procedure Lock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);
   -- Locks the block at the given Address. Blocks until the lock is available.
   -- Otherwise raises IO_Error.
   -- TODO XXX In fact, this may *never* raise an error, does it? We have to
   -- think about this very carefully and re-design gen_btrees-insert.adb
   -- respectively.

   with procedure Unlock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);
   -- Unlocks the lock held on the block at Address.
   -- Otherwise raises IO_Error.
   -- TODO XXX In fact, this may *never* raise an error, does it? We have to
   -- think about this very carefully and re-design gen_btrees-insert.adb
   -- respectively.

   pragma Warnings (On);

package DB.Blocks.Gen_IO_Signature is
   pragma Pure;
end DB.Blocks.Gen_IO_Signature;

