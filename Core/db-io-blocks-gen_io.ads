generic
   type P_File_Type is limited private;
   type P_Ticket_Type is private;
   type P_Address_Type is private;
   type P_Valid_Address_Type is private;
   P_Needs_Explicit_Block_Count : in Boolean;
   P_Invalid_Address : in P_Address_Type;
   with procedure P_Create
          (ID   : in  String;
           File : out P_File_Type);
   with procedure P_Open
          (ID   : in  String;
           File : out P_File_Type);
   with procedure P_Set_Block_Count
          (File    : in out P_File_Type;
           Address : in     P_Address_Type);
   with procedure P_Close
          (File : in out P_File_Type);
   with function P_First
           return P_Valid_Address_Type;
   with function P_Succ
          (Address : P_Valid_Address_Type)
           return P_Valid_Address_Type;
   with function P_Is_Less
          (A, B : P_Valid_Address_Type)
           return Boolean;
   with function P_Is_Equal
          (A, B : P_Valid_Address_Type)
           return Boolean;
   with function P_Image
          (A : in P_Valid_Address_Type)
           return String;
   with function P_To_Address
          (Address : P_Valid_Address_Type)
           return P_Address_Type;
   with function P_To_Valid_Address
          (Address : P_Address_Type)
           return P_Valid_Address_Type;
   with function P_Is_Valid_Address
          (Address : P_Address_Type)
           return Boolean;
   with procedure P_Read
          (File    : in out P_File_Type;
           Address : in     P_Valid_Address_Type;
           Block   :    out Block_Type);
   with procedure P_Write
          (File    : in out P_File_Type;
           Address : in     P_Valid_Address_Type;
           Block   : in     Block_Type);
   with procedure P_Seek_New
          (File    : in out P_File_Type;
           Address :    out P_Valid_Address_Type);
   with procedure P_Acquire_Ticket
          (File   : in out P_File_Type;
           Ticket :    out P_Ticket_Type);
   with procedure P_Release_Ticket
          (File   : in out P_File_Type;
           Ticket : in     P_Ticket_Type);
   with procedure P_Read_Lock
          (File   : in out P_File_Type;
           Ticket : in     P_Ticket_Type);
   with procedure P_Write_Lock
          (File   : in out P_File_Type;
           Ticket : in     P_Ticket_Type);
   with procedure P_Certify_Lock
          (File   : in out P_File_Type;
           Ticket : in     P_Ticket_Type);
   with procedure P_Unlock
          (File   : in out P_File_Type;
           Ticket : in     P_Ticket_Type);
package DB.IO.Blocks.Gen_IO is
   pragma Pure;

   subtype File_Type is P_File_Type;
   subtype Ticket_Type is P_Ticket_Type;
   subtype Address_Type is P_Address_Type;
   subtype Valid_Address_Type is P_Valid_Address_Type;

   Needs_Explicit_Block_Count : constant Boolean
                              := P_Needs_Explicit_Block_Count;
   -- Indicates that after a file is Opened, Set_Block_Count must be called
   -- before any calls to Seek_New.

   Invalid_Address : constant Address_Type := P_Invalid_Address;
   -- The invalid address usable at many places similar to a null-pointer.

   procedure Create
     (ID   : in  String;
      File : out File_Type)
   renames P_Create;
   -- Creates a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode.

   procedure Open
     (ID   : in  String;
      File : out File_Type)
   renames P_Open;
   -- Opens a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode in general (this might not be needed if the implementor
   -- is sure that nobody ever calls Write of his Gen_IO implementation).

   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type)
   renames P_Set_Block_Count;
   -- While the name of the procedure says that it informs the File object
   -- about the count of blocks in file, This is somewhat ambiguous: 
   -- In fact, Address is the last (i.e. greatest with regard to "<") 
   -- address in File that contains data.
   -- This procedure generally has no effect if Needs_Explicit_Block_Count 
   -- is False.

   procedure Close
     (File : in out File_Type)
   renames P_Close;
   -- Properly closes a File. If this fails, an IO_Error is raised.

   function First
      return Valid_Address_Type
   renames P_First;
   -- Returns the first valid address of all files.

   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type
   renames P_Succ;
   -- Returns the successor of Address. In a simple case, this might be
   -- Address + 1.

   function "<"
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_Is_Less;
   -- Strict less operation for valid addresses. X < Succ(X) must always hold.

   function "="
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_Is_Equal;
   -- Equality relation for valid addresses.

   function Image
     (A : in Valid_Address_Type)
      return String
   renames P_Image;
   -- Converts a valid address into a string. Just for debugging purposes.

   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type
   renames P_To_Address;
   -- Casts or converts Address to an address. Is allowed for all valid
   -- addresses.

   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type
   renames P_To_Valid_Address;
   -- Casts or converts Address to a valid address. If Address is invalid,
   -- the behaviour is undefined, should be checked with Is_Valid_Address
   -- in advance.

   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean
   renames P_Is_Valid_Address;
   -- Checks whether Address is valid or not.

   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   renames P_Read;
   -- Reads a Block from File at position Address. There must exist some
   -- data on Address, otherwise an IO_Error is raised.

   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type)
   renames P_Write;
   -- Reads a Block from File at position Address. There must exist some
   -- data on Address, otherwise an IO_Error is raised.

   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type)
   renames P_Seek_New;
   -- Sets Address to a new position in File at which further Write calls
   -- will not fail and at which further Read calls will do fail before the
   -- next Write. No further assumptions can be done (in particular, no 
   -- assumptions with regard to the ordering and successor-relation).
   -- If this fails for whatever reason, an IO_Error is raised.

   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Ticket_Type)
   renames P_Acquire_Ticket;
   -- Acquires a Ticket that can hold locks on File. Until the Release_Ticket
   -- call, Ticket is valid can hold therefore locks. Each acquired ticket
   -- *must* be released.

   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Ticket_Type)
   renames P_Release_Ticket;
   -- Releases a Ticket that can hold locks on File.
   -- The Ticket must be unlocked, i.e. hold no locks, otherwise a Lock_Error
   -- is raised.

   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type)
   renames P_Read_Lock;
   -- After the return of this procedure, Ticket holds exactly a read-lock.
   -- If it held a write- or certify-lock before, the lock is downgraded.
   -- Multiple further read-locks and/or at most one write-locks can be hold
   -- by different Tickets at the same time.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type)
   renames P_Write_Lock;
   -- After the return of this procedure, Ticket holds exactly a write-lock.
   -- If it held a read-lock before, the lock is upgraded, if it held a
   -- certify-lock before, the lock is downgraded.
   -- Such a lock prohibits further write-locks or even certify-locks at the
   -- same time hold by other tickets.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type)
   renames P_Certify_Lock;
   -- After the return of this procedure, Ticket holds exactly a certify-lock.
   -- If it held a read- or write-lock before, the lock is upgraded.
   -- Such a lock prohibits *any* further locks at the same time hold by other
   -- tickets.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type)
   renames P_Unlock;
   -- Releases the lock hold by Ticket. When this procedure returns, Ticket
   -- holds no more locks.
   -- If the Ticket does not hold any lock, the procedure has no effect.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

end DB.IO.Blocks.Gen_IO;

