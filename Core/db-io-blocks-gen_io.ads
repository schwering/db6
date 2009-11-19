-- Abstract:
--
-- IO signature package.
--
-- Copyright 2008, 2009 Christoph Schwering

generic
   type File_Type is limited private;
   -- File handle.

   type Ticket_Type is private;
   -- Ticket type for locks.

   type Address_Type is private;
   -- General addresses.

   type Valid_Address_Type is private;
   -- Valid addresses.

   Needs_Explicit_Block_Count : in Boolean;
   -- Indicates that after a file is Opened, Set_Block_Count must be called
   -- before any calls to Seek_New.

   Invalid_Address : in Address_Type;
   -- The invalid address usable at many places similar to a null-pointer.

   with procedure Create
          (ID   : in  String;
           File : out File_Type);
   -- Creates a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode.

   with procedure Open
          (ID   : in  String;
           File : out File_Type);
   -- Opens a File. If this fails, an IO_Error is raised. The File is opened
   -- in read/write-mode in general (this might not be needed if the implementor
   -- is sure that nobody ever calls Write of his Gen_IO implementation).

   with procedure Set_Block_Count
          (File    : in out File_Type;
           Address : in     Address_Type);
   -- While the name of the procedure says that it informs the File object
   -- about the count of blocks in file, This is somewhat ambiguous:
   -- In fact, Address is the last (i.e. greatest with regard to "<")
   -- address in File that contains data.
   -- This procedure generally has no effect if Needs_Explicit_Block_Count
   -- is False.

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

   with procedure Seek_New
          (File    : in out File_Type;
           Address :    out Valid_Address_Type);
   -- Sets Address to a new position in File at which further Write calls
   -- will not fail and at which further Read calls will do fail before the
   -- next Write. No further assumptions can be done (in particular, no
   -- assumptions with regard to the ordering and successor-relation).
   -- If this fails for whatever reason, an IO_Error is raised.

   with procedure Acquire_Ticket
          (File   : in out File_Type;
           Ticket :    out Ticket_Type);
   -- Acquires a Ticket that can hold locks on File. Until the Release_Ticket
   -- call, Ticket is valid can hold therefore locks. Each acquired ticket
   -- *must* be released.

   with procedure Release_Ticket
          (File   : in out File_Type;
           Ticket : in     Ticket_Type);
   -- Releases a Ticket that can hold locks on File.
   -- The Ticket must be unlocked, i.e. hold no locks, otherwise a Lock_Error
   -- is raised.

   with procedure Read_Lock
          (File   : in out File_Type;
           Ticket : in     Ticket_Type);
   -- After the return of this procedure, Ticket holds exactly a read-lock.
   -- If it held a write- or certify-lock before, the lock is downgraded.
   -- Multiple further read-locks and/or at most one write-locks can be hold
   -- by different Tickets at the same time.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   with procedure Write_Lock
          (File   : in out File_Type;
           Ticket : in     Ticket_Type);
   -- After the return of this procedure, Ticket holds exactly a write-lock.
   -- If it held a read-lock before, the lock is upgraded, if it held a
   -- certify-lock before, the lock is downgraded.
   -- Such a lock prohibits further write-locks or even certify-locks at the
   -- same time hold by other tickets.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   with procedure Certify_Lock
          (File   : in out File_Type;
           Ticket : in     Ticket_Type);
   -- After the return of this procedure, Ticket holds exactly a certify-lock.
   -- If it held a read- or write-lock before, the lock is upgraded.
   -- Such a lock prohibits *any* further locks at the same time hold by other
   -- tickets.
   -- This procedure possibly blocks.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.

   with procedure Unlock
          (File   : in out File_Type;
           Ticket : in     Ticket_Type);
   -- Releases the lock hold by Ticket. When this procedure returns, Ticket
   -- holds no more locks.
   -- If the Ticket does not hold any lock, the procedure has no effect.
   -- The Ticket must be valid, i.e. it must be acquired but not yet released,
   -- otherwise a Lock_Error is raised.
package DB.IO.Blocks.Gen_IO is
   pragma Pure;
end DB.IO.Blocks.Gen_IO;

