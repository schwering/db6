with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Finalization; use Ada.Finalization;

procedure Trans
is
   type Process_Type is task interface;
   type Superviser_Type is task interface;
   type Superviser_Class_Access_Type is access Superviser_Type'Class;

   Processes : array (1 .. 10) of Superviser_Class_Access_Type;
   Index     : Positive := Processes'First;

   generic
      type Process_Impl_Type is new Process_Type with private;
   procedure Start_And_Supervise;
   procedure Start_And_Supervise
   is
      task type Superviser_Impl_Type is new Superviser_Type with
      end Superviser_Impl_Type;
      task body Superviser_Impl_Type
      is begin
         <<Restart>>
         declare
            Transaction : Process_Impl_Type;
         begin
            null;
         exception
            when Program_Error =>
               goto Restart; 
         end;
      end Superviser_Impl_Type;
      type Superviser_Impl_Access_Type is access Superviser_Impl_Type;
      for Superviser_Impl_Access_Type'Storage_Pool use
         Superviser_Class_Access_Type'Storage_Pool;

      S : constant Superviser_Impl_Access_Type := new Superviser_Impl_Type;
      --S : constant Superviser_Class_Access_Type := new Superviser_Impl_Type;
   begin
      null;
      Put_Line("Task "& Image(Current_Task));
      --Processes(Index) := Superviser_Class_Access_Type'(S);
      --Index            := Index + 1;
   end Start_And_Supervise;


   protected type Prot_Lock_Type is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := False;
   end Prot_Lock_Type;

   protected body Prot_Lock_Type is
      entry Lock when not Locked is begin Locked := True; end;
      procedure Unlock is begin Locked := False; end;
   end Prot_Lock_Type;

   type Lock_Type is new Limited_Controlled with
      record
         Prot_Lock : Prot_Lock_Type;
      end record;
   overriding procedure Finalize (L : in out Lock_Type);
   overriding procedure Finalize (L : in out Lock_Type)
   is begin L.Prot_Lock.Unlock; end;


   task type Process_Impl_Type is new Process_Type with
   end Process_Impl_Type;
   task body Process_Impl_Type
   is begin
      loop null; end loop;--Put_Line("Task "& Image(Current_Task)); end loop;
   end Process_Impl_Type;

   procedure Start is new Start_And_Supervise(Process_Impl_Type);
begin
   Put_Line("Lulu");
   Start;
   Put_Line("LuluLulu");
end Trans;

