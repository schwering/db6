with AUnit.Run;
with AUnit.Reporter.Text;

with Suite;

procedure Run_Tests
is
   procedure Run is new AUnit.Run.Test_Runner(Suite.Suite);
   Reporter : Aunit.Reporter.Text.Text_Reporter;
begin
   Run(Reporter);
end Run_Tests;

