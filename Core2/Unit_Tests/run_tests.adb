-- Abstract:
--
-- Runs the main test suite.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Run;
with AUnit.Reporter.Text;

with Composite_Suite;

procedure Run_Tests is
   procedure Run is new AUnit.Run.Test_Runner (Composite_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Run_Tests;

