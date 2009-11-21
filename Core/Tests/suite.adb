with Algos.Graphs.Dense.DAG_Tests;
with Algos.Graphs.Dijkstra.Tests;
with Algos.Graphs.Ford_Fulkerson.Tests;
with Algos.Graphs.Dinic.Tests;
with Algos.Graphs.Heaps.Tests;

package body Suite is

   Test_1 : aliased Algos.Graphs.Dense.DAG_Tests.Test_Case;
   Test_2 : aliased Algos.Graphs.Dijkstra.Tests.Test_Case;
   Test_3 : aliased Algos.Graphs.Ford_Fulkerson.Tests.Test_Case;
   Test_4 : aliased Algos.Graphs.Dinic.Tests.Test_Case;
   Test_5 : aliased Algos.Graphs.Heaps.Tests.Test_Case;

   Result : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Result.Add_Test(Test_1'Access);
      Result.Add_Test(Test_2'Access);
      Result.Add_Test(Test_3'Access);
      Result.Add_Test(Test_4'Access);
      Result.Add_Test(Test_5'Access);
      return Result'Access;
   end Suite;

end Suite;

