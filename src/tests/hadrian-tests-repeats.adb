package body Hadrian.Tests.Repeats is

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out WL.Tests.Test_Suite) is
      use Hadrian.Parser;
   begin
      Suite.Append
        (Category => "repeats",
         Name     => "detect and avoid avoid infinite loops",
         Test     =>
           New_Parse_Failure_Test
             (Parser   => Repeat (not (Literal ("cat"))),
              Sentence => "bird mouse horse dog"));
   end Add_Tests;

end Hadrian.Tests.Repeats;
