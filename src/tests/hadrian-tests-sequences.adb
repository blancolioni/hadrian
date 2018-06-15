package body Hadrian.Tests.Sequences is

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out WL.Tests.Test_Suite) is
      use Hadrian.Parser;
   begin
      Suite.Append
        (Category => "sequences",
         Name     => "match two literals",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   => Hadrian.Parser.Literal ("dog") & "cat",
              Sentence => "dog cat",
              Image    => "dog-cat"));
      Suite.Append
        (Category => "sequences",
         Name     => "match three literals",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   => Hadrian.Parser.Literal ("dog") & "cat" & "horse",
              Sentence => "dog cat horse",
              Image    => "dog-cat-horse"));
   end Add_Tests;

end Hadrian.Tests.Sequences;
