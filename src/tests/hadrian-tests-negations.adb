package body Hadrian.Tests.Negations is

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out WL.Tests.Test_Suite) is
      use Hadrian.Parser;
   begin
      Suite.Append
        (Category => "negations",
         Name     => "not a cat, but a dog",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   => not (Literal ("dog")) & "cat",
              Sentence => "cat",
              Image    => "cat"));
      Suite.Append
        (Category => "negations",
         Name     => "any number of non-cats, followed by a dog",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   =>
                  Repeat (not (Literal ("cat")) & Any_Text_Word) & "dog",
              Sentence => "bird mouse horse dog",
              Image    => "bird-mouse-horse-dog"));
      Suite.Append
        (Category => "negations",
         Name     => "not a number of things",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   =>
                  Repeat
                    (not (Literal ("a") or Literal ("is") or Literal ("and"))
                  & Any_Text_Word),
              Sentence => "bird mouse horse dog",
              Image    => "bird-mouse-horse-dog"));
   end Add_Tests;

end Hadrian.Tests.Negations;
