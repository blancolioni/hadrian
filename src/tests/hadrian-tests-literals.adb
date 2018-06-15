package body Hadrian.Tests.Literals is

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out WL.Tests.Test_Suite) is
   begin
      Suite.Append
        (Category => "literals",
         Name     => "match a single literal",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   => Hadrian.Parser.Literal ("dog"),
              Sentence => "dog",
              Image    => "dog"));
      Suite.Append
        (Category => "literals",
         Name     => "match a single string",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   =>
                  Hadrian.Parser.Any_String,
              Sentence => """this is a string""",
              Image    => """this is a string"""));
      Suite.Append
        (Category => "literals",
         Name     => "mismatched literal",
         Test     =>
           New_Parse_Failure_Test
             (Parser   => Hadrian.Parser.Literal ("dog"),
              Sentence => "cat"));
      Suite.Append
        (Category => "literals",
         Name     => "match any text word",
         Test     =>
           New_Simple_Syntax_Test
             (Parser   => Hadrian.Parser.Any_Text_Word,
              Sentence => "cat",
              Image    => "cat"));
   end Add_Tests;

end Hadrian.Tests.Literals;
