private with Ada.Strings.Unbounded;

private with WL.Tests;

private with Hadrian.String_Lists;
private with Hadrian.Parser;

package Hadrian.Tests is

   procedure Run_Tests;

private

   type Simple_Syntax_Test is
     new WL.Tests.Root_Test_Type with
      record
         Parser         : Hadrian.Parser.Parser_Type;
         Sentence       : Hadrian.String_Lists.List;
         Result         : Ada.Strings.Unbounded.Unbounded_String;
         Expect_Failure : Boolean;
      end record;

   overriding function Execute
     (Test : in out Simple_Syntax_Test)
      return Boolean;

   function New_Simple_Syntax_Test
     (Parser         : Hadrian.Parser.Parser_Type;
      Sentence       : String;
      Image          : String)
      return Simple_Syntax_Test'Class;

   function New_Parse_Failure_Test
     (Parser         : Hadrian.Parser.Parser_Type;
      Sentence       : String)
      return Simple_Syntax_Test'Class;

   function To_Words
     (Sentence : String)
      return Hadrian.String_Lists.List;

end Hadrian.Tests;
