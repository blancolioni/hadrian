with Ada.Characters.Handling;

with Hadrian.Tests.Literals;
with Hadrian.Tests.Negations;
with Hadrian.Tests.Sequences;
with Hadrian.Tests.Repeats;

package body Hadrian.Tests is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Test : in out Simple_Syntax_Test)
      return Boolean
   is
      Parse : constant Hadrian.Parser.Parse_Tree :=
                Hadrian.Parser.Parse (Test.Parser, Test.Sentence);
   begin
      if not Hadrian.Parser.Has_Tree (Parse) then
         if Test.Expect_Failure then
            return True;
         else
            Test.Add_Message ("no parse");
            return False;
         end if;
      end if;

      if Test.Expect_Failure then
         Test.Add_Message ("expected parse to fail");
         return False;
      end if;

      declare
         use type Ada.Strings.Unbounded.Unbounded_String;
         Result_Image : constant String :=
                          Hadrian.Parser.Image
                            (Tree       => Parse,
                             Separator  => '-',
                             Show_Names => True);
      begin
         if Test.Result /= Result_Image then
            Test.Add_Message
              ("expected: " & Ada.Strings.Unbounded.To_String (Test.Result));
            Test.Add_Message
              ("found: " & Result_Image);
            return False;
         end if;
      end;

      return True;
   end Execute;

   ----------------------------
   -- New_Parse_Failure_Test --
   ----------------------------

   function New_Parse_Failure_Test
     (Parser         : Hadrian.Parser.Parser_Type;
      Sentence       : String)
      return Simple_Syntax_Test'Class
   is
   begin
      return Test : constant Simple_Syntax_Test :=
        Simple_Syntax_Test'
          (WL.Tests.Root_Test_Type with
             Parser         => Parser,
             Sentence       => To_Words (Sentence),
             Result         => <>,
             Expect_Failure => True);
   end New_Parse_Failure_Test;

   ----------------------------
   -- New_Simple_Syntax_Test --
   ----------------------------

   function New_Simple_Syntax_Test
     (Parser   : Hadrian.Parser.Parser_Type;
      Sentence : String;
      Image    : String)
      return Simple_Syntax_Test'Class
   is
   begin
      return Test : constant Simple_Syntax_Test :=
        Simple_Syntax_Test'
          (WL.Tests.Root_Test_Type with
             Parser         => Parser,
             Sentence       => To_Words (Sentence),
             Result         =>
               Ada.Strings.Unbounded.To_Unbounded_String (Image),
             Expect_Failure => False);
   end New_Simple_Syntax_Test;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Suite : WL.Tests.Test_Suite;
   begin
      Hadrian.Tests.Literals.Add_Tests (Suite);
      Hadrian.Tests.Negations.Add_Tests (Suite);
      Hadrian.Tests.Repeats.Add_Tests (Suite);
      Hadrian.Tests.Sequences.Add_Tests (Suite);
      Suite.Run_Tests;
   end Run_Tests;

   --------------
   -- To_Words --
   --------------

   function To_Words
     (Sentence : String)
      return Hadrian.String_Lists.List
   is
      use Ada.Characters.Handling;
      Result : Hadrian.String_Lists.List;
      Index  : Positive := Sentence'First;
   begin
      while Index <= Sentence'Last loop
         if Is_Alphanumeric (Sentence (Index)) then
            declare
               Last : Positive := Index;
            begin
               while Last <= Sentence'Last
                 and then (Is_Alphanumeric (Sentence (Last))
                           or else Sentence (Last) = '''
                           or else Sentence (Last) = '-'
                           or else Sentence (Last) = '_')
               loop
                  Last := Last + 1;
               end loop;
               Result.Append (Sentence (Index .. Last - 1));
               Index := Last;
            end;
         elsif Sentence (Index) = '"' then
            declare
               Buffer     : String (Sentence'Range);
               To_Index   : Natural := 0;
               From_Index : Positive := Index + 1;
            begin
               while From_Index <= Sentence'Last
                 and then (Sentence (From_Index) /= '"'
                           or else (From_Index < Sentence'Last
                                    and then Sentence (From_Index) = '"'
                                    and then Sentence (From_Index + 1) = '"'))
               loop
                  if Sentence (From_Index) = '"' then
                     From_Index := From_Index + 1;
                  end if;
                  To_Index := To_Index + 1;
                  Buffer (To_Index) := Sentence (From_Index);
                  From_Index := From_Index + 1;
               end loop;
               Result.Append ('"' & Buffer (1 .. To_Index) & '"');
               Index := From_Index + 1;
            end;
         elsif Is_Space (Sentence (Index)) then
            Index := Index + 1;
         else
            Result.Append (Sentence (Index .. Index));
            Index := Index + 1;
         end if;
      end loop;
      return Result;
   end To_Words;

end Hadrian.Tests;
