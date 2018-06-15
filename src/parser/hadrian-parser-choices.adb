package body Hadrian.Parser.Choices is

   -----------
   -- Parse --
   -----------

   procedure Parse_Choice
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is

      procedure Try_Choice (Choice : Parser_Trees.Cursor);

      ----------------
      -- Try_Choice --
      ----------------

      procedure Try_Choice (Choice : Parser_Trees.Cursor) is
         This_Parse : Partial_Parse_List;
      begin
         Parse (Choice, Current, This_Parse);

         for Item of This_Parse.List loop
            Parses.List.Append (Item);
         end loop;

      end Try_Choice;

   begin
      Parser_Trees.Iterate_Children (Parser, Try_Choice'Access);
   end Parse_Choice;

end Hadrian.Parser.Choices;
