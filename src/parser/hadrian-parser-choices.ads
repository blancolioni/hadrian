private package Hadrian.Parser.Choices is

   procedure Parse_Choice
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
     with Pre => Parser_Trees.Element (Parser).Node_Type = Choice;

end Hadrian.Parser.Choices;
