private package Hadrian.Parser.Negations is

   procedure Parse_Negation
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
     with Pre => Parser_Trees.Element (Parser).Node_Type = Negate;

end Hadrian.Parser.Negations;
