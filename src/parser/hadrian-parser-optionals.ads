private package Hadrian.Parser.Optionals is

   procedure Parse_Optional
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
     with Pre => Parser_Trees.Element (Parser).Node_Type = Optional;

end Hadrian.Parser.Optionals;
