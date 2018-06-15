private package Hadrian.Parser.Repeats is

   procedure Parse_Repeat
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
     with Pre => Parser_Trees.Element (Parser).Node_Type = Repeat;

end Hadrian.Parser.Repeats;
