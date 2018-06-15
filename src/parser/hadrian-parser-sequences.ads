private package Hadrian.Parser.Sequences is

   procedure Parse_Sequence
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
     with Pre => Parser_Trees.Element (Parser).Node_Type = Sequence;

end Hadrian.Parser.Sequences;
