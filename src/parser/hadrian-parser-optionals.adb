package body Hadrian.Parser.Optionals is

   -----------
   -- Parse --
   -----------

   procedure Parse_Optional
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
   begin
      Parse (Parser_Trees.First_Child (Parser), Current, Parses);
      Parses.List.Append (Partial_Parse'
                            (Rest     => Current,
                             Tree     => <>));
   end Parse_Optional;

end Hadrian.Parser.Optionals;
