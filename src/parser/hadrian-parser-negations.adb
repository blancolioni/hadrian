package body Hadrian.Parser.Negations is

   -----------
   -- Parse --
   -----------

   procedure Parse_Negation
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
      Check_Parses : Partial_Parse_List;
   begin
      Parse (Parser_Trees.First_Child (Parser), Current, Check_Parses);

      if Check_Parses.List.Is_Empty then
         Parses.List.Append
           (Partial_Parse'
              (Rest     => Current,
               Tree     => <>));
      end if;

   end Parse_Negation;

end Hadrian.Parser.Negations;
