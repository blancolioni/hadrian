package body Hadrian.Parser.Repeats is

   -----------
   -- Parse --
   -----------

   procedure Parse_Repeat
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
      Acc      : Partial_Parse_List;
      Tree     : Parse_Trees.Tree;

   begin

      Tree.Append_Child
        (Tree.Root,
         Parse_Tree_Node'
           (Syntax => Parser,
            Value  => <>));

      Acc.List.Append (Partial_Parse'
                         (Rest     => Current,
                          Tree     => Tree));

      loop
         declare
            use type Hadrian.String_Lists.Cursor;
            New_Parse : Partial_Parse_List;
         begin
            for Partial of Acc.List loop
               declare
                  Try_Parses, Non_Empty_Parses : Partial_Parse_List;
               begin
                  if Hadrian.String_Lists.Has_Element (Partial.Rest) then
                     Parse
                       (Parser_Trees.First_Child (Parser), Partial.Rest,
                        Try_Parses);

                     for P of Try_Parses.List loop
                        if P.Rest /= Partial.Rest then
                           Non_Empty_Parses.List.Append (P);
                        end if;
                     end loop;

                     if not Non_Empty_Parses.List.Is_Empty then
                        Append (New_Parse, Partial.Tree, Non_Empty_Parses);
                     end if;
                  end if;
               end;
            end loop;

            exit when New_Parse.List.Is_Empty;

            Acc := New_Parse;
            for Partial of New_Parse.List loop
               Parses.List.Append (Partial);
            end loop;
         end;
      end loop;

   end Parse_Repeat;

end Hadrian.Parser.Repeats;
