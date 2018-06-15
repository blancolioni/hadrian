package body Hadrian.Parser.Sequences is

   -----------
   -- Parse --
   -----------

   procedure Parse_Sequence
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is

      package Partial_Parse_List_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Partial_Parse_List);

      Acc : Partial_Parse_List;

      procedure Parse_Child (Child : Parser_Trees.Cursor);

      -----------------
      -- Parse_Child --
      -----------------

      procedure Parse_Child (Child : Parser_Trees.Cursor) is
         New_Parse : Partial_Parse_List;
      begin
         for Partial of Acc.List loop
            declare
               This_Parse : Partial_Parse_List;
            begin
               Parse (Child, Partial.Rest, This_Parse);
               if not This_Parse.List.Is_Empty then
                  Append (New_Parse, Partial.Tree, This_Parse);
               end if;
            end;
         end loop;
         Acc := New_Parse;
      end Parse_Child;

      Seq_Tree : Parse_Trees.Tree;

   begin

      Seq_Tree.Append_Child
        (Seq_Tree.Root,
         Parse_Tree_Node'
           (Syntax => Parser,
            Value  => <>));

      Acc.List.Append (Partial_Parse'
                         (Rest     => Current,
                          Tree     => Seq_Tree));

      Parser_Trees.Iterate_Children
        (Parser, Parse_Child'Access);

      Parses := Acc;
   end Parse_Sequence;

end Hadrian.Parser.Sequences;
