with Hadrian.Strings;                  use Hadrian.Strings;

package body Hadrian.Parser.Literals is

   type Literal_Parser_Element is
     new Root_Parser_Element with
      record
         Text : Unbounded_String;
      end record;

   overriding procedure Parse
     (Element  : Literal_Parser_Element;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List);

   type Any_Parser_Element is
     new Root_Parser_Element with null record;

   overriding procedure Parse
     (Element  : Any_Parser_Element;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List);

   Any_Parser : aliased Any_Parser_Element;

   ---------
   -- Any --
   ---------

   function Any return Parser_Element is
   begin
      return Any_Parser'Access;
   end Any;

   -------------
   -- Literal --
   -------------

   function Literal (Text : String) return Parser_Element is
   begin
      return new Literal_Parser_Element'
        (Root_Parser_Element with
         Text            => +Text);
   end Literal;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Element  : Literal_Parser_Element;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
   begin
      if not Hadrian.String_Lists.Has_Element (Current) then
         return;
      end if;

      if Element.Text = Hadrian.String_Lists.Element (Current) then
         declare
            Parse : Partial_Parse;
            Name  : constant String := -Element.Name;
            Text  : constant String := -Element.Text;
         begin
            Parse.Consumed.Append (Text);
            Parse.Rest := Hadrian.String_Lists.Next (Current);

            Parse.Tree.Append_Child (Parse.Tree.Root, Text);
            if Name /= "" then
               Reparent (Parse.Tree, Name);
            end if;

            Parses.List.Append (Parse);
         end;
      end if;
   end Parse;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Element  : Any_Parser_Element;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
      Parse : Partial_Parse;
   begin
      if not Hadrian.String_Lists.Has_Element (Current) then
         return;
      end if;

      declare
         Name  : constant String := -Element.Name;
         Text  : constant String := Hadrian.String_Lists.Element (Current);
      begin
         Parse.Consumed.Append (Text);
         Parse.Rest := Hadrian.String_Lists.Next (Current);
         Parse.Tree.Append_Child (Parse.Tree.Root, Text);

         if Name /= "" then
            Reparent (Parse.Tree, Name);
         end if;

      end;

      Parses.List.Append (Parse);
   end Parse;

end Hadrian.Parser.Literals;
