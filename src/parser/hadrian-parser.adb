with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Hadrian.Parser.Choices;
with Hadrian.Parser.Negations;
with Hadrian.Parser.Optionals;
with Hadrian.Parser.Repeats;
with Hadrian.Parser.Sequences;

package body Hadrian.Parser is

   Trace_Parse : constant Boolean := False;

   package List_Of_Transformers is
     new Ada.Containers.Doubly_Linked_Lists (Transformation_Function);

   type Image_Context is
      record
         Separator    : Character := ' ';
         Show_Names   : Boolean := False;
         Transformers : List_Of_Transformers.List;
      end record;

   function Image_With_Context
     (Top     : Parse_Trees.Cursor;
      Context : Image_Context)
      return String;

   type Parser_Array is
     array (Positive range <>) of Parser_Type;

   Null_Parser_Array : Parser_Array (1 .. 0);

   function Make
     (Node     : Parser_Node_Type;
      Name     : String := "";
      Children : Parser_Array := Null_Parser_Array)
      return Parser_Type;

   function Make
     (Node     : Parser_Node;
      Children : Parser_Array := Null_Parser_Array)
      return Parser_Type;

   package Parse_Tree_Cursor_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Parse_Trees.Cursor,
        "="          => Parse_Trees."=");

   procedure Trace_Node (Position : Parse_Trees.Cursor);

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Parser_Type) return Parser_Type is
   begin
      return Make (Sequence, Children => (Left, Right));
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Literal : String) return Parser_Type is
      Node : constant Parser_Node :=
               Parser_Node'
                 (Node_Type => Terminal,
                  Name      => Hadrian.Strings.Null_String,
                  Literal   =>
                    Ada.Strings.Unbounded.To_Unbounded_String (Literal),
                  others    => <>);
   begin
      return Parse : Parser_Type do
         Parse.Tree.Append_Child (Parse.Tree.Root, Node);
      end return;
   end "+";

   -----------
   -- "not" --
   -----------

   function "not" (Left : Parser_Type) return Parser_Type is
   begin
      return Make (Negate, Children => (1 => Left));
   end "not";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Parser_Type) return Parser_Type is
   begin
      return Make (Choice, Children => (Left, Right));
   end "or";

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out Hadrian.String_Lists.List;
      From : Hadrian.String_Lists.List)
   is
   begin
      for Item of From loop
         To.Append (Item);
      end loop;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (To     : in out Partial_Parse_List;
      Tree   : Parse_Trees.Tree;
      From   : Partial_Parse_List)
   is
   begin

      for Item of From.List loop
         declare
            New_Parse : Partial_Parse := Item;
            New_Tree  : Parse_Trees.Tree := Tree;

            procedure Copy_Child (Child : Parse_Trees.Cursor);

            ----------------
            -- Copy_Child --
            ----------------

            procedure Copy_Child (Child : Parse_Trees.Cursor) is
            begin
               New_Tree.Copy_Subtree
                 (Parent   => Parse_Trees.First_Child (New_Tree.Root),
                  Before   => Parse_Trees.No_Element,
                  Source   => Child);
            end Copy_Child;

         begin

            if New_Tree.Is_Empty then
               New_Tree := Item.Tree;
            else
               Parse_Trees.Iterate_Children
                 (Item.Tree.Root, Copy_Child'Access);
            end if;

            New_Parse.Tree := New_Tree;

            To.List.Append (New_Parse);
         end;
      end loop;
   end Append;

   -----------
   -- Class --
   -----------

   function Class (Parser : Parser_Trees.Cursor) return String is
   begin
      return Ada.Characters.Handling.To_Lower
        (Parser_Node_Type'Image
           (Parser_Trees.Element (Parser).Node_Type));
   end Class;

   -----------------
   -- Get_Subtree --
   -----------------

   function Get_Subtree
     (From : Parse_Subtree;
      Name : String)
      return Parse_Subtree
   is
      use Parse_Trees;
      Queue : Parse_Tree_Cursor_Lists.List;

      procedure Add_Child (Child : Cursor);

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child (Child : Cursor) is
      begin
         Queue.Append (Child);
      end Add_Child;

   begin

      if Cursor (From) = No_Element then
         return Parse_Subtree (Parse_Trees.No_Element);
      end if;

      Parse_Trees.Iterate_Children
        (Cursor (From), Add_Child'Access);

      while not Queue.Is_Empty loop
         declare
            Position  : constant Cursor := Queue.First_Element;
            Parser    : constant Parser_Trees.Cursor :=
                          Element (Position).Syntax;
            Node_Name : constant String :=
                          Ada.Strings.Unbounded.To_String
                            (Parser_Trees.Element (Parser).Name);
         begin
            Queue.Delete_First;

            if Node_Name = Name then
               return Parse_Subtree (Position);
            end if;

            Parse_Trees.Iterate_Children (Position, Add_Child'Access);
         end;
      end loop;

      return Parse_Subtree (No_Element);
   end Get_Subtree;

   --------------
   -- Has_Tree --
   --------------

   function Has_Tree (Tree : Parse_Tree) return Boolean is
   begin
      return not Tree.Tree.Is_Empty;
   end Has_Tree;

   -----------
   -- Image --
   -----------

   function Image
     (Top        : Parse_Trees.Cursor;
      Separator  : Character := ' ';
      Show_Names : Boolean := False)
      return String
   is
   begin
      return Image_With_Context
        (Top,
         Context => Image_Context'
           (Separator    => Separator,
            Show_Names   => Show_Names,
            Transformers => <>));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Tree       : Parse_Subtree;
                   Separator  : Character := ' ';
                   Show_Names : Boolean := False)
                   return String
   is
   begin
      return Image (Parse_Trees.Cursor (Tree), Separator, Show_Names);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Tree       : Parse_Tree;
                   Separator  : Character := ' ';
                   Show_Names : Boolean := False)
                   return String
   is
   begin
      if Parse_Trees.Is_Empty (Tree.Tree) then
         return "no parse";
      else
         return Image (Tree.Tree.Root, Separator, Show_Names);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Parser : Parser_Type) return String is

      function Image (Parent : Parser_Trees.Cursor) return String;
      function Image_With_Name (Parent : Parser_Trees.Cursor) return String;

      -----------
      -- Image --
      -----------

      function Image (Parent : Parser_Trees.Cursor) return String is
         use Ada.Strings.Unbounded;
         Node : constant Parser_Node := Parser_Trees.Element (Parent);
      begin
         if Node.Node_Type = Terminal then
            if Node.Literal /= "" then
               return "'" & To_String (Node.Literal) & "'";
            else
               return "(terminal)";
            end if;
         else
            declare
               Children : Unbounded_String;
            begin
               for Position in Parser.Tree.Iterate_Children (Parent) loop
                  Children := Children
                    & (if Children = "" then ""
                       elsif Node.Node_Type = Choice then " | "
                       else " ")
                    & Image_With_Name (Position);
               end loop;

               case Non_Terminal_Node_Type (Node.Node_Type) is
                  when Negate =>
                     return "~(" & To_String (Children) & ")";
                  when Sequence =>
                     return To_String (Children);
                  when Optional =>
                     return "[" & To_String (Children) & "]";
                  when Repeat =>
                     return "{" & To_String (Children) & "}";
                  when Choice =>
                     return To_String (Children);
               end case;
            end;
         end if;
      end Image;

      ---------------------
      -- Image_With_Name --
      ---------------------

      function Image_With_Name (Parent : Parser_Trees.Cursor) return String is
         use Ada.Strings.Unbounded;
         Name : constant String :=
                  To_String (Parser_Trees.Element (Parent).Name);
         Child : constant String := Image (Parent);
      begin
         if Name = "" then
            return Child;
         else
            return Name & "<" & Child & ">";
         end if;
      end Image_With_Name;

   begin
      return Image_With_Name (Parser_Trees.First_Child (Parser.Tree.Root));
   end Image;

   ------------------------
   -- Image_With_Context --
   ------------------------

   function Image_With_Context
     (Top     : Parse_Trees.Cursor;
      Context : Image_Context)
      return String
   is

      use Hadrian.Strings;

      New_Context : Image_Context := Context;

      function Apply_Transforms (S : Unbounded_String) return String;

      function Children_Image (Child : Parse_Trees.Cursor) return String;

      procedure Update_Context;

      ----------------------
      -- Apply_Transforms --
      ----------------------

      function Apply_Transforms (S : Unbounded_String) return String is
         It : Unbounded_String := S;
      begin
         for T of Context.Transformers loop
            It := +(T (-It));
         end loop;
         return -It;
      end Apply_Transforms;

      --------------------
      -- Children_Image --
      --------------------

      function Children_Image (Child : Parse_Trees.Cursor) return String is
      begin
         if not Parse_Trees.Has_Element (Child) then
            return "";
         else
            declare
               Rest  : constant String :=
                         Children_Image (Parse_Trees.Next_Sibling (Child));
               Space : constant String :=
                         (if Parse_Trees.Has_Element
                            (Parse_Trees.Next_Sibling (Child))
                          then (1 => New_Context.Separator) else "");
               This  : constant String :=
                         Image_With_Context (Child, New_Context);
            begin
               return This & Space & Rest;
            end;
         end if;
      end Children_Image;

      --------------------
      -- Update_Context --
      --------------------

      procedure Update_Context is
         Node : constant Parse_Tree_Node := Parse_Trees.Element (Top);
         Syntax : constant Parser_Node :=
                    Parser_Trees.Element (Node.Syntax);
      begin
         if Syntax.Transform /= null then
            New_Context.Transformers.Append (Syntax.Transform);
         end if;
      end Update_Context;

   begin

      if Parse_Trees.Is_Root (Top) then
         return Children_Image (Parse_Trees.First_Child (Top));
      elsif not Parse_Trees.Has_Element (Top) then
         return "<>";
      elsif Parse_Trees.Is_Leaf (Top) then
         Update_Context;
         return Apply_Transforms (Parse_Trees.Element (Top).Value);
      else
         Update_Context;
         declare
            Children : constant String :=
                         Children_Image (Parse_Trees.First_Child (Top));
            Syntax   : constant Parser_Trees.Cursor :=
                         Parse_Trees.Element (Top).Syntax;
            Text     : constant String :=
                         -Parser_Trees.Element (Syntax).Name;
         begin
            if Text = "" or else not Context.Show_Names then
               return Children;
            else
               return Text & "<" & Children & ">";
            end if;
         end;
      end if;
   end Image_With_Context;

   ----------
   -- Make --
   ----------

   function Make
     (Node     : Parser_Node_Type;
      Name     : String := "";
      Children : Parser_Array := Null_Parser_Array)
      return Parser_Type
   is
      use Hadrian.Strings;
      Tree_Node : constant Parser_Node :=
                    Parser_Node'
                      (Node_Type => Node,
                       Name      => +Name,
                       others    => <>);
   begin
      return Make (Tree_Node, Children);
   end Make;

   ----------
   -- Make --
   ----------

   function Make
     (Node     : Parser_Node;
      Children : Parser_Array := Null_Parser_Array)
      return Parser_Type
   is
   begin
      return Parser : Parser_Type do
         Parser.Tree.Append_Child
           (Parser.Tree.Root, Node);
         for Child of Children loop
            Parser.Tree.Copy_Subtree
              (Parser_Trees.First_Child (Parser.Tree.Root),
               Parser_Trees.No_Element,
               Parser_Trees.First_Child (Child.Tree.Root));
         end loop;
      end return;
   end Make;

   --------------------
   -- Match_Terminal --
   --------------------

   function Match_Terminal
     (Match : Terminal_Match_Function)
      return Parser_Type
   is
      Node : constant Parser_Node :=
               Parser_Node'
                 (Node_Type => Terminal,
                  Name      => Hadrian.Strings.Null_String,
                  Literal   => Hadrian.Strings.Null_String,
                  Match     => Match,
                  others    => <>);
   begin
      return Make (Node);
   end Match_Terminal;

   ------------------
   -- Non_Terminal --
   ------------------

   function Non_Terminal
     (Node_Type : Non_Terminal_Node_Type;
      Name      : String := "")
      return Parser_Node
   is
   begin
      return Parser_Node'
        (Node_Type => Node_Type,
         Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         others    => <>);
   end Non_Terminal;

   --------------
   -- Optional --
   --------------

   function Optional (Parser : Parser_Type) return Parser_Type is
   begin
      return Make (Optional, Children => (1 => Parser));
   end Optional;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List)
   is
      use Hadrian.Strings;
      use Parser_Trees;
      Node : constant Parser_Node := Element (Parser);
   begin
      if not Hadrian.String_Lists.Has_Element (Current) then
         return;
      end if;

      case Node.Node_Type is
         when Terminal =>
            declare
               Term_Text : constant String :=
                             Hadrian.String_Lists.Element (Current);
            begin
               if Node.Match = null then
                  if Ada.Strings.Fixed.Equal_Case_Insensitive
                    (-Node.Literal, Term_Text)
                  then
                     Parses.List.Append
                       (Partial_Parse'
                          (Rest     => Hadrian.String_Lists.Next (Current),
                           Tree     =>
                             To_Parse_Tree
                               (Parser => Parser,
                                Value  => Term_Text)));
                  end if;
               else
                  if Node.Match (Term_Text) then
                     Parses.List.Append
                       (Partial_Parse'
                          (Rest     => Hadrian.String_Lists.Next (Current),
                           Tree     =>
                             To_Parse_Tree
                               (Parser => Parser,
                                Value  => Term_Text)));
                  end if;
               end if;
            end;
         when Choice =>
            Hadrian.Parser.Choices.Parse_Choice
              (Parser  => Parser,
               Current => Current,
               Parses  => Parses);

         when Negate =>
            Hadrian.Parser.Negations.Parse_Negation
              (Parser  => Parser,
               Current => Current,
               Parses  => Parses);

         when Optional =>
            Hadrian.Parser.Optionals.Parse_Optional
              (Parser  => Parser,
               Current => Current,
               Parses  => Parses);

         when Repeat =>
            Hadrian.Parser.Repeats.Parse_Repeat
              (Parser  => Parser,
               Current => Current,
               Parses  => Parses);

         when Sequence =>
            Hadrian.Parser.Sequences.Parse_Sequence
              (Parser  => Parser,
               Current => Current,
               Parses  => Parses);

      end case;

   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse (Parser         : Parser_Type;
                   Sentence       : Hadrian.String_Lists.List;
                   Rank_Heuristic : Parse_Rank_Heuristic := None)
                   return Parse_Tree
   is
      use type Ada.Containers.Count_Type;
      Partial : Partial_Parse_List;
      Highest_Node_Count : Ada.Containers.Count_Type := 0;
   begin

      Parse (Parser_Trees.First_Child (Parser.Tree.Root),
             Sentence.First, Partial);

      declare
         Result : Parse_Tree;
         Found  : Boolean := False;
      begin
         for P of Partial.List loop
            if not Hadrian.String_Lists.Has_Element (P.Rest) then
               if Found then
                  case Rank_Heuristic is
                     when None =>
                        Result := (Tree => Parse_Trees.Empty_Tree);
                     when First =>
                        null;
                     when Node_Count =>
                        if P.Tree.Node_Count > Highest_Node_Count then
                           Result := (Tree => P.Tree);
                           Highest_Node_Count := P.Tree.Node_Count;
                        end if;
                  end case;
               else
                  Result := (Tree => P.Tree);
                  Found := True;
               end if;
            end if;
         end loop;

         if Trace_Parse then
            if not Result.Tree.Is_Empty then
               Trace (Result.Tree);
            end if;
         end if;
         return Result;
      end;
   end Parse;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (To   : in out Hadrian.String_Lists.List;
      From : Hadrian.String_Lists.List)
   is
   begin
      for Item of reverse From loop
         To.Insert (To.First, Item);
      end loop;
   end Prepend;

--     --------------
--     -- Reparent --
--     --------------
--
--     procedure Reparent
--       (Tree : in out Parse_Trees.Tree;
--        Name : String)
--     is
--        use Hadrian.Strings;
--
--        Old_Tree : constant Parse_Trees.Tree := Tree;
--
--        procedure Append (Child : Parse_Trees.Cursor);
--
--        ------------
--        -- Append --
--        ------------
--
--        procedure Append (Child : Parse_Trees.Cursor) is
--        begin
--           Tree.Copy_Subtree
--             (Parent => Parse_Trees.First_Child (Tree.Root),
--              Before => Parse_Trees.No_Element,
--              Source => Child);
--        end Append;
--
--     begin
--        Tree.Clear;
--        Tree.Append_Child
--          (Parent   => Tree.Root,
--           New_Item =>
--             Parse_Tree_Node'
--               (Syntax => Parser_Trees.No_Element,
--                Name   => +Name,
--                Value  => Null_String));
--        Parse_Trees.Iterate_Children (Old_Tree.Root, Append'Access);
--     end Reparent;

   ------------
   -- Repeat --
   ------------

   function Repeat (Parser : Parser_Type) return Parser_Type is
   begin
      return Make (Repeat, Children => (1 => Parser));
   end Repeat;

   procedure Scan_Leaves
     (Start   : Parse_Subtree;
      Process : not null access
        procedure (Leaf : Parse_Subtree))
   is
      use Parse_Trees;

      Queue : Parse_Tree_Cursor_Lists.List;

      procedure Add_Child (Child : Cursor);

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child (Child : Cursor) is
      begin
         Queue.Append (Child);
      end Add_Child;

   begin

      if Cursor (Start) = No_Element then
         return;
      end if;

      Parse_Trees.Iterate_Children
        (Cursor (Start), Add_Child'Access);

      while not Queue.Is_Empty loop
         declare
            Position  : constant Cursor := Queue.First_Element;
         begin
            Queue.Delete_First;

            if Is_Leaf (Position) then
               Process (Parse_Subtree (Position));
            else
               Parse_Trees.Iterate_Children (Position, Add_Child'Access);
            end if;
         end;
      end loop;
   end Scan_Leaves;

   -------------------------
   -- Scan_Named_Subtrees --
   -------------------------

   procedure Scan_Named_Subtrees
     (Start   : Parse_Subtree;
      Process : not null access
        procedure (Subtree      : Parse_Subtree))
   is
      use Parse_Trees;

      procedure Scan_Tree (Tree : Cursor);

      ---------------
      -- Scan_Tree --
      ---------------

      procedure Scan_Tree (Tree : Cursor) is
         Parser    : constant Parser_Trees.Cursor :=
                       Element (Tree).Syntax;
         Node_Name : constant String :=
                       Ada.Strings.Unbounded.To_String
                         (Parser_Trees.Element (Parser).Name);
      begin
         if Node_Name /= "" then
            Process (Parse_Subtree (Tree));
         else
            Parse_Trees.Iterate_Children (Tree, Scan_Tree'Access);
         end if;
      end Scan_Tree;

   begin

      if Cursor (Start) = No_Element then
         return;
      end if;

      Parse_Trees.Iterate_Children
        (Cursor (Start), Scan_Tree'Access);
   end Scan_Named_Subtrees;

   -------------------
   -- To_Identifier --
   -------------------

   function To_Identifier
     (List : Hadrian.String_Lists.List)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Item of List loop
         if Result = "" then
            Result := Result & Item;
         else
            Result := Result & "-" & Item;
         end if;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end To_Identifier;

   -------------------
   -- To_Parse_Tree --
   -------------------

   function To_Parse_Tree
     (Parser : Parser_Trees.Cursor;
      Value  : String)
      return Parse_Trees.Tree
   is
      use Hadrian.Strings;
   begin

      return Tree : Parse_Trees.Tree do
         Tree.Append_Child
           (Tree.Root,
            Parse_Tree_Node'
              (Syntax => Parser,
               Value  => +Value));
      end return;
   end To_Parse_Tree;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Tree : Parse_Trees.Tree)
   is
   begin
      if Tree.Is_Empty then
         Ada.Text_IO.Put_Line ("<>");
      else
         Trace_Node (Tree.Root);
      end if;
   end Trace;

   ----------------
   -- Trace_Node --
   ----------------

   procedure Trace_Node (Position : Parse_Trees.Cursor) is
      use Ada.Text_IO;
      use Hadrian.Strings;
      Indent : constant Count := Col;
   begin
      if Parse_Trees.Is_Root (Position) then
         null;
      else
         declare
            Node   : constant Parse_Tree_Node :=
                       Parse_Trees.Element (Position);
            Name   : constant String :=
                       Non_Terminal_Name (Node.Syntax);
         begin
            if Parse_Trees.Is_Leaf (Position) then
               Put_Line
                 ("<" & (-Node.Value) & ">");
            else
               Put ("+");
               Put (Class (Node.Syntax));
               if Name /= "" then
                  Put ("/" & Name);
               end if;
               Put (":");
               Put ("[" & (-Node.Value) & "]");
            end if;
         end;
      end if;

      if not Parse_Trees.Is_Leaf (Position) then
         Put_Line (" {");

         declare
            procedure Trace_Child (Child : Parse_Trees.Cursor);

            -----------------
            -- Trace_Child --
            -----------------

            procedure Trace_Child (Child : Parse_Trees.Cursor) is
            begin
               Set_Col (Indent);
               Put ("  ");
               Trace_Node (Child);
            end Trace_Child;

         begin
            Parse_Trees.Iterate_Children (Position, Trace_Child'Access);
         end;
         Set_Col (Indent);
         Put_Line ("}");
      end if;
   end Trace_Node;

   ---------------
   -- With_Name --
   ---------------

   function With_Name (Name   : String;
                       Parser : Parser_Type)
                       return Parser_Type
   is
      New_Tree : Parser_Trees.Tree;
   begin
      New_Tree.Append_Child
        (New_Tree.Root, Non_Terminal (Sequence, Name));

      New_Tree.Copy_Subtree
        (Parser_Trees.First_Child (New_Tree.Root),
         Parser_Trees.No_Element,
         Parser_Trees.First_Child (Parser.Tree.Root));
      return (Tree => New_Tree);
   end With_Name;

   ----------------------
   -- With_Transformer --
   ----------------------

   function With_Transformer
     (Transformer : Transformation_Function;
      Parser      : Parser_Type)
      return Parser_Type
   is
      New_Tree : Parser_Trees.Tree;
      New_Node : Parser_Node := Non_Terminal (Sequence);
   begin
      New_Node.Transform := Transformer;
      New_Tree.Append_Child (New_Tree.Root, New_Node);

      New_Tree.Copy_Subtree
        (Parser_Trees.First_Child (New_Tree.Root),
         Parser_Trees.No_Element,
         Parser_Trees.First_Child (Parser.Tree.Root));
      return (Tree => New_Tree);
   end With_Transformer;

   ---------------------
   -- With_Validation --
   ---------------------

   function With_Validation
     (Validator : Validation_Function;
      Parser    : Parser_Type)
      return Parser_Type
   is
      New_Tree : Parser_Trees.Tree;
      New_Node : Parser_Node := Non_Terminal (Sequence);
   begin
      New_Node.Validate := Validator;
      New_Tree.Append_Child (New_Tree.Root, New_Node);

      New_Tree.Copy_Subtree
        (Parser_Trees.First_Child (New_Tree.Root),
         Parser_Trees.No_Element,
         Parser_Trees.First_Child (Parser.Tree.Root));
      return (Tree => New_Tree);
   end With_Validation;

end Hadrian.Parser;
