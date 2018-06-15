with Ada.Containers.Doubly_Linked_Lists;

private with Ada.Characters.Handling;
private with Ada.Containers.Multiway_Trees;
private with WL.String_Maps;
private with Hadrian.Strings;

with Hadrian.String_Lists;

package Hadrian.Parser is

   type Parser_Type is private;

   function "or" (Left, Right : Parser_Type) return Parser_Type;
   function "not" (Left : Parser_Type) return Parser_Type;
   function "&" (Left, Right : Parser_Type) return Parser_Type;
   function "+" (Literal : String) return Parser_Type;

   function "&" (Left : Parser_Type; Right : String) return Parser_Type;

   function Optional (Parser : Parser_Type) return Parser_Type;
   function Repeat (Parser : Parser_Type) return Parser_Type;
   function Literal (Text : String) return Parser_Type
                     renames "+";

   function Any_Text_Word return Parser_Type;
--     function Any_Upper_Case_Word return Parser_Type;
--     function Any_Lower_Case_Word return Parser_Type;
   function Any_String return Parser_Type;

   type Terminal_Match_Function is access
     function (Text : String) return Boolean;

   type Character_Match_Function is access
     function (Ch : Character) return Boolean;

   function Match_Terminal
     (Match : Terminal_Match_Function)
      return Parser_Type;

   function With_Name (Name   : String;
                       Parser : Parser_Type)
                       return Parser_Type;

   function Image (Parser : Parser_Type) return String;

   type Parse_Tree is private;

   type Parse_Subtree is private;

   function Root (Tree : Parse_Tree) return Parse_Subtree;
   function Name (Tree : Parse_Subtree) return String;

   function Has_Tree (Tree : Parse_Tree) return Boolean;
   function Image (Tree       : Parse_Tree;
                   Separator  : Character := ' ';
                   Show_Names : Boolean := False)
                   return String;

   function Image (Tree       : Parse_Subtree;
                   Separator  : Character := ' ';
                   Show_Names : Boolean := False)
                   return String;

   type Parse_Rank_Heuristic is
     (None, First, Node_Count);

   function Parse (Parser         : Parser_Type;
                   Sentence       : Hadrian.String_Lists.List;
                   Rank_Heuristic : Parse_Rank_Heuristic := None)
                   return Parse_Tree;

   type Validation_Function is access
     function (Parse : Parse_Subtree) return Boolean;

   function With_Validation
     (Validator : Validation_Function;
      Parser    : Parser_Type)
      return Parser_Type;

   type Transformation_Function is access
     function (Leaf_Text : String) return String;

   function With_Transformer
     (Transformer : Transformation_Function;
      Parser      : Parser_Type)
      return Parser_Type;

   function Is_Empty
     (Tree : Parse_Subtree)
      return Boolean;

   function Get_Subtree
     (From : Parse_Subtree;
      Name : String)
      return Parse_Subtree;

   function Get_Subtree
     (From_Tree : Parse_Tree;
      Name      : String)
      return Parse_Subtree
   is (Get_Subtree (Root (From_Tree), Name));

   procedure Scan_Named_Subtrees
     (Start : Parse_Subtree;
      Process : not null access
        procedure (Subtree      : Parse_Subtree));

   procedure Scan_Leaves
     (Start   : Parse_Subtree;
      Process : not null access
        procedure (Leaf : Parse_Subtree));

   function To_Identifier
     (List : Hadrian.String_Lists.List)
      return String;

private

   type Parser_Node_Type is
     (Terminal,
      Negate, Sequence, Optional, Repeat, Choice);

   subtype Non_Terminal_Node_Type is Parser_Node_Type range Negate .. Choice;

   type Parser_Node is
      record
         Node_Type : Parser_Node_Type;
         Name      : Hadrian.Strings.Unbounded_String;
         Literal   : Hadrian.Strings.Unbounded_String;
         Match     : Terminal_Match_Function;
         Validate  : Validation_Function;
         Transform : Transformation_Function;
      end record;

   function Non_Terminal
     (Node_Type : Non_Terminal_Node_Type;
      Name      : String := "")
      return Parser_Node;

   package Parser_Trees is
     new Ada.Containers.Multiway_Trees (Parser_Node);

   function Class (Parser : Parser_Trees.Cursor) return String;
   function Non_Terminal_Name
     (Parser : Parser_Trees.Cursor)
      return String;

   type Parser_Type is
      record
         Tree : Parser_Trees.Tree;
      end record;

   procedure Append
     (To   : in out Hadrian.String_Lists.List;
      From : Hadrian.String_Lists.List);

   procedure Prepend
     (To   : in out Hadrian.String_Lists.List;
      From : Hadrian.String_Lists.List);

   package Sentence_Fragment_Maps is
     new WL.String_Maps (Hadrian.String_Lists.List,
                         Hadrian.String_Lists."=");

   type Parse_Tree_Node is
      record
         Syntax : Parser_Trees.Cursor;
         Value  : Hadrian.Strings.Unbounded_String;
      end record;

   package Parse_Trees is
     new Ada.Containers.Multiway_Trees (Parse_Tree_Node);

   procedure Trace
     (Tree : Parse_Trees.Tree);

   function Image (Top        : Parse_Trees.Cursor;
      Separator  : Character := ' ';
      Show_Names : Boolean := False)
                   return String;

   function Non_Terminal_Name
     (Parser : Parser_Trees.Cursor)
      return String
   is (Hadrian.Strings."-" (Parser_Trees.Element (Parser).Name));

   type Parse_Tree is
      record
         Tree     : Parse_Trees.Tree;
      end record;

   type Parse_Subtree is new Parse_Trees.Cursor;

   function Root (Tree : Parse_Tree) return Parse_Subtree
   is (Parse_Subtree (Tree.Tree.Root));

   function Name (Tree : Parse_Subtree) return String
   is (Hadrian.Strings."-"
         (Parser_Trees.Element
          (Parse_Trees.Element (Parse_Trees.Cursor (Tree)).Syntax)
          .Name));

   type Partial_Parse is
      record
         Rest     : Hadrian.String_Lists.Cursor;
         Tree     : Parse_Trees.Tree;
      end record;

   package Partial_Parse_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Partial_Parse);

   type Partial_Parse_List is
      record
         List : Partial_Parse_Lists.List;
      end record;

   procedure Append
     (To     : in out Partial_Parse_List;
      Tree   : Parse_Trees.Tree;
      From   : Partial_Parse_List);

   procedure Parse
     (Parser   : Parser_Trees.Cursor;
      Current  : Hadrian.String_Lists.Cursor;
      Parses   : out Partial_Parse_List);

   function To_Parse_Tree
     (Parser : Parser_Trees.Cursor;
      Value  : String)
      return Parse_Trees.Tree;

   function Is_Alphanumeric_Text
     (S : String)
      return Boolean
   is (for all Ch of S => Ada.Characters.Handling.Is_Alphanumeric (Ch));

   function Is_String_Word
     (S : String)
      return Boolean
   is (S'Length >= 2 and then S (S'First) = '"' and then S (S'Last) = '"');

   function Any_Text_Word return Parser_Type
   is (Match_Terminal (Is_Alphanumeric_Text'Access));

   function Any_String return Parser_Type
   is (Match_Terminal (Is_String_Word'Access));

   function Is_Empty
     (Tree : Parse_Subtree)
      return Boolean
   is (not Parse_Trees.Has_Element (Parse_Trees.Cursor (Tree)));

   --     function Any_Upper_Case_Word return Parser_Type
--     is (Match_Terminal (Ada.Characters.Handling.Is_Upper'Access));
--
--     function Any_Lower_Case_Word return Parser_Type
--     is (Match_Terminal (Ada.Characters.Handling.Is_Lower'Access));
--
   function "&" (Left : Parser_Type; Right : String) return Parser_Type
   is (Left & Literal (Right));

end Hadrian.Parser;
