with Ada.Strings.Unbounded;

private package Hadrian.Strings is

   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function "=" (Left : Unbounded_String;
                 Right : String)
                 return Boolean
                 renames Ada.Strings.Unbounded."=";

   function "+" (S : String) return Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   Null_String : constant Unbounded_String :=
                   Ada.Strings.Unbounded.Null_Unbounded_String;

end Hadrian.Strings;
