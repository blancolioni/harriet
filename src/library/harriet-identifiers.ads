package Harriet.Identifiers is

   subtype Object_Identifier is String (1 .. 8);

   function Next_Identifier return Object_Identifier;

end Harriet.Identifiers;
