package body Harriet.Identifiers is

   Template : constant Object_Identifier := "0AA00AA0";

   protected Identifier_Source is

      procedure Next_Identifier (Id : out Object_Identifier);

   private
      Current : Object_Identifier := Template;
   end Identifier_Source;

   protected body Identifier_Source is

      procedure Next_Identifier (Id : out Object_Identifier) is

         function Inc (Ch : in out Character) return Boolean
           with Pre => Ch in 'A' .. 'Z' | '0' .. '9';

         ---------
         -- Inc --
         ---------

         function Inc (Ch : in out Character) return Boolean is
         begin
            if Ch = 'Z' then
               Ch := 'A';
               return True;
            elsif Ch = '9' then
               Ch := '0';
               return True;
            else
               Ch := Character'Succ (Ch);
               return False;
            end if;
         end Inc;

      begin
         Id := Current;
         for Ch of reverse Current loop
            if not Inc (Ch) then
               return;
            end if;
         end loop;
         raise Program_Error with "out of identifiers";
      end Next_Identifier;
   end Identifier_Source;

   ---------------------
   -- Next_Identifier --
   ---------------------

   function Next_Identifier return Object_Identifier is
   begin
      return Id : Object_Identifier do
         Identifier_Source.Next_Identifier (Id);
      end return;
   end Next_Identifier;

end Harriet.Identifiers;
