with Harriet.File_System;

package body Harriet.Commands.System.Cat is

   type Cat_Command_Record is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
     (Command   : Cat_Command_Record;
      Context   : in out Harriet.Contexts.Context_Type;
      Writer    : in out Harriet.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   -----------------
   -- Cat_Command --
   -----------------

   function Cat_Command return Root_Harriet_Command'Class is
   begin
      return Command : Cat_Command_Record;
   end Cat_Command;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Cat_Command_Record;
      Context   : in out Harriet.Contexts.Context_Type;
      Writer    : in out Harriet.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);
   begin
      if Argument_Count (Arguments) = 0 then
         Writer.Put_Error ("Usage: cat file [ files ... ]");
         return;
      end if;

      for I in 1 .. Argument_Count (Arguments) loop
         declare
            Node : constant Harriet.File_System.Node_Id :=
              Context.Find_Node (Argument (Arguments, I));
         begin
            if Node.Is_Empty then
               Writer.Put_Error (Argument (Arguments, I) & ": not found");
               exit;
            end if;

            declare
               Contents : constant String :=
                 Harriet.File_System.Get (Node).Contents;
               Start : Positive := Contents'First;
            begin
               for I in Contents'Range loop
                  if Contents (I) = Character'Val (10) then
                     Writer.Put_Line (Contents (Start .. I));
                     Start := I + 1;
                  end if;
               end loop;
               if Start <= Contents'Last then
                  Writer.Put_Line (Contents (Start .. Contents'Last));
               end if;
            end;
         end;

      end loop;

   end Perform;

end Harriet.Commands.System.Cat;
