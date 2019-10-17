with Harriet.Json;
with Harriet.UI.Models.Loader;
with Harriet.Writers;

package body Harriet.Commands.System.Show is

   type Show_Command_Record is
     new Root_Harriet_Command with null record;

   overriding procedure Perform
     (Command   : Show_Command_Record;
      Context   : in out Harriet.Contexts.Context_Type;
      Writer    : in out Harriet.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Show_Command_Record;
      Context   : in out Harriet.Contexts.Context_Type;
      Writer    : in out Harriet.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Context);
      Response : Harriet.Json.Json_Object;
      Model_Name : constant String :=
        Argument (Arguments, 1);
   begin
      if not Harriet.UI.Models.Loader.Exists (Model_Name) then
         Writer.Put_Error
           (Model_Name & ": no such model");
         return;
      end if;

      declare
         View_Name  : constant String :=
           Argument (Arguments, "view",
                     Harriet.UI.Models.Loader.Get (Model_Name)
                     .Default_View_Name);
         Model_Args : constant String :=
           Argument (Arguments, "model-args", "");
      begin
         Response.Set_Property
           ("control", "replace-view");
         Response.Set_Property
           ("view", View_Name);
         Response.Set_Property
           ("model", Model_Name);
         Response.Set_Property
           ("modelArg", Model_Args);
         Writer.Control (Response);
         Writer.Put_Line ("Loading " & View_Name & "/" & Model_Name &
                            " " & Model_Args);
      end;

   end Perform;

   ------------------
   -- Show_Command --
   ------------------

   function Show_Command return Root_Harriet_Command'Class is
   begin
      return Command : Show_Command_Record;
   end Show_Command;

end Harriet.Commands.System.Show;
