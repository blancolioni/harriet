private with Harriet.UI.Views;

private package Harriet.Commands.Views is

   procedure Load_View_Commands;

private

   type Load_View_Command is
     abstract new Root_Harriet_Command with null record;

   function Create_View
     (Command   : Load_View_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Arguments : Argument_List)
      return Harriet.UI.Views.View_Type
      is abstract;

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Harriet.Sessions.Harriet_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

end Harriet.Commands.Views;
