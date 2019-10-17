with Harriet.Json;

package Harriet.UI.Web_UI.Handlers.Sessions is

   type Environment_Handler is
     new Routes.Request_Handler with private;

private

   type Environment_Handler is
     new Routes.Request_Handler with null record;

   overriding function Handle_Get
     (Handler    : Environment_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class;

end Harriet.UI.Web_UI.Handlers.Sessions;
