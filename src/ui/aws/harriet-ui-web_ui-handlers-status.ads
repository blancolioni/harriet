with Harriet.Json;

package Harriet.UI.Web_UI.Handlers.Status is

   type Status_Handler is
     new Routes.Request_Handler with private;

private

   type Status_Handler is
     new Routes.Request_Handler with null record;

   overriding function Handle_Get
     (Handler    : Status_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Handle_Post
     (Handler    : Status_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class;

end Harriet.UI.Web_UI.Handlers.Status;
