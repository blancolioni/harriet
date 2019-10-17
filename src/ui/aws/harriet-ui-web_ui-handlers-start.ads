package Harriet.UI.Web_UI.Handlers.Start is

   type Start_Handler is
     new Routes.Request_Handler with private;

private

   type Start_Handler is
     new Routes.Request_Handler with null record;

   overriding function Creates_State
     (Handler : Start_Handler)
      return Boolean
   is (True);

   overriding function Handle_Create
     (Handler    : Start_Handler;
      Parameters : Routes.Parameter_Container'Class)
      return State_Interface'Class;

end Harriet.UI.Web_UI.Handlers.Start;
