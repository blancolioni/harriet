package Harriet.UI.Web_UI.Handlers.Login is

   type Login_Handler is
     new Routes.Request_Handler with private;

private

   type Login_Handler is
     new Routes.Request_Handler with null record;

   overriding function Creates_State
     (Handler : Login_Handler)
      return Boolean
   is (True);

   overriding function Handle_Create
     (Handler    : Login_Handler;
      Parameters : Routes.Parameter_Container'Class)
      return State_Interface'Class;

end Harriet.UI.Web_UI.Handlers.Login;
