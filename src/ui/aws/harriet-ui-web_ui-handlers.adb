with Harriet.UI.Web_UI.Handlers.Clients;
with Harriet.UI.Web_UI.Handlers.Sessions;
with Harriet.UI.Web_UI.Handlers.Login;
with Harriet.UI.Web_UI.Handlers.Status;

package body Harriet.UI.Web_UI.Handlers is

   ---------------------------
   -- Handle_Client_Request --
   ---------------------------

   function Handle_Client_Request
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Clients.Client_Request_Handler;
   end Handle_Client_Request;

   --------------------------------
   -- Handle_Environment_Request --
   --------------------------------

   function Handle_Environment_Request
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Sessions.Environment_Handler;
   end Handle_Environment_Request;

   ------------------
   -- Handle_Login --
   ------------------

   function Handle_Login
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Login.Login_Handler;
   end Handle_Login;

   -----------------------
   -- Handle_New_Client --
   -----------------------

   function Handle_New_Client
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Clients.New_Client_Handler;
   end Handle_New_Client;

   ---------------------------
   -- Handle_Status_Request --
   ---------------------------

   function Handle_Status_Request
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Status.Status_Handler;
   end Handle_Status_Request;

end Harriet.UI.Web_UI.Handlers;
