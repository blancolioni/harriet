package body Harriet.UI.Web_UI.Handlers.Clients is

   ----------------
   -- Handle_Get --
   ----------------

   overriding function Handle_Get
     (Handler    : Client_Request_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
   begin
      return State.Handle_Client_Get
        (Client  => Client_Id'Value (Parameters.Parameter ("client")),
         Request => Parameters.To_Json);
   end Handle_Get;

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : New_Client_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
      Model_Name     : constant String :=
        Parameters.Parameter ("model");
      Model_Argument : constant String :=
        Parameters.Parameter ("modelArg");
      New_Client_Id  : constant Client_Id :=
        State.New_Client (Model_Name, Model_Argument);
   begin
      return Response : Harriet.Json.Json_Object do
         if New_Client_Id = 0 then
            if Model_Name = "" then
               Response.Set_Property
                 ("error", "missing required argument: model");
            else
               Response.Set_Property
                 ("error", "no such model: " & Model_Name);
            end if;
         else
            Response.Set_Property ("clientId", Natural (New_Client_Id));
         end if;
      end return;
   end Handle_Post;

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : Client_Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
   begin
      return State.Handle_Client_Post
        (Client  => Client_Id'Value (Parameters.Parameter ("client")),
         Request => Parameters.To_Json);
   end Handle_Post;

end Harriet.UI.Web_UI.Handlers.Clients;
