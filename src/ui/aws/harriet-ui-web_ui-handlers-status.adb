package body Harriet.UI.Web_UI.Handlers.Status is

   ----------------
   -- Handle_Get --
   ----------------

   overriding function Handle_Get
     (Handler    : Status_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
   begin
      return State.Status_Value (Parameters.Parameter ("setting"));
   end Handle_Get;

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : Status_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
      Setting : constant String := Parameters.Parameter ("setting");
      Value   : constant String := Parameters.Parameter ("value");
   begin
      State.Set_Status_Value (Setting, Json.String_Value (Value));

      return Result : Json.Json_Object do
         Result.Set_Property (Setting, Value);
      end return;

   end Handle_Post;

end Harriet.UI.Web_UI.Handlers.Status;
