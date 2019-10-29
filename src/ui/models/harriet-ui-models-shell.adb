package body Harriet.UI.Models.Shell is

   type Shell_Model_Type is
     new Root_Harriet_Model with
      record
         null;
      end record;

   overriding function Name
     (Model : Shell_Model_Type)
      return String
   is ("shell");

   overriding function Default_View_Name
     (Model : Shell_Model_Type)
      return String
   is ("Shell");

   overriding function Handle
     (Model   : in out Shell_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Get
     (Model   : Shell_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Changed
     (Model : Shell_Model_Type)
      return Boolean
   is (False);

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : Shell_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Model, State, Client, Request);
   begin
      return Json.Null_Value;
   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Shell_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Model);
      Command : constant String :=
        Harriet.Json.Json_Object (Request).Get_Property ("data");
      Response : constant Harriet.Json.Json_Value'Class :=
          State.Execute_Command (Client, Command);
   begin
      return Response;
   end Handle;

   -----------------
   -- Shell_Model --
   -----------------

   function Shell_Model
      return Root_Harriet_Model'Class
   is
   begin
      return Model : Shell_Model_Type;
   end Shell_Model;

end Harriet.UI.Models.Shell;
