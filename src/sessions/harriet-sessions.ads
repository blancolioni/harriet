private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Ordered_Maps;
private with WL.String_Maps;

private with Harriet.Json;
private with Harriet.Writers;

with Ada.Finalization;

with Harriet.Contexts;
with Harriet.Signals;
with Harriet.UI;
with Harriet.UI.Models;

private with Harriet.Db;

package Harriet.Sessions is

   function New_Session
     (User_Name : String;
      Password  : String)
     return Harriet.UI.State_Interface'Class;

   function New_Administrator_Session
      return Harriet.UI.State_Interface'Class;

   type Root_Harriet_Session is
     new Ada.Finalization.Controlled
     and Harriet.UI.State_Interface with private;

   subtype Harriet_Session is Root_Harriet_Session'Class;

   function Default_Context
     (Session : Root_Harriet_Session'Class)
      return Harriet.Contexts.Context_Type;

private

   type Client_Type is
      record
         Model   : Harriet.UI.Models.Harriet_Model;
         Context : Harriet.Contexts.Context_Type;
      end record;

   package Client_Maps is
     new Ada.Containers.Ordered_Maps
       (Harriet.UI.Client_Id, Client_Type, Harriet.UI."<");

   package Environment_Maps is
     new WL.String_Maps (Harriet.Json.Json_Value'Class, Harriet.Json."=");

   type Context_Updater is access
     procedure (Context : in out Harriet.Contexts.Context_Type);

   package Connection_Holders is
     new Ada.Containers.Indefinite_Holders
       (Harriet.UI.Connection_Interface'Class,
        Harriet.UI."=");

   protected type Session_Data is

      procedure Create_Client
        (User           : Harriet.Db.User_Reference;
         Context        : Harriet.Contexts.Context_Type;
         Model_Name     : String;
         Model_Argument : String;
         Client_Id      : out Harriet.UI.Client_Id);

      procedure Close_Client
        (Client_Id      : Harriet.UI.Client_Id);

      procedure Scan_Clients
        (Process : not null access
           procedure
             (Client : Harriet.UI.Client_Id;
              Model  : in out Harriet.UI.Models.Root_Harriet_Model'Class));

      procedure Execute_Command
        (Client_Id : Harriet.UI.Client_Id;
         Writer    : in out Harriet.Writers.Writer_Interface'Class;
         Command   : String);

      function Get_Model
        (Client_Id : Harriet.UI.Client_Id)
         return Harriet.UI.Models.Harriet_Model;

      procedure Set_Model
        (Client_Id : Harriet.UI.Client_Id;
         Model     : Harriet.UI.Models.Harriet_Model);

      procedure Set_Environment_Value
        (Name : String;
         Value : Json.Json_Value'Class);

      function Get_Environment_Value
        (Name : String)
         return Json.Json_Value'Class;

      procedure Reference;
      procedure Unreference (Finished : out Boolean);

   private

      References      : Natural := 1;
      Last_Client     : Harriet.UI.Client_Id := 0;
      Client_Map      : Client_Maps.Map;
      Environment     : Environment_Maps.Map;
   end Session_Data;

   type Session_Data_Access is access Session_Data;

   type Root_Harriet_Session is
     new Ada.Finalization.Controlled
     and Harriet.UI.State_Interface with
      record
         User             : Harriet.Db.User_Reference :=
           Harriet.Db.Null_User_Reference;
         Default_Context  : Harriet.Contexts.Context_Type;
         Connection       : Connection_Holders.Holder;
         Dispatcher       : Harriet.Signals.Signal_Dispatcher;
         On_Clock_Tick_Id : Harriet.Signals.Handler_Id;
         Data             : Session_Data_Access;
      end record;

   overriding procedure Initialize (Session : in out Root_Harriet_Session);
   overriding procedure Finalize (Session : in out Root_Harriet_Session);
   overriding procedure Adjust (Session : in out Root_Harriet_Session);

   overriding function Valid
     (Session   : Root_Harriet_Session)
      return Boolean;

   overriding function Is_Administrator
     (Session   : Root_Harriet_Session)
      return Boolean;

   overriding function User_Name
     (Session   : Root_Harriet_Session)
      return String;

   overriding function Faction_Name
     (Session   : Root_Harriet_Session)
      return String;

   overriding function New_Client
     (Session        : in out Root_Harriet_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Harriet.UI.Client_Id;

   overriding procedure Replace_Model
     (Session        : in out Root_Harriet_Session;
      Client         : Harriet.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String);

   overriding procedure Close_Client
     (Session   : in out Root_Harriet_Session;
      Client    : Harriet.UI.Client_Id);

   overriding procedure Set_Connection
     (Session    : in out Root_Harriet_Session;
      Connection : Harriet.UI.Connection_Interface'Class);

   overriding function Handle_Message
     (Session    : in out Root_Harriet_Session;
      Message    : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding procedure Send_Message
     (Session : Root_Harriet_Session;
      Message : Harriet.Json.Json_Value'Class);

   overriding function Execute_Command
     (Session : in out Root_Harriet_Session;
      Client  : Harriet.UI.Client_Id;
      Command : String)
      return Harriet.Json.Json_Value'Class;

   overriding function Handle_Client_Get
     (Session : Root_Harriet_Session;
      Client  : Harriet.UI.Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Handle_Client_Post
     (Session : in out Root_Harriet_Session;
      Client  : Harriet.UI.Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding procedure Send_Signal
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type);

   overriding function Add_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
     return Harriet.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id);

   overriding function Status_Value
     (Session : Root_Harriet_Session;
      Name    : String)
      return Harriet.Json.Json_Value'Class;

   overriding procedure Set_Status_Value
     (Session : in out Root_Harriet_Session;
      Name    : String;
      Value   : Harriet.Json.Json_Value'Class);

   overriding function Environment_Value
     (Session : Root_Harriet_Session;
      Name  : String)
      return Harriet.Json.Json_Value'Class;

   function Status_Message
     (Session : Root_Harriet_Session'Class)
      return Json.Json_Object;

   function Default_Context
     (Session : Root_Harriet_Session'Class)
      return Harriet.Contexts.Context_Type
   is (Session.Default_Context);

end Harriet.Sessions;
