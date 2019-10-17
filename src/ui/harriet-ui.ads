with Harriet.Json;
with Harriet.Signals;

package Harriet.UI is

   Signal_Clock_Tick : constant Harriet.Signals.Signal_Type :=
     "signal-clock-tick";

   type Client_Id is new Natural;

   type Connection_Interface is interface
     and Harriet.Signals.Signal_Data_Interface;

   procedure Send_Message
     (Connection : in out Connection_Interface;
      Message    : Harriet.Json.Json_Value'Class)
   is abstract;

   type UI_Interface is interface;

   procedure Start
     (Item  : UI_Interface)
   is abstract;

   procedure Stop
     (Item    : UI_Interface;
      Message : String)
   is abstract;

   procedure Broadcast
     (UI     : UI_Interface;
      Signal : Harriet.Signals.Signal_Type)
   is abstract;

   function Current_UI return UI_Interface'Class;

   type State_Interface is interface
     and Harriet.Signals.Signaler;

   function Valid
     (State : State_Interface)
      return Boolean
      is abstract;

   function User_Name
     (State : State_Interface)
      return String
   is abstract
     with Pre'Class => State.Valid;

   function Faction_Name
     (State : State_Interface)
      return String
      is abstract
     with Pre'Class => State.Valid;

   function Is_Administrator
     (State : State_Interface)
      return Boolean
      is abstract
     with Pre'Class => State.Valid;

   function New_Client
     (State          : in out State_Interface;
      Model_Name     : String;
      Model_Argument : String)
      return Client_Id
      is abstract;

   procedure Replace_Model
     (State          : in out State_Interface;
      Client         : Client_Id;
      Model_Name     : String;
      Model_Argument : String)
      is abstract;

   procedure Close_Client
     (State  : in out State_Interface;
      Client : Client_Id)
   is abstract;

   function Handle_Message
     (State      : in out State_Interface;
      Connection : Connection_Interface'Class;
      Message    : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
      is abstract;

   function Execute_Command
     (State   : in out State_Interface;
      Client  : Client_Id;
      Command : String)
      return Harriet.Json.Json_Value'Class
   is abstract;

   function Handle_Client_Request
     (State   : in out State_Interface;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is abstract;

   function Status_Value
     (State : State_Interface;
      Name  : String)
      return Harriet.Json.Json_Value'Class
      is abstract;

   procedure Set_Status_Value
     (State : in out State_Interface;
      Name  : String;
      Value : Harriet.Json.Json_Value'Class)
   is abstract;

   function Environment_Value
     (State : State_Interface;
      Name  : String)
      return Harriet.Json.Json_Value'Class
   is abstract;

   function Environment_Value
     (State : State_Interface'Class;
      Name  : String)
      return String
   is (State.Environment_Value (Name).Image);

   procedure Close_All;

private

   procedure On_UI_Started (UI : UI_Interface'Class);

end Harriet.UI;
