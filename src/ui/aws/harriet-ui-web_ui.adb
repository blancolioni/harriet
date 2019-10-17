with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;

with Harriet.Calendar;

with Harriet.UI.Sessions;
with Harriet.UI.Web_UI.Handlers;
with Harriet.UI.Web_UI.Logging;
with Harriet.UI.Web_UI.Routes;

package body Harriet.UI.Web_UI is

   type Active_Socket_Record is
      record
         Recipient  : AWS.Net.WebSocket.Registry.Recipient;
         Session_Id : Ada.Strings.Unbounded.Unbounded_String;
         Connecting : Boolean;
      end record;

   package Active_Socket_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => AWS.Net.WebSocket.UID,
        Element_Type => Active_Socket_Record,
        "<"          => AWS.Net.WebSocket."<");

   Active_Sockets : Active_Socket_Maps.Map;

   type Socket_Type is
     new AWS.Net.WebSocket.Object
   with null record;

   overriding procedure On_Close
     (Socket : in out Socket_Type;
      Message : String);

   overriding procedure On_Error
     (Socket  : in out Socket_Type;
      Message : String);

   overriding procedure On_Message
     (Socket  : in out Socket_Type;
      Message : String);

   overriding procedure On_Open
     (Socket  : in out Socket_Type;
      Message : String);

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class;

   type Socket_Connection is
     new Connection_Interface with
      record
         Recipient : AWS.Net.WebSocket.Registry.Recipient;
      end record;

   overriding procedure Send_Message
     (Connection : Socket_Connection;
      Message    : Harriet.Json.Json_Value'Class);

   type Web_UI_Type is
     new UI_Interface
     and Harriet.Signals.Signaler with
      record
         null;
      end record;

   overriding function Add_Handler
     (UI      : in out Web_UI_Type;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
      return Harriet.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (UI      : in out Web_UI_Type;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id);

   overriding procedure Send_Signal
     (UI     : in out Web_UI_Type;
      Signal : Harriet.Signals.Signal_Type);

   overriding procedure Start
     (Web_UI  : Web_UI_Type);

   overriding procedure Stop
     (Item    : Web_UI_Type;
      Message : String);

   overriding procedure Broadcast
     (UI     : Web_UI_Type;
      Signal : Harriet.Signals.Signal_Type);

   procedure On_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class);

   Server           : AWS.Server.HTTP;
   Broadcaster      : Harriet.Signals.Signal_Dispatcher;
   Clock_Handler_Id : Harriet.Signals.Handler_Id;
   Broadcast_Rcp    : AWS.Net.WebSocket.Registry.Recipient;

   procedure Create_Routes;

   procedure Create_Socket;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (UI      : in out Web_UI_Type;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
      return Harriet.Signals.Handler_Id
   is
      pragma Unreferenced (UI);
   begin
      return Broadcaster.Add_Handler
        (Signal  => Signal,
         Handler => Handler,
         Data    => Data);
   end Add_Handler;

   ---------------
   -- Broadcast --
   ---------------

   overriding procedure Broadcast
     (UI      : Web_UI_Type;
      Signal  : Harriet.Signals.Signal_Type)
   is
   begin
      Broadcaster.Call_Handlers
        (UI, Signal);
   end Broadcast;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class
   is
   begin
      return Socket_Type'
        (AWS.Net.WebSocket.Object
           (AWS.Net.WebSocket.Create (Socket, Request)) with null record);
   end Create;

   -------------------
   -- Create_Routes --
   -------------------

   procedure Create_Routes is
   begin
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/login",
         Handler => Handlers.Handle_Login);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/new-client",
         Handler => Handlers.Handle_New_Client);
      Routes.Add_Route
        (Method  => AWS.Status.GET,
         Path    => "/environment/:name",
         Handler => Handlers.Handle_Environment_Request);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/client/:client",
         Handler => Handlers.Handle_Client_Request);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/status/:setting/:value",
         Handler => Handlers.Handle_Status_Request);
   end Create_Routes;

   -------------------
   -- Create_Socket --
   -------------------

   procedure Create_Socket is
   begin
      AWS.Net.WebSocket.Registry.Register
        (URI     => "/socket",
         Factory => Create'Access);
      AWS.Net.WebSocket.Registry.Control.Start;
      Broadcast_Rcp :=
        AWS.Net.WebSocket.Registry.Create (URI => "/socket");
   end Create_Socket;

   ----------------
   -- Get_Web_UI --
   ----------------

   function Get_Web_UI return UI_Interface'Class is
   begin
      return Web_UI : Web_UI_Type;
   end Get_Web_UI;

   -------------------
   -- On_Clock_Tick --
   -------------------

   procedure On_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Object, Data);
      Now : constant Harriet.Calendar.Time := Harriet.Calendar.Clock;
      Message : Json.Json_Object;
   begin
      Message.Set_Property ("type", "update-state");
      Message.Set_Property
        ("currentTime",
         Float (Harriet.Calendar.To_Real (Now)));
      Message.Set_Property
        ("currentTimeImage",
         Harriet.Calendar.Image (Now, False));
      AWS.Net.WebSocket.Registry.Send
        (To           => Broadcast_Rcp,
         Message      => Message.Serialize);
   end On_Clock_Tick;

   --------------
   -- On_Close --
   --------------

   overriding procedure On_Close
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Socket);
   begin
      null;
   end On_Close;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Socket);
   begin
      null;
   end On_Error;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Socket  : in out Socket_Type;
      Message : String)
   is
      UID : constant AWS.Net.WebSocket.UID :=
        Socket.Get_UID;
   begin
      pragma Assert (Active_Sockets.Contains (UID));

      if Active_Sockets (UID).Connecting then
         declare
            Id : constant String :=
              Json.Deserialize (Message).Get_Property ("id").Image;
         begin
            if Harriet.UI.Sessions.Exists (Id) then
               Active_Sockets (UID).Session_Id :=
                 Ada.Strings.Unbounded.To_Unbounded_String (Id);
               Active_Sockets (UID).Connecting := False;
               Harriet.UI.Sessions.Reference (Id).Set_Connection
                 (Socket_Connection'
                    (Recipient => Active_Sockets (UID).Recipient));
               Socket.Send (Json.Serialize (Json.String_Value ("ok")));
            else
               Socket.Close ("invalid");
               Active_Sockets.Delete (UID);
            end if;
         end;
      else
         declare
            Response : constant String :=
              Routes.Handle_Socket_Message (Message);
         begin
            Socket.Send (Message => Response);
         end;
      end if;
   end On_Message;

   -------------
   -- On_Open --
   -------------

   overriding procedure On_Open
     (Socket  : in out Socket_Type;
      Message : String)
   is
      pragma Unreferenced (Message);
      UID : constant AWS.Net.WebSocket.UID :=
        Socket.Get_UID;
   begin
      pragma Assert (not Active_Sockets.Contains (UID));
      Active_Sockets.Insert
        (UID,
         Active_Socket_Record'
           (Recipient  =>
                AWS.Net.WebSocket.Registry.Create (UID),
            Session_Id => <>,
            Connecting => True));

   end On_Open;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (UI      : in out Web_UI_Type;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id)
   is
      pragma Unreferenced (UI);
   begin
      Broadcaster.Remove_Handler (Signal, Id);
   end Remove_Handler;

   ------------------
   -- Send_Message --
   ------------------

   overriding procedure Send_Message
     (Connection     : Socket_Connection;
      Message    : Harriet.Json.Json_Value'Class)
   is
   begin
      AWS.Net.WebSocket.Registry.Send
        (Connection.Recipient, Message.Serialize);
   end Send_Message;

   -----------------
   -- Send_Signal --
   -----------------

   overriding procedure Send_Signal
     (UI     : in out Web_UI_Type;
      Signal : Harriet.Signals.Signal_Type)
   is
   begin
      Broadcaster.Call_Handlers (UI, Signal);
   end Send_Signal;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI : Web_UI_Type) is
   begin

      Logging.On_Starting;

      On_UI_Started (Web_UI);

      Create_Routes;
      Create_Socket;

      AWS.Server.Start
        (Web_Server => Server,
         Name       => "Harriet",
         Callback   => Routes.Handle_Http_Request'Access,
         Port       => 8080);

   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Item    : Web_UI_Type;
      Message : String)
   is
      pragma Unreferenced (Item);
   begin
      Logging.On_Stopping (Message);
      Broadcaster.Remove_Handler
        (Harriet.UI.Signal_Clock_Tick, Clock_Handler_Id);
      AWS.Net.WebSocket.Registry.Control.Shutdown;
      AWS.Server.Shutdown (Server);
      Logging.On_Stop;
   end Stop;

begin
   Clock_Handler_Id :=
     Harriet.Signals.Add_Handler
       (Dispatcher  => Broadcaster,
        Signal  => Harriet.UI.Signal_Clock_Tick,
        Handler => On_Clock_Tick'Access,
        Data    => Harriet.Signals.Null_Signal_Data'(null record));

end Harriet.UI.Web_UI;
