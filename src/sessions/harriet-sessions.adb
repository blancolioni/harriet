with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Tropos.Reader;
with Harriet.Configure;

with Harriet.Calendar;
with Harriet.Commands.Writers;
with Harriet.Money;

with Harriet.File_System.Root;

with Harriet.Sessions.Status;

with Harriet.UI.Models.Loader;

with Harriet.Db.Faction;
with Harriet.Db.User;

package body Harriet.Sessions is

   type Status_Get_Function is access
     function (Session : Root_Harriet_Session'Class)
               return Json.Json_Value'Class;

   type Status_Set_Procedure is access
     procedure (Session : in out Root_Harriet_Session'Class;
                Value   : Json.Json_Value'Class);

   type Status_Setting_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Get  : Status_Get_Function;
         Set  : Status_Set_Procedure;
      end record;

   package Status_Setting_Maps is
     new WL.String_Maps (Status_Setting_Record);

   Status_Settings : Status_Setting_Maps.Map;

   procedure Add_Status
     (Name : String;
      Get  : Status_Get_Function;
      Set  : Status_Set_Procedure);

   procedure On_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class);

   function Default_Dashboard return Harriet.Json.Json_Value'Class;

   ------------------
   -- Session_Data --
   ------------------

   protected body Session_Data is

      ------------------
      -- Close_Client --
      ------------------

      procedure Close_Client
        (Client_Id      : Harriet.UI.Client_Id)
      is
      begin
         Client_Map.Delete (Client_Id);
      end Close_Client;

      -------------------
      -- Create_Client --
      -------------------

      procedure Create_Client
        (User           : Harriet.Db.User_Reference;
         Context        : Harriet.Contexts.Context_Type;
         Model_Name     : String;
         Model_Argument : String;
         Client_Id      : out Harriet.UI.Client_Id)
      is
         use type Harriet.UI.Client_Id;
      begin
         Client_Id := 0;

         if not Harriet.UI.Models.Loader.Exists (Model_Name) then
            return;
         end if;

         Last_Client := Last_Client + 1;

         declare
            Model : Harriet.UI.Models.Root_Harriet_Model'Class :=
              Harriet.UI.Models.Loader.Get (Model_Name);
         begin
            Model.Start (User, Model_Argument);
            Client_Map.Insert
              (Last_Client,
               Client_Type'
                 (Model   =>
                      Model_Holders.To_Holder (Model),
                  Context => Context));
         end;

         Client_Id := Last_Client;
      end Create_Client;

      ---------------------
      -- Execute_Command --
      ---------------------

      procedure Execute_Command
        (Client_Id : Harriet.UI.Client_Id;
         Writer    : in out Harriet.Writers.Writer_Interface'Class;
         Command   : String)
      is
      begin
         Harriet.Commands.Execute_Command_Line
           (Line    => Command,
            Context => Client_Map (Client_Id).Context,
            Writer  => Writer);
      end Execute_Command;

      ---------------------------
      -- Get_Environment_Value --
      ---------------------------

      function Get_Environment_Value
        (Name : String)
         return Json.Json_Value'Class
      is
      begin
         if Environment.Contains (Name) then
            return Environment.Element (Name);
         else
            return Harriet.Json.Null_Value;
         end if;
      end Get_Environment_Value;

      ---------------
      -- Get_Model --
      ---------------

      function Get_Model
        (Client_Id : Harriet.UI.Client_Id)
         return Harriet.UI.Models.Root_Harriet_Model'Class
      is
      begin
         return Client_Map.Element (Client_Id).Model.Element;
      end Get_Model;

      ---------------
      -- Reference --
      ---------------

      procedure Reference is
      begin
         References := References + 1;
      end Reference;

      ---------------------------
      -- Set_Environment_Value --
      ---------------------------

      procedure Set_Environment_Value
        (Name  : String;
         Value : Json.Json_Value'Class)
      is
      begin
         if Environment.Contains (Name) then
            Environment.Replace (Name, Value);
         else
            Environment.Insert (Name, Value);
         end if;
      end Set_Environment_Value;

      ---------------
      -- Set_Model --
      ---------------

      procedure Set_Model
        (Client_Id : Harriet.UI.Client_Id;
         Model     : Harriet.UI.Models.Root_Harriet_Model'Class)
      is
      begin
         Client_Map (Client_Id).Model :=
           Model_Holders.To_Holder (Model);
      end Set_Model;

      -----------------
      -- Unreference --
      -----------------

      procedure Unreference (Finished : out Boolean) is
      begin
         References := References - 1;
         Finished := References = 0;
      end Unreference;

   end Session_Data;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Handler : Harriet.Signals.Handler_Type;
      Data    : Harriet.Signals.Signal_Data_Interface'Class)
      return Harriet.Signals.Handler_Id
   is
   begin
      return Session.Dispatcher.Add_Handler (Signal, Handler, Data);
   end Add_Handler;

   ----------------
   -- Add_Status --
   ----------------

   procedure Add_Status
     (Name : String;
      Get  : Status_Get_Function;
      Set  : Status_Set_Procedure)
   is
   begin
      Status_Settings.Insert
        (Name,
         Status_Setting_Record'
           (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Get  => Get,
            Set  => Set));
   end Add_Status;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Session : in out Root_Harriet_Session) is
   begin
      if Session.Data /= null then
         Session.Data.Reference;
      end if;
   end Adjust;

   ------------------
   -- Close_Client --
   ------------------

   overriding procedure Close_Client
     (Session   : in out Root_Harriet_Session;
      Client    : Harriet.UI.Client_Id)
   is
   begin
      Session.Data.Close_Client (Client);
   end Close_Client;

   -----------------------
   -- Default_Dashboard --
   -----------------------

   function Default_Dashboard return Harriet.Json.Json_Value'Class is

      Boxes : Harriet.Json.Json_Array;

      procedure Add_Box
        (Id : Natural;
         Left, Top : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0);

      -------------
      -- Add_Box --
      -------------

      procedure Add_Box
        (Id            : Natural;
         Left, Top     : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0)
      is
         Box    : Harriet.Json.Json_Object;
      begin

         Box.Set_Property ("id", Id);

         declare
            Anchor : Harriet.Json.Json_Object;
         begin
            Anchor.Set_Property ("left", Left);
            Anchor.Set_Property ("top", Top);
            Anchor.Set_Property ("right", Right);
            Anchor.Set_Property ("bottom", Bottom);
            Box.Set_Property ("anchor", Anchor);
         end;

         if Child_1 > 0 then
            declare
               Child_Boxes : Harriet.Json.Json_Array;
            begin
               Child_Boxes.Append (Json.Integer_Value (Child_1));
               Child_Boxes.Append (Json.Integer_Value (Child_2));
               Box.Set_Property ("childBoxes", Child_Boxes);
            end;
         end if;

         Boxes.Append (Box);
      end Add_Box;

      Next_Id : Natural := 0;

   begin
      for Config of
        Tropos.Reader.Read_Config
          (Harriet.Configure.Scenario_File
             ("default", "factions", "dashboard.config"))
      loop
         declare
            function Anchor (Name : String) return Positive
            is (Config.Child ("anchor").Get (Name));

            function Child (Index : Positive) return Natural
            is (if Config.Contains ("childBoxes")
                then Config.Child ("childBoxes").Get (Index)
                else 0);
         begin
            Add_Box
              (Id      => Next_Id,
               Left    => Anchor ("left"),
               Top     => Anchor ("top"),
               Right   => Anchor ("right"),
               Bottom  => Anchor ("bottom"),
               Child_1 => Child (1),
               Child_2 => Child (2));
         end;
         Next_Id := Next_Id + 1;
      end loop;

      return Dashboard : Harriet.Json.Json_Object do
         Dashboard.Set_Property ("nextId", Next_Id);
         Dashboard.Set_Property ("boxes", Boxes);
      end return;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Unable to load default dashboard: "
            & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "config path: "
            & Harriet.Configure.Scenario_File
              ("default", "factions", "dashboard.config"));
         raise;
   end Default_Dashboard;

   -----------------------
   -- Environment_Value --
   -----------------------

   overriding function Environment_Value
     (Session : Root_Harriet_Session;
      Name    : String)
      return Harriet.Json.Json_Value'Class
   is
   begin
      return Session.Data.Get_Environment_Value (Name);
   end Environment_Value;

   ---------------------
   -- Execute_Command --
   ---------------------

   overriding function Execute_Command
     (Session : in out Root_Harriet_Session;
      Client  : Harriet.UI.Client_Id;
      Command : String)
      return Harriet.Json.Json_Value'Class
   is
      Writer : Harriet.Commands.Writers.Json_Writer;

   begin

      Session.Data.Execute_Command
        (Client, Writer, Command);
      return Writer.To_Json;

   end Execute_Command;

   ------------------
   -- Faction_Name --
   ------------------

   overriding function Faction_Name
     (Session   : Root_Harriet_Session)
      return String
   is
      Faction : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.First_By_User
          (Session.User);
   begin
      if Faction.Has_Element then
         return Faction.Name;
      else
         return Session.User_Name;
      end if;
   end Faction_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Session : in out Root_Harriet_Session) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Session_Data, Session_Data_Access);
   begin
      if Session.Data /= null then
         declare
            Finished : Boolean;
         begin
            Session.Data.Unreference (Finished);
            if Finished then
               Ada.Text_IO.Put_Line
                 ("close session");
               Free (Session.Data);
            end if;
         end;
      end if;
   end Finalize;

   ---------------------------
   -- Handle_Client_Request --
   ---------------------------

   overriding function Handle_Client_Request
     (Session : in out Root_Harriet_Session;
      Client  : Harriet.UI.Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      Model : Harriet.UI.Models.Root_Harriet_Model'Class :=
        Session.Data.Get_Model (Client);
   begin
      return Model.Handle (Session, Client, Request);
   end Handle_Client_Request;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (Session    : in out Root_Harriet_Session;
      Message    : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Message);
   begin
      return Session.Status_Message;
   end Handle_Message;

   overriding procedure Initialize (Session : in out Root_Harriet_Session)
   is null;

   ----------------------
   -- Is_Administrator --
   ----------------------

   overriding function Is_Administrator
     (Session   : Root_Harriet_Session)
      return Boolean
   is
   begin
      return Harriet.Db.User.Get (Session.User).Administrator;
   end Is_Administrator;

   -------------------------------
   -- New_Administrator_Session --
   -------------------------------

   function New_Administrator_Session
     return Harriet.UI.State_Interface'Class
   is
      User : constant Harriet.Db.User.User_Type :=
        Harriet.Db.User.Get_By_Login ("root");
   begin
      return New_Session (User.Login, User.Password);
   end New_Administrator_Session;

   ----------------
   -- New_Client --
   ----------------

   overriding function New_Client
     (Session        : in out Root_Harriet_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Harriet.UI.Client_Id
   is
   begin
      return Id : Harriet.UI.Client_Id do
         Session.Data.Create_Client
           (Session.User, Session.Default_Context,
            Model_Name, Model_Argument,
            Id);
      end return;
   end New_Client;

   -----------------
   -- New_Session --
   -----------------

   function New_Session
     (User_Name : String;
      Password  : String)
      return Harriet.UI.State_Interface'Class
   is
      User : constant Harriet.Db.User.User_Type :=
        Harriet.Db.User.Get_By_Login (User_Name);
      Session : Root_Harriet_Session;
   begin
      if User.Has_Element
        and then User.Password = Password
      then
         Session.User := User.Get_User_Reference;

         declare
            Faction : constant Harriet.Db.Faction.Faction_Type :=
              Harriet.Db.Faction.First_By_User
                (User.Get_User_Reference);
            Home    : constant String :=
              (if Faction.Has_Element
               then "/home/" & Faction.Name
               else "/");
         begin
            if Faction.Has_Element
              or else User.Administrator
            then
               Session.Default_Context.Create_Context
                 (Root          =>
                    Harriet.File_System.Root.System_Root_Node_Id,
                  Default_Scope => Home);
               Session.Data := new Session_Data;
               Session.Data.Set_Environment_Value
                 ("HOME", Harriet.Json.String_Value (Home));
               Session.Data.Set_Environment_Value
                 ("DASHBOARD", Default_Dashboard);
               Session.On_Clock_Tick_Id :=
                 Session.Add_Handler
                   (Signal     => Harriet.UI.Signal_Clock_Tick,
                    Handler    => On_Clock_Tick'Access,
                    Data       =>
                      Harriet.Signals.Null_Signal_Data'(null record));
            else
               Session.User := Harriet.Db.Null_User_Reference;
            end if;
         end;
      end if;
      return Session;
   end New_Session;

   -------------------
   -- On_Clock_Tick --
   -------------------

   procedure On_Clock_Tick
     (Object : Harriet.Signals.Signaler'Class;
      Data   : Harriet.Signals.Signal_Data_Interface'Class)
   is
      pragma Unreferenced (Data);
      Session    : Root_Harriet_Session'Class renames
        Root_Harriet_Session'Class (Object);
   begin
      Session.Connection.Element.Send_Message
        (Session.Status_Message);
   end On_Clock_Tick;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type;
      Id      : Harriet.Signals.Handler_Id)
   is
   begin
      Session.Dispatcher.Remove_Handler (Signal, Id);
   end Remove_Handler;

   -------------------
   -- Replace_Model --
   -------------------

   overriding procedure Replace_Model
     (Session        : in out Root_Harriet_Session;
      Client         : Harriet.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String)
   is
      Model : Harriet.UI.Models.Root_Harriet_Model'Class :=
        Harriet.UI.Models.Loader.Get (Model_Name);
   begin
      Model.Start (Session.User, Model_Argument);
      Session.Data.Set_Model (Client, Model);
   end Replace_Model;

   -----------------
   -- Send_Signal --
   -----------------

   overriding procedure Send_Signal
     (Session : in out Root_Harriet_Session;
      Signal  : Harriet.Signals.Signal_Type)
   is
   begin
      Session.Dispatcher.Call_Handlers (Session, Signal);
   end Send_Signal;

   --------------------
   -- Set_Connection --
   --------------------

   overriding procedure Set_Connection
     (Session    : in out Root_Harriet_Session;
      Connection : Harriet.UI.Connection_Interface'Class)
   is
   begin
      Session.Connection := Connection_Holders.To_Holder (Connection);
   end Set_Connection;

   ----------------------
   -- Set_Status_Value --
   ----------------------

   overriding procedure Set_Status_Value
     (Session : in out Root_Harriet_Session;
      Name    : String;
      Value   : Harriet.Json.Json_Value'Class)
   is
   begin
      if Status_Settings.Contains (Name) then
         Status_Settings.Element (Name).Set (Session, Value);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "no such status: " & Name);
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "error setting " & Name & ": "
            & Ada.Exceptions.Exception_Message (E));

   end Set_Status_Value;

   --------------------
   -- Status_Message --
   --------------------

   function Status_Message
     (Session : Root_Harriet_Session'Class)
      return Json.Json_Object
   is
      Now : constant Harriet.Calendar.Time := Harriet.Calendar.Clock;
   begin
      return Message : Json.Json_Object do
         Message.Set_Property ("type", "update-faction");
         Message.Set_Property
           ("cash",
            Float
              (Harriet.Money.To_Real
                   (Harriet.Db.Faction.First_By_User
                        (Session.User)
                    .Cash)));
         Message.Set_Property
           ("currentTime",
            Float (Harriet.Calendar.To_Real (Now)));
         Message.Set_Property
           ("currentTimeImage",
            Harriet.Calendar.Image (Now, False));
      end return;
   end Status_Message;

   ------------------
   -- Status_Value --
   ------------------

   overriding function Status_Value
     (Session : Root_Harriet_Session;
      Name    : String)
      return Harriet.Json.Json_Value'Class
   is
   begin
      if Status_Settings.Contains (Name) then
         return Status_Settings.Element (Name).Get (Session);
      else
         return Result : Json.Json_Object do
            Result.Set_Property ("noSuchSetting", Name);
         end return;
      end if;
   end Status_Value;

   ---------------
   -- User_Name --
   ---------------

   overriding function User_Name
     (Session   : Root_Harriet_Session)
      return String
   is
   begin
      return Harriet.Db.User.Get (Session.User).Login;
   end User_Name;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Session   : Root_Harriet_Session)
      return Boolean
   is
      use type Harriet.Db.User_Reference;
   begin
      return Session.User /= Harriet.Db.Null_User_Reference;
   end Valid;

begin
   Add_Status ("updateSpeed",
               Harriet.Sessions.Status.Get_Update_Speed'Access,
               Harriet.Sessions.Status.Set_Update_Speed'Access);
end Harriet.Sessions;
