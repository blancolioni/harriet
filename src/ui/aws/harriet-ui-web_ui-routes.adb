with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with WL.Guids;

with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;

with Harriet.UI.Sessions;
with Harriet.UI.Web_UI.Logging;

package body Harriet.UI.Web_UI.Routes is

   package Request_Handler_Holders is
     new Ada.Containers.Indefinite_Holders
       (Request_Handler'Class);

   type Route_Record is
      record
         Template : Ada.Strings.Unbounded.Unbounded_String;
         Handler  : Request_Handler_Holders.Holder;
      end record;

   package Route_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Route_Record);

   type Method_Route_Array is
     array (AWS.Status.Request_Method) of Route_Lists.List;

   Routes : Method_Route_Array;

   function Match
     (Template : String;
      URI      : String)
      return Boolean;

   function Get_Parameters
     (Request    : AWS.Status.Data;
      Template   : String)
     return Parameter_Container;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Split_Path
     (Path : String)
      return String_Vectors.Vector;

   function Handle (Request : AWS.Status.Data) return AWS.Response.Data;

   ---------------
   -- Add_Route --
   ---------------

   procedure Add_Route
     (Method  : AWS.Status.Request_Method;
      Path    : String;
      Handler : Request_Handler'Class)
   is
   begin
      Routes (Method).Append
        (Route_Record'
           (Ada.Strings.Unbounded.To_Unbounded_String (Path),
            Request_Handler_Holders.To_Holder (Handler)));
   end Add_Route;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Request    : AWS.Status.Data;
      Template   : String)
      return Parameter_Container
   is
      Ts : constant String_Vectors.Vector := Split_Path (Template);
      Us : constant String_Vectors.Vector :=
        Split_Path (AWS.Status.URI (Request));
      Ps : constant AWS.Parameters.List :=
        AWS.Status.Parameters (Request);

      Result : Parameter_Container;

      procedure Param
        (Name, Value : String);

      -----------
      -- Param --
      -----------

      procedure Param
        (Name, Value : String)
      is
      begin
         if Result.Contains (Name) then
            Result.Replace (Name, Value);
         else
            Result.Insert (Name, Value);
         end if;
      end Param;

   begin
      for I in 1 .. Ts.Last_Index loop
         declare
            Item : constant String := Ts.Element (I);
         begin
            if Ada.Strings.Fixed.Head (Item, 1) = ":" then
               Param (Item (Item'First + 1 .. Item'Last), Us.Element (I));
            end if;
         end;
      end loop;

      for I in 1 .. Ps.Count loop
         declare
            Name  : constant String :=
              Ps.Get_Name (I);
            Value : constant String :=
              Ps.Get_Value (I);
         begin
            Param (Name, Value);
         end;
      end loop;

      return Result;

   end Get_Parameters;

   ------------
   -- Handle --
   ------------

   function Handle (Request : AWS.Status.Data) return AWS.Response.Data is
      Method : constant AWS.Status.Request_Method :=
        AWS.Status.Method (Request);
   begin
      for Route of Routes (Method) loop
         if Match
           (Template => Ada.Strings.Unbounded.To_String (Route.Template),
            URI      => AWS.Status.URI (Request))
         then
            declare
               Handler : constant Request_Handler'Class :=
                 Route.Handler.Element;
               Parameters : constant Parameter_Container :=
                 Get_Parameters
                   (Request,
                    Ada.Strings.Unbounded.To_String (Route.Template));
               State_Id   : constant String :=
                 Parameters.Parameter ("id");
               Response   : AWS.Response.Data;
            begin
               case Method is
                  when AWS.Status.GET =>
                     if State_Id = ""
                       or else not Sessions.Exists (State_Id)
                     then
                        Response := AWS.Response.Build
                          (Content_Type  => "text/plain",
                           Message_Body  => "invalid session id",
                           Status_Code   => AWS.Messages.S404);
                     else
                        declare
                           Result_Json : constant Json.Json_Value'Class :=
                             Handle_Get
                               (Handler    => Handler,
                                State      =>
                                  Sessions.Reference (State_Id).all,
                                Parameters => Parameters);
                        begin
                           Response := AWS.Response.Build
                             (Content_Type  => "text/plain",
                              Message_Body  => Json.Serialize (Result_Json));
                        end;
                     end if;
                  when AWS.Status.POST =>
                     if (not Handler.Creates_State
                         and then State_Id = "")
                       or else
                         (State_Id /= ""
                          and then not Sessions.Exists (State_Id))
                     then
                        Response := AWS.Response.Build
                          (Content_Type  => "text/plain",
                           Message_Body  => "invalid session id",
                           Status_Code   => AWS.Messages.S404);
                     elsif State_Id = "" then
                        declare
                           Id        : constant String :=
                             WL.Guids.To_String
                               (WL.Guids.New_Guid);
                           New_State : constant State_Interface'Class :=
                             Handler.Handle_Create (Parameters);
                           Json      : Harriet.Json.Json_Object;
                           Status    : constant AWS.Messages.Status_Code :=
                             (if New_State.Valid
                              then AWS.Messages.S200
                              else AWS.Messages.S401);
                        begin
                           if New_State.Valid then
                              Harriet.UI.Sessions.New_Session
                                (Id    => Id,
                                 State => New_State);

                              Json.Set_Property ("id", Id);
                              Json.Set_Property
                                ("user", New_State.User_Name);
                              Json.Set_Property
                                ("faction", New_State.Faction_Name);
                           else
                              Json.Set_Property ("error", "login failed");
                           end if;
                           Response :=
                             AWS.Response.Build
                               ("text/json", Json.Serialize, Status);
                        end;
                     else
                        declare
                           Result_Json : constant Json.Json_Value'Class :=
                             Handle_Post
                               (Handler    => Handler,
                                State      =>
                                  Sessions.Reference (State_Id).all,
                                Parameters => Parameters);
                        begin
                           Response := AWS.Response.Build
                             (Content_Type  => "text/plain",
                              Message_Body  => Json.Serialize (Result_Json));
                        end;
                     end if;

                  when others =>

                     Response := AWS.Response.Build
                       (Content_Type  => "text/plain",
                        Message_Body  => "Method not allowed",
                        Status_Code   => AWS.Messages.S405);
               end case;

               AWS.Response.Set.Add_Header
                 (Response,
                  "Access-Control-Allow-Origin",
                  "http://localhost:3000");

               return Response;

            end;
         end if;
      end loop;

      return AWS.Response.Build
        (Content_Type  => "text/plain",
         Message_Body  => "Not found",
         Status_Code   => AWS.Messages.S404);
   end Handle;

   -------------------
   -- Handle_Create --
   -------------------

   function Handle_Create
     (Handler    : Request_Handler;
      Parameters : Parameter_Container'Class)
      return State_Interface'Class
   is
      pragma Unreferenced (Handler, Parameters);
   begin
      return (raise Route_Error with "bad GET request");
   end Handle_Create;

   ----------------
   -- Handle_Get --
   ----------------

   function Handle_Get
     (Handler    : Request_Handler;
      State      : State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, State, Parameters);
   begin
      return (raise Route_Error with "bad GET request");
   end Handle_Get;

   -------------------------
   -- Handle_Http_Request --
   -------------------------

   function Handle_Http_Request
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
   begin
      declare
         Response : constant AWS.Response.Data :=
           Handle (Request);
      begin
         Harriet.UI.Web_UI.Logging.Log_Request
           (Request, Response);
         return Response;
      end;
   end Handle_Http_Request;

   -----------------
   -- Handle_Post --
   -----------------

   function Handle_Post
     (Handler    : Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Parameter_Container'Class)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, State, Parameters);
   begin
      return (raise Route_Error with "bad POST request");
   end Handle_Post;

   ---------------------------
   -- Handle_Socket_Message --
   ---------------------------

   function Handle_Socket_Message
     (Connection : Connection_Interface'Class;
      Message    : String)
      return String
   is
      Json : constant Harriet.Json.Json_Value'Class :=
        Harriet.Json.Deserialize (Message);
      Result_Json : constant Harriet.Json.Json_Value'Class :=
        Sessions.Reference (Json.Get_Property ("id"))
          .Handle_Message
          (Connection => Connection,
           Message    => Json);
   begin
      return Result_Json.Serialize;
   end Handle_Socket_Message;

   -----------
   -- Match --
   -----------

   function Match
     (Template : String;
      URI      : String)
      return Boolean
   is
   begin
      if Template = URI then
         return True;
      elsif Ada.Strings.Fixed.Index (Template, ":") = 0 then
         return False;
      end if;

      declare
         Template_Path : constant String_Vectors.Vector :=
           Split_Path (Template);
         URI_Path      : constant String_Vectors.Vector :=
           Split_Path (URI);
      begin

         if Template_Path.Last_Index /= URI_Path.Last_Index then
            return False;
         end if;

         for I in 1 .. Template_Path.Last_Index loop
            if Template_Path (I) /= URI_Path (I)
              and then Ada.Strings.Fixed.Head (Template_Path (I), 1) /= ":"
            then
               return False;
            end if;
         end loop;
         return True;
      end;
   end Match;

   ---------------
   -- Parameter --
   ---------------

   function Parameter
     (Container : Parameter_Container;
      Name      : String)
      return String
   is
      Position : constant String_Maps.Cursor :=
        Container.Find (Name);
   begin
      if String_Maps.Has_Element (Position) then
         return String_Maps.Element (Position);
      else
         return "";
      end if;
   end Parameter;

   ----------------
   -- Split_Path --
   ----------------

   function Split_Path
     (Path : String)
      return String_Vectors.Vector
   is
      P : constant String :=
        (if Path (Path'Last) = '/' then Path else Path & '/');
      Start : Positive := P'First;
      Index : Positive := Start;
   begin
      return Vector : String_Vectors.Vector do
         for Ch of P loop
            if Ch = '/' then
               if Index > Start then
                  Vector.Append (P (Start .. Index - 1));
               end if;

               Start := Index + 1;
            end if;
            Index := Index + 1;
         end loop;
      end return;
   end Split_Path;

   -------------
   -- To_Json --
   -------------

   function To_Json
     (Container : Parameter_Container)
      return Harriet.Json.Json_Value'Class
   is
   begin
      return Json : Harriet.Json.Json_Object do
         for Position in Container.Iterate loop
            Json.Set_Property (String_Maps.Key (Position),
                               String_Maps.Element (Position));
         end loop;
      end return;
   end To_Json;

end Harriet.UI.Web_UI.Routes;
