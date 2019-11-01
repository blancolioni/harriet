with Ada.Unchecked_Deallocation;

with WL.String_Maps;

with Harriet.Db.User;

package body Harriet.UI.Sessions is

   type State_Access is access all State_Interface'Class;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (State_Interface'Class, State_Access);

   package State_Maps is
     new WL.String_Maps (State_Access);

   States      : State_Maps.Map;
   User_States : State_Maps.Map;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast
     (Signal : Harriet.Signals.Signal_Type)
   is
   begin
      for State of States loop
         State.Send_Signal (Signal);
      end loop;
   end Broadcast;

   ------------------------
   -- Close_All_Sessions --
   ------------------------

   procedure Close_All_Sessions is
   begin
      User_States.Clear;
      for State of States loop
         Free (State);
      end loop;
      States.Clear;
   end Close_All_Sessions;

   -------------------
   -- Close_Session --
   -------------------

   procedure Close_Session (Id : String) is
      State : State_Access := States.Element (Id);
   begin
      User_States.Delete (State.User_Name);
      States.Delete (Id);
      Free (State);
   end Close_Session;

   -------------
   -- Element --
   -------------

   function Element
     (User : Harriet.Db.User_Reference)
      return access constant State_Interface'Class
   is
   begin
      return User_States.Element
        (Harriet.Db.User.Get (User).Login);
   end Element;

   ------------
   -- Exists --
   ------------

   function Exists (Id : String) return Boolean is
   begin
      return States.Contains (Id);
   end Exists;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (User : Harriet.Db.User_Reference)
      return Boolean
   is
   begin
      return User_States.Contains (Harriet.Db.User.Get (User).Login);
   end Is_Active;

   -----------------
   -- New_Session --
   -----------------

   procedure New_Session
     (Id    : String;
      State : State_Interface'Class)
   is
      New_State : constant State_Access :=
        new State_Interface'Class'(State);
   begin
      States.Insert (Id, New_State);
      User_States.Insert (State.User_Name, New_State);
   end New_Session;

   ---------------
   -- Reference --
   ---------------

   function Reference (Id : String) return access State_Interface'Class is
   begin
      return States.Element (Id);
   end Reference;

   --------------------------
   -- Scan_Active_Sessions --
   --------------------------

   procedure Scan_Active_Sessions
     (Process : not null access
        procedure (State : State_Interface'Class))
   is
   begin
      for State of States loop
         Process (State.all);
      end loop;
   end Scan_Active_Sessions;

end Harriet.UI.Sessions;
