with Ada.Unchecked_Deallocation;

with WL.String_Maps;

package body Harriet.UI.Sessions is

   type State_Access is access all State_Interface'Class;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (State_Interface'Class, State_Access);

   package State_Maps is
     new WL.String_Maps (State_Access);

   States : State_Maps.Map;

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
      States.Delete (Id);
      Free (State);
   end Close_Session;

   ------------
   -- Exists --
   ------------

   function Exists (Id : String) return Boolean is
   begin
      return States.Contains (Id);
   end Exists;

   -----------------
   -- New_Session --
   -----------------

   procedure New_Session
     (Id    : String;
      State : State_Interface'Class)
   is
   begin
      States.Insert (Id, new State_Interface'Class'(State));
   end New_Session;

   ---------------
   -- Reference --
   ---------------

   function Reference (Id : String) return access State_Interface'Class is
   begin
      return States.Element (Id);
   end Reference;

end Harriet.UI.Sessions;
