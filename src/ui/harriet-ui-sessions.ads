private package Harriet.UI.Sessions is

   function Exists (Id : String) return Boolean;

   procedure New_Session
     (Id    : String;
      State : State_Interface'Class)
     with Pre => not Exists (Id);

   procedure Close_Session
     (Id : String)
     with Pre => Exists (Id);

   procedure Close_All_Sessions;

   function Reference
     (Id : String)
      return access State_Interface'Class
     with Pre => Exists (Id);

   procedure Broadcast
     (Signal : Harriet.Signals.Signal_Type);

end Harriet.UI.Sessions;
