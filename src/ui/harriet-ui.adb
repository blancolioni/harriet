with Ada.Containers.Indefinite_Holders;

with Harriet.UI.Sessions;

package body Harriet.UI is

   package UI_Holders is
     new Ada.Containers.Indefinite_Holders (UI_Interface'Class);

   Holder : UI_Holders.Holder;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Signal : Harriet.Signals.Signal_Type) is
   begin
      Current_UI.Broadcast (Signal);
      Harriet.UI.Sessions.Broadcast (Signal);
   end Broadcast;

   ---------------
   -- Close_All --
   ---------------

   procedure Close_All is
   begin
      Sessions.Close_All_Sessions;
   end Close_All;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return UI_Interface'Class is
   begin
      return Holder.Element;
   end Current_UI;

   -------------------
   -- On_UI_Started --
   -------------------

   procedure On_UI_Started (UI : UI_Interface'Class) is
   begin
      Holder := UI_Holders.To_Holder (UI);
   end On_UI_Started;

end Harriet.UI;
