with Ada.Containers.Indefinite_Holders;

with Harriet.Money;

with Harriet.UI.Sessions;

with Harriet.Db.Faction;

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

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Message : Faction_Message)
   is
      M : Json.Json_Object;
      F : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get (Message.Faction);
   begin
      if Harriet.UI.Sessions.Is_Active (F.User) then
         if (for some Flag of Message.Flags => Flag) then
            M.Set_Property ("type", "update-faction");
            if Message.Flags (Cash_Changed) then
               M.Set_Property ("cash", Float (Harriet.Money.To_Real (F.Cash)));
            end if;
            Harriet.UI.Sessions.Element (F.User)
              .Send_Message (M);
         end if;
      end if;
   end Send_Message;

end Harriet.UI;
