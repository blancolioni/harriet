with Harriet.Factions;
with Harriet.Worlds;

--  with Harriet.Colonies.Updates;

with Harriet.Db.Colony;

package body Harriet.Managers.Colonies is

   type Root_Colony_Manager is new Root_Manager_Type with
      record
         Colony  : Harriet.Db.Colony_Reference;
         Faction : Harriet.Db.Faction_Reference;
         World   : Harriet.Db.World_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager)
      return String
   is ("colony" & Harriet.Db.To_String (Manager.Colony) & " manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager)
   is
   begin
      Manager.Log
        (Harriet.Factions.Name (Harriet.Factions.Get (Manager.Faction))
         & " colony on "
         & Harriet.Worlds.Name (Manager.World)
         & " activating");
      Manager.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
   end Activate;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Colony  : constant Harriet.Db.Colony.Colony_Type :=
                  Harriet.Db.Colony.Get_Colony (Managed);
      Manager : Root_Colony_Manager :=
                  Root_Colony_Manager'
                    (Root_Manager_Type with
                     Colony          => Colony.Get_Colony_Reference,
                     Faction         => Colony.Faction,
                     World           => Colony.World);
   begin
      Manager.Colony := Colony.Get_Colony_Reference;
      return new Root_Colony_Manager'(Manager);
   end Create_Default_Manager;

end Harriet.Managers.Colonies;
