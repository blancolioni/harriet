with Harriet.Db.Faction;

package body Harriet.Managers.Factions is

   type Root_Faction_Manager is new Root_Manager_Type with
      record
         Faction : Harriet.Db.Faction_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Faction_Manager)
      return String
   is ("faction" & Harriet.Db.To_String (Manager.Faction) & " manager");

   overriding procedure Activate
     (Manager : not null access Root_Faction_Manager)
   is null;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Manager : Root_Faction_Manager;
      Faction : constant Harriet.Db.Faction.Faction_Type :=
        Harriet.Db.Faction.Get_Faction (Managed);
   begin
      Manager.Faction := Faction.Get_Faction_Reference;
      return new Root_Faction_Manager'(Manager);
   end Create_Default_Manager;

end Harriet.Managers.Factions;
