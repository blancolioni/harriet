with Harriet.Quantities;

package Harriet.Managers.Goals is

   procedure Colony_Needs_Resource
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      Colony   : Harriet.Db.Colony_Reference;
      Resource : Harriet.Db.Resource_Reference;
      Quantity : Harriet.Quantities.Quantity_Type);

   procedure Add_Colonisation_Goal
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      World    : Harriet.Db.World_Reference);

   procedure Add_World_Scan_Goal
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      World    : Harriet.Db.World_Reference);

   procedure Add_System_Scan_Goal
     (Faction  : Harriet.Db.Faction_Reference;
      Priority : Priority_Type;
      System   : Harriet.Db.Star_System_Reference);

end Harriet.Managers.Goals;
