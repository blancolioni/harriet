with Harriet.Db;

package Harriet.Managers.Execution is

   procedure Start_Manager
     (Managed : Harriet.Db.Managed_Reference);

   procedure Start_Middle_Manager
     (Faction : Harriet.Db.Faction_Reference;
      Area    : Middle_Manager_Area;
      Name    : String);

   procedure Load_Managers;

end Harriet.Managers.Execution;
