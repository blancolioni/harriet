with Tropos;

with Harriet.Color;

with Harriet.Db;

package Harriet.Factions.Create is

   function Create_Faction
     (User        : Harriet.Db.User_Reference;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Harriet.Color.Harriet_Color;
      Setup       : Tropos.Configuration)
      return Harriet.Db.Faction_Reference;

   procedure Create_Factions
     (Faction_Config : Tropos.Configuration;
      Setup_Config   : Tropos.Configuration);

end Harriet.Factions.Create;
