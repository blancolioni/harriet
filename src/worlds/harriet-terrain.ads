with Harriet.Color;

with Harriet.Db;

package Harriet.Terrain is

   function Name
     (Terrain : Harriet.Db.Terrain_Reference)
      return String;

   function Color
     (Terrain : Harriet.Db.Terrain_Reference)
      return Harriet.Color.Harriet_Color;

   function Is_Water
     (Terrain : Harriet.Db.Terrain_Reference)
      return Boolean;

   function Hazard
     (Terrain : Harriet.Db.Terrain_Reference)
      return Unit_Real;

end Harriet.Terrain;
