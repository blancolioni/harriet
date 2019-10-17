with WL.Localisation;

with Harriet.Db.Terrain;

package body Harriet.Terrain is

   -----------
   -- Color --
   -----------

   function Color
     (Terrain : Harriet.Db.Terrain_Reference)
      return Harriet.Color.Harriet_Color
   is
      Rec : constant Harriet.Db.Terrain.Terrain_Type :=
              Harriet.Db.Terrain.Get (Terrain);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   ------------
   -- Hazard --
   ------------

   function Hazard
     (Terrain : Harriet.Db.Terrain_Reference)
      return Unit_Real
   is (Harriet.Db.Terrain.Get (Terrain).Hazard);

   ----------
   -- Name --
   ----------

   function Name
     (Terrain : Harriet.Db.Terrain_Reference)
      return String
   is (WL.Localisation.Local_Text
       (Harriet.Db.Terrain.Get (Terrain).Tag));

   --------------
   -- Is_Water --
   --------------

   function Is_Water
     (Terrain : Harriet.Db.Terrain_Reference)
      return Boolean
   is (Harriet.Db.Terrain.Get (Terrain).Is_Water);

end Harriet.Terrain;
