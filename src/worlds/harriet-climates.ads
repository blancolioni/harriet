with Harriet.Db;

package Harriet.Climates is

   function Airless return Harriet.Db.Climate_Reference;
   function Iceball return Harriet.Db.Climate_Reference;
   function Jovian return Harriet.Db.Climate_Reference;
   function Martian return Harriet.Db.Climate_Reference;
   function Temperate return Harriet.Db.Climate_Reference;
   function Venusian return Harriet.Db.Climate_Reference;
   function Water return Harriet.Db.Climate_Reference;

   function Name
     (Climate : Harriet.Db.Climate_Reference)
      return String;

   function Habitability
     (Climate : Harriet.Db.Climate_Reference)
      return Unit_Real;

   function Default_Terrain
     (Climate : Harriet.Db.Climate_Reference)
      return Harriet.Db.Terrain_Reference;

end Harriet.Climates;
