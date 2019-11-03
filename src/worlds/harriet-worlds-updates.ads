with Harriet.Quantities;

package Harriet.Worlds.Updates is

   procedure Mine_Resource
     (Sector        : Harriet.Db.World_Sector_Reference;
      Resource      : Harriet.Db.Resource_Reference;
      Effectiveness : Non_Negative_Real;
      Mined         : out Harriet.Quantities.Quantity_Type);

end Harriet.Worlds.Updates;
