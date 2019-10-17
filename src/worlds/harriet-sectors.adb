with Harriet.Db.World_Sector;

package body Harriet.Sectors is

   -------------------------
   -- Has_Stock_Reference --
   -------------------------

   function Has_Stock_Reference
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Has_Stock_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Get_Has_Stock_Reference;
   end Has_Stock_Reference;

end Harriet.Sectors;
