with Harriet.Db.Deposit;

package body Harriet.Worlds.Updates is

   -------------------
   -- Mine_Resource --
   -------------------

   procedure Mine_Resource
     (Sector        : Harriet.Db.World_Sector_Reference;
      Resource      : Harriet.Db.Resource_Reference;
      Effectiveness : Non_Negative_Real;
      Mined         : out Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Db;
      use Harriet.Quantities;

      Deposit : constant Harriet.Db.Deposit.Deposit_Type :=
                  Harriet.Db.Deposit.Get_By_World_Sector
                    (Sector);
   begin

      if Resource = Null_Resource_Reference then
         Mined := Quantities.To_Quantity (Effectiveness * 1000.0);
      else
         Mined :=
           Scale (Deposit.Available,
                  Effectiveness * Deposit.Concentration
                  * 1.0e-4);
         declare
            New_Available     : constant Quantity_Type :=
                                  Deposit.Available - Mined;
            New_Concentration : constant Unit_Real :=
                                  Deposit.Concentration
                                    * To_Real (New_Available)
                                  / To_Real (Deposit.Available);
         begin
            Harriet.Db.Deposit.Update_Deposit (Deposit.Get_Deposit_Reference)
              .Set_Available (Deposit.Available - Mined)
              .Set_Concentration (New_Concentration)
              .Done;
         end;
      end if;
   end Mine_Resource;

end Harriet.Worlds.Updates;
