with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Db.Deposit;

package body Harriet.Worlds.Updates is

   -------------------
   -- Mine_Resource --
   -------------------

   procedure Mine_Resource
     (World         : Harriet.Db.World_Reference;
      Resource      : Harriet.Db.Resource_Reference;
      Effectiveness : Non_Negative_Real;
      Mined         : out Harriet.Quantities.Quantity_Type)
   is
      use Harriet.Db;
      use Harriet.Quantities;

      type Mined_Record is
         record
            Deposit       : Harriet.Db.Deposit_Reference;
            Concentration : Unit_Real;
            Available     : Quantity_Type;
            Mined         : Quantity_Type;
         end record;

      package Mined_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Mined_Record);

      List : Mined_Lists.List;

   begin

      if Resource = Null_Resource_Reference then
         Mined := Quantities.Zero;
         for Deposit of Harriet.Db.Deposit.Select_By_World (World) loop
            declare
               This_Mined : constant Quantity_Type :=
                 Scale (Deposit.Available,
                        Effectiveness * Deposit.Concentration
                        * 5.0e-5);
            begin
               List.Append
                 (Mined_Record'
                    (Deposit       => Deposit.Get_Deposit_Reference,
                     Concentration => Deposit.Concentration,
                     Available     => Deposit.Available,
                     Mined         => This_Mined));
               Mined := Mined + This_Mined;
            end;
         end loop;
      else
         Mined := Quantities.Zero;
         for Deposit of Harriet.Db.Deposit.Select_By_Deposit
           (World, Resource)
         loop
            declare
               This_Mined : constant Quantity_Type :=
                 Scale (Deposit.Available,
                        Effectiveness * Deposit.Concentration
                        * 1.0e-4);
            begin
               List.Append
                 (Mined_Record'
                    (Deposit       => Deposit.Get_Deposit_Reference,
                     Concentration => Deposit.Concentration,
                     Available     => Deposit.Available,
                     Mined         => This_Mined));
               Mined := Mined + This_Mined;
            end;
         end loop;
      end if;

      for Rec of List loop
         declare
            New_Available : constant Quantity_Type :=
                              Rec.Available - Rec.Mined;
            New_Concentration : constant Unit_Real :=
                                  Rec.Concentration
                                    * To_Real (New_Available)
                                  / To_Real (Rec.Available);
         begin
            Harriet.Db.Deposit.Update_Deposit (Rec.Deposit)
              .Set_Available (New_Available)
              .Set_Concentration (New_Concentration)
              .Done;
         end;
      end loop;
   end Mine_Resource;

end Harriet.Worlds.Updates;
