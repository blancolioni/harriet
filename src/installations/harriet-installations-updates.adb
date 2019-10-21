with Harriet.Calendar;
with Harriet.Logging;
with Harriet.Quantities;

with Harriet.Commodities;
with Harriet.Worlds.Updates;

with Harriet.Db.Colony;
with Harriet.Db.Facility;
with Harriet.Db.Installation;
with Harriet.Db.Resource;
with Harriet.Db.World;

with Harriet.Updates.Events;

package body Harriet.Installations.Updates is

   type Installation_Update is
     new Harriet.Updates.Update_Interface with
      record
         Installation : Harriet.Db.Installation_Reference;
      end record;

   overriding procedure Activate
     (Update : Installation_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Installation_Update)
   is
      use Harriet.Db;
      Installation  : constant Harriet.Db.Installation.Installation_Type :=
                        Harriet.Db.Installation.Get (Update.Installation);
      Facility : constant Harriet.Db.Facility.Facility_Type :=
                   Harriet.Db.Facility.Get (Installation.Facility);
      World    : constant Harriet.Db.World.World_Type :=
                  Harriet.Db.World.Get (Installation.World);
      Colony   : constant Harriet.Db.Colony.Colony_Type :=
                   Harriet.Db.Colony.Get (Installation.Colony);
      Stock        : constant Harriet.Db.Has_Stock_Reference :=
        Colony.Get_Has_Stock_Reference;
      Quantity      : Harriet.Quantities.Quantity_Type;
   begin
      if Facility.Mining > 0
        and then Installation.Resource /= Null_Resource_Reference
      then
         Harriet.Worlds.Updates.Mine_Resource
           (World         => Installation.World,
            Resource      => Installation.Resource,
            Effectiveness =>
              Real (Facility.Mining) * Installation.Efficiency,
            Mined         => Quantity);

         Harriet.Logging.Log
           (Facility.Tag & " on " & World.Name,
            "mined " & Harriet.Quantities.Show (Quantity)
            & " " & Harriet.Db.Resource.Get (Installation.Resource).Tag);

         Harriet.Commodities.Add_Stock
           (To        => Stock,
            Commodity =>
              Harriet.Db.Resource.Get (Installation.Resource)
            .Get_Commodity_Reference,
            Quantity  => Quantity);
      end if;

      if Facility.Strip_Mining > 0 then
         Harriet.Worlds.Updates.Mine_Resource
           (World         => Installation.World,
            Resource      => Harriet.Db.Null_Resource_Reference,
            Effectiveness =>
              Real (Facility.Strip_Mining) * Installation.Efficiency,
            Mined         => Quantity);

         Harriet.Logging.Log
           (Facility.Tag & " on " & World.Name,
            "strip-mined " & Harriet.Quantities.Show (Quantity)
            & " raw resources");

         Harriet.Commodities.Add_Stock
           (To        => Stock,
            Commodity => Harriet.Commodities.Raw_Resources,
            Quantity  => Quantity);
      end if;

      if Facility.Industry > 0 then
         if Installation.Resource /= Null_Resource_Reference then
            declare
               use Harriet.Quantities;
               Converted : constant Quantity_Type :=
                 Min (To_Quantity (Real (Facility.Industry)),
                      Harriet.Commodities.Current_Quantity
                        (Stock, Harriet.Commodities.Raw_Resources));
            begin
               Harriet.Commodities.Remove_Stock
                 (Stock, Harriet.Commodities.Raw_Resources, Converted);
               Harriet.Commodities.Add_Stock
                 (Stock,
                  Harriet.Db.Resource.Get (Installation.Resource)
                  .Get_Commodity_Reference,
                  Scale (Converted, 0.1));
               Harriet.Logging.Log
                 (Facility.Tag & " on " & World.Name,
                  "converted " & Harriet.Quantities.Show (Converted)
                  & " raw resources to "
                  & Harriet.Quantities.Show (Scale (Converted, 0.1))
                  & " "
                  & Harriet.Db.Resource.Get (Installation.Resource).Tag);
            end;
         end if;
      end if;

      Harriet.Updates.Events.Update_With_Delay
        (Harriet.Calendar.Days (1), Update);
   end Activate;

   ------------------
   -- Daily_Update --
   ------------------

   function Daily_Update
     (Reference : Harriet.Db.Installation_Reference)
      return Harriet.Updates.Update_Interface'Class
   is
   begin
      return Installation_Update'(Installation => Reference);
   end Daily_Update;

end Harriet.Installations.Updates;
