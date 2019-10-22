with Harriet.Calendar;
with Harriet.Logging;
with Harriet.Quantities;

with Harriet.Commodities;
with Harriet.Worlds.Updates;

with Harriet.Db.Colony;
with Harriet.Db.Commodity;
with Harriet.Db.Facility;
with Harriet.Db.Input_Commodity;
with Harriet.Db.Installation;
with Harriet.Db.Manufactured;
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

   procedure Execute_Production
     (Colony     : Harriet.Db.Colony_Reference;
      Commodity  : Harriet.Db.Commodity_Reference;
      Available  : Natural);

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
         elsif Installation.Commodity /= Null_Commodity_Reference then
            Execute_Production
              (Colony     => Installation.Colony,
               Commodity  => Installation.Commodity,
               Available  =>
                 Natural
                   (Real'Truncation
                        (Installation.Efficiency * Real (Facility.Industry))));
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

   procedure Execute_Production
     (Colony     : Harriet.Db.Colony_Reference;
      Commodity  : Harriet.Db.Commodity_Reference;
      Available  : Natural)
   is
      use Harriet.Quantities;
      Manufacture : constant Harriet.Db.Manufactured_Reference :=
        Harriet.Db.Manufactured.Get_Manufactured
          (Commodity)
        .Get_Manufactured_Reference;
      Max_Production : Quantity_Type := To_Quantity (Real (Available));
      Stock          : constant Harriet.Db.Has_Stock_Reference :=
        Harriet.Db.Colony.Get (Colony).Get_Has_Stock_Reference;
   begin
      for Part of
        Harriet.Db.Input_Commodity.Select_By_Manufactured
          (Manufacture)
      loop
         declare
            Required : constant Quantity_Type :=
              Part.Quantity * Max_Production;
            Available : constant Quantity_Type :=
              Harriet.Commodities.Current_Quantity (Stock, Part.Commodity);
            This_Max  : constant Quantity_Type :=
              Available / Part.Quantity;
         begin
            Max_Production := Min (Max_Production, This_Max);
            Harriet.Logging.Log
              ("production",
               Harriet.Db.Commodity.Get (Part.Commodity).Tag
               & ": required " & Show (Required)
               & "; available " & Show (Available)
               & "; limit " & Show (This_Max)
               & "; new max " & Show (Max_Production));
         end;
      end loop;

      Harriet.Logging.Log
        (Category => "production",
         Message  =>
           "industry on "
         & Harriet.Db.World.Get
           (Harriet.Db.Colony.Get (Colony).World).Name
         & " produces "
         & Show (Max_Production) & " "
         & Harriet.Db.Commodity.Get (Commodity).Tag);

      for Part of
        Harriet.Db.Input_Commodity.Select_By_Manufactured
          (Manufacture)
      loop
         Harriet.Commodities.Remove_Stock
           (Stock, Part.Commodity, Part.Quantity * Max_Production);
      end loop;

      Harriet.Commodities.Add_Stock (Stock, Commodity, Max_Production);

   end Execute_Production;

end Harriet.Installations.Updates;
