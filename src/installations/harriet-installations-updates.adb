with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Calendar;
with Harriet.Logging;
with Harriet.Quantities;
with Harriet.Real_Images;

with Harriet.Commodities;
with Harriet.Worlds.Updates;

with Harriet.Db.Colony;
with Harriet.Db.Commodity;
with Harriet.Db.Facility;
with Harriet.Db.Input_Commodity;
with Harriet.Db.Installation;
with Harriet.Db.Manufactured;
with Harriet.Db.Production_Goal;
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

   function Execute_Production
     (Colony    : Harriet.Db.Colony_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Max       : Harriet.Quantities.Quantity_Type)
      return Harriet.Quantities.Quantity_Type;

   function Execute_Resource_Conversion
     (Colony     : Harriet.Db.Colony_Reference;
      Commodity  : Harriet.Db.Commodity_Reference;
      Max        : Harriet.Quantities.Quantity_Type)
      return Harriet.Quantities.Quantity_Type;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Installation_Update)
   is
      use Harriet.Db;
      use Harriet.Quantities;
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

      type Remaining_Quantity_Record is
         record
            Goal     : Production_Goal_Reference;
            Quantity : Quantity_Type;
         end record;

      package Remaining_Quantity_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Remaining_Quantity_Record);

      Goal_Remaining : Remaining_Quantity_Lists.List;

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

      if Facility.Industry > 0
        and then Installation.Efficiency > 0.0
      then

         declare
            Available_Capacity : constant Quantity_Type :=
              To_Quantity (Real (Facility.Industry)
                           * Installation.Efficiency);
            Remaining_Capacity : Quantity_Type :=
              Available_Capacity;
         begin
            for Production_Goal of
              Harriet.Db.Production_Goal
                .Select_Production_Priority_Bounded_By_Priority
                  (Installation.Colony, 1, 99)
            loop
               declare
                  Commodity : constant Commodity_Reference :=
                    Production_Goal.Commodity;
                  Wanted    : constant Quantity_Type :=
                    Production_Goal.Quantity;
                  Capacity  : constant Quantity_Type :=
                    Remaining_Capacity;
                  Max       : constant Quantity_Type :=
                    Min (Wanted, Capacity);
                  Converted     : constant Quantity_Type :=
                    (if Wanted = Zero then Zero
                     elsif Harriet.Commodities.Is_Resource (Commodity)
                     then Execute_Resource_Conversion
                       (Installation.Colony, Commodity, Max)
                     else Execute_Production
                       (Installation.Colony, Commodity, Max));
               begin
                  if Converted > Zero then
                     Remaining_Capacity := Remaining_Capacity - Converted;
                     Goal_Remaining.Append
                       ((Production_Goal.Get_Production_Goal_Reference,
                        Wanted - Converted));
                  end if;
               end;
            end loop;

            Harriet.Logging.Log
              ("installation"
               & Harriet.Db.To_String
                 (Installation.Get_Installation_Reference),
               "available " & Show (Available_Capacity)
               & "; remaining " & Show (Remaining_Capacity)
               & "; used " & Show (Available_Capacity - Remaining_Capacity)
               & "; utilitisation "
               & Harriet.Real_Images.Approximate_Image
                 (To_Real (Available_Capacity - Remaining_Capacity)
                  / To_Real (Available_Capacity)
                  * 100.0)
               & "%");

         end;
      end if;

      for Remaining of Goal_Remaining loop
         Harriet.Db.Production_Goal.Update_Production_Goal
           (Remaining.Goal)
           .Set_Quantity (Remaining.Quantity)
           .Done;
      end loop;

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

   ------------------------
   -- Execute_Production --
   ------------------------

   function Execute_Production
     (Colony    : Harriet.Db.Colony_Reference;
      Commodity : Harriet.Db.Commodity_Reference;
      Max       : Harriet.Quantities.Quantity_Type)
      return Harriet.Quantities.Quantity_Type
   is
      use Harriet.Quantities;
      Manufacture : constant Harriet.Db.Manufactured_Reference :=
        Harriet.Db.Manufactured.Get_Manufactured
          (Commodity)
        .Get_Manufactured_Reference;
      Max_Production : Quantity_Type := Max;
      Stock          : constant Harriet.Db.Has_Stock_Reference :=
        Harriet.Db.Colony.Get (Colony).Get_Has_Stock_Reference;
   begin
      for Part of
        Harriet.Db.Input_Commodity.Select_By_Manufactured
          (Manufacture)
      loop
         declare
--              Required : constant Quantity_Type :=
--                Part.Quantity * Max_Production;
            Available : constant Quantity_Type :=
              Harriet.Commodities.Current_Quantity (Stock, Part.Commodity);
            This_Max  : constant Quantity_Type :=
              Available / Part.Quantity;
         begin
            Max_Production := Min (Max_Production, This_Max);
--              Harriet.Logging.Log
--                ("production",
--                 Harriet.Db.Commodity.Get (Part.Commodity).Tag
--                 & ": required " & Show (Required)
--                 & "; available " & Show (Available)
--                 & "; limit " & Show (This_Max)
--                 & "; new max " & Show (Max_Production));
         end;
      end loop;
      if Max_Production > Zero then
         Harriet.Logging.Log
           (Category => "production",
            Message  =>
              "industry on "
            & Harriet.Db.World.Get
              (Harriet.Db.Colony.Get (Colony).World).Name
            & " produces "
            & Show (Max_Production)
            & "/"
            & Show (Max)
            & " "
            & Harriet.Db.Commodity.Get (Commodity).Tag);

         for Part of
           Harriet.Db.Input_Commodity.Select_By_Manufactured
             (Manufacture)
         loop
            Harriet.Commodities.Remove_Stock
              (Stock, Part.Commodity, Part.Quantity * Max_Production);
         end loop;

         Harriet.Commodities.Add_Stock (Stock, Commodity, Max_Production);
      end if;

      return Max_Production;

   end Execute_Production;

   ---------------------------------
   -- Execute_Resource_Conversion --
   ---------------------------------

   function Execute_Resource_Conversion
     (Colony     : Harriet.Db.Colony_Reference;
      Commodity  : Harriet.Db.Commodity_Reference;
      Max        : Harriet.Quantities.Quantity_Type)
      return Harriet.Quantities.Quantity_Type
   is
      use Harriet.Quantities;
      Stock          : constant Harriet.Db.Has_Stock_Reference :=
        Harriet.Db.Colony.Get (Colony).Get_Has_Stock_Reference;
      Raw_Resources  : constant Harriet.Db.Commodity_Reference :=
        Harriet.Commodities.Raw_Resources;
      Available      : constant Quantity_Type :=
        Harriet.Commodities.Current_Quantity (Stock, Raw_Resources);
      Max_Production : constant Quantity_Type :=
        Min (Max, Scale (Available, 0.1));
   begin

      if Max_Production > Zero then
         Harriet.Logging.Log
           (Category => "production",
            Message  =>
              "industry on "
            & Harriet.Db.World.Get
              (Harriet.Db.Colony.Get (Colony).World).Name
            & " converts "
            & Show (Scale (Max_Production, 10.0)) & " raw resources to "
            & Show (Max_Production) & " "
            & Harriet.Db.Commodity.Get (Commodity).Tag);

         Harriet.Commodities.Remove_Stock
           (Stock, Raw_Resources, Scale (Max_Production, 10.0));
         Harriet.Commodities.Add_Stock (Stock, Commodity, Max_Production);
      end if;

      return Max_Production;

   end Execute_Resource_Conversion;

end Harriet.Installations.Updates;
