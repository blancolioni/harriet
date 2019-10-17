with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Quantities;
with Harriet.Random;
with Harriet.Real_Images;

with Harriet.Orbits;

with Harriet.Worlds;

with Harriet.Db.Commodity;
with Harriet.Db.Container_Component;
with Harriet.Db.Drive_Component;
with Harriet.Db.Ship_Component;
with Harriet.Db.Ship_Design;
with Harriet.Db.Ship_Module;
with Harriet.Db.Ship_Module_Design;
with Harriet.Db.World;

package body Harriet.Ships is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Owner   : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Design  : Harriet.Db.Ship_Design_Reference;
      Manager : String;
      Name    : String)
   is
      use type Harriet.Calendar.Time;
      World_Rec : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (World);
      Orbit     : constant Long_Float :=
        World_Rec.Radius
          + 1000.0 * (3000.0 + 1000.0 * Harriet.Random.Unit_Random);
      M : constant Real := World_Rec.Mass;
      Period    : constant Long_Float := Harriet.Orbits.Period (M, Orbit);
      Ship         : constant Harriet.Db.Ship_Reference :=
                       Harriet.Db.Ship.Create
                         (Name              => Name,
                          Capacity          =>
                            Harriet.Db.Ship_Design.Get (Design).Hold_Size,
                          Faction           => Owner,
                          Updates_Started   => True,
                          Next_Update       => Harriet.Calendar.Clock,
                          Active            => True,
                          Scheduled         => False,
                          Next_Event        => Harriet.Calendar.Clock,
                          Manager           => Manager,
                          World             => World,
                          Star_System       => World_Rec.Star_System,
                          Orbit             => Orbit,
                          Inclination       =>
                            Harriet.Random.Unit_Random * 10.0 - 5.0,
                          Epoch             =>
                            Harriet.Calendar.Clock
                          - Duration (Period * Harriet.Random.Unit_Random),
                          Ship_Design       => Design,
                          Alive             => True,
                          Training          => 0.0,
                          Fuel              => 0.0,
                          Departure         => Harriet.Calendar.Clock,
                          Arrival           => Harriet.Calendar.Clock,
                          Dest_Orbit        => 0.0,
                          Dest_Incl         => 0.0,
                          Dest_Epoch        => Harriet.Calendar.Clock,
                          Final_Destination => Harriet.Db.Null_World_Reference,
                          Current_Order     => 0,
                          Cycle_Orders      => False);
   begin
      for Design_Component of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design
          (Design)
      loop
         Harriet.Db.Ship_Module.Create
           (Ship               => Ship,
            Ship_Module_Design =>
              Design_Component.Get_Ship_Module_Design_Reference,
            Crew               => 0,
            Condition          => 0.0);
      end loop;

      if True then
         declare
            S : constant Ship_Type := Get (Ship);
         begin
            Ada.Text_IO.Put_Line
              (S.Name & " in orbit above "
               & Harriet.Worlds.Name (S.World)
               & ": alt"
               & Natural'Image
                 (Natural
                      ((S.Orbit - Harriet.Worlds.Radius (S.World)) / 1_000.0))
               & "km incl "
               & Harriet.Real_Images.Approximate_Image
                 (S.Inclination)
               & " deg prd "
               & Harriet.Real_Images.Approximate_Image
                 (Harriet.Orbits.Period
                      (Harriet.Worlds.Mass (S.World), S.Orbit) / 60.0)
               & "m posn "
               & Harriet.Real_Images.Approximate_Image
                 (S.Current_Longitude)
               & " E");
         end;
      end if;
   end Create_Ship;

   -----------------------
   -- Current_Longitude --
   -----------------------

   function Current_Longitude
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
      use Harriet.Calendar;
      Orbit       : constant Non_Negative_Real := Ship.Orbit;
      World_Mass  : constant Non_Negative_Real :=
                     Harriet.Worlds.Mass (Ship.World);
      Period      : constant Non_Negative_Real :=
        Harriet.Orbits.Period (World_Mass, Orbit);
      Epoch       : constant Time :=
                     Harriet.Db.Ship.Get (Ship.Reference).Epoch;
      Now         : constant Time := Clock;
      Elapsed     : constant Duration := Now - Epoch;
      Orbit_Count : constant Non_Negative_Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
                      Orbit_Count - Real'Truncation (Orbit_Count);
      Longitude   : Non_Negative_Real := Partial * 360.0;
   begin
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      return Longitude;
   end Current_Longitude;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Dry_Mass;
   end Current_Mass;

   -------------------------
   -- Design_Cargo_Volume --
   -------------------------

   function Design_Cargo_Volume
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
   begin
      return Volume : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
         loop
            declare
               use Harriet.Db, Harriet.Db.Ship_Component;
               Component : constant Ship_Component_Type :=
                             Get (Module.Ship_Component);
            begin
               if Component.Top_Record = R_Container_Component then
                  declare
                     use Harriet.Db.Container_Component;
                     Container : constant Container_Component_Type :=
                       Get_Container_Component
                         (Component.Get_Ship_Component_Reference);
                  begin
                     if not Container.Tank then
                        Volume := Volume
                          + Harriet.Quantities.To_Real
                          (Container.Capacity);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end return;
   end Design_Cargo_Volume;

   --------------------
   -- Design_Delta_V --
   --------------------

   function Design_Delta_V
     (Design     : Harriet.Db.Ship_Design_Reference;
      Cargo_Mass : Non_Negative_Real) return Non_Negative_Real
   is
      Ve : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db, Harriet.Db.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Drive_Component then
               declare
                  use Harriet.Db.Drive_Component;
                  Drive : constant Drive_Component_Type :=
                                Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  Ve := Drive.Exhaust_Velocity;
                  exit;
               end;
            end if;
         end;
      end loop;

      declare
         use Harriet.Elementary_Functions;
         Dry_Mass : constant Non_Negative_Real :=
                      Design_Mass (Design) + Cargo_Mass;
         Fuel_Mass : constant Non_Negative_Real := Design_Fuel_Mass (Design);
         Full_Mass : constant Non_Negative_Real := Dry_Mass + Fuel_Mass;
      begin
         return Ve * Log (Full_Mass / Dry_Mass);
      end;
   end Design_Delta_V;

   ----------------------
   -- Design_Fuel_Mass --
   ----------------------

   function Design_Fuel_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real
   is
      type Consumption_Record is
         record
            Commodity       : Harriet.Db.Commodity_Reference;
            Liquid          : Boolean;
            Cryo            : Boolean;
            Density         : Non_Negative_Real;
            Kg_Per_Second   : Non_Negative_Real;
            Relative_Mass   : Non_Negative_Real;
            Relative_Volume : Non_Negative_Real;
         end record;

      package Consumption_Record_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Consumption_Record);

      List : Consumption_Record_Lists.List;

      procedure Update_Fuel
        (Fuel          : Harriet.Db.Commodity_Reference;
         Kg_Per_Second : Non_Negative_Real);

      -----------------
      -- Update_Fuel --
      -----------------

      procedure Update_Fuel
        (Fuel          : Harriet.Db.Commodity_Reference;
         Kg_Per_Second : Non_Negative_Real)
      is
         use Harriet.Db;
         Rec : constant Harriet.Db.Commodity.Commodity_Type :=
                 Commodity.Get (Fuel);
      begin
         for Item of List loop
            if Item.Commodity = Fuel then
               Item.Kg_Per_Second :=
                 Item.Kg_Per_Second + Kg_Per_Second;
               return;
            end if;
         end loop;
         List.Append
           (Consumption_Record'
              (Commodity       => Fuel,
               Liquid          => True,
               Cryo            => True,
               Density         => Rec.Density,
               Kg_Per_Second   => Kg_Per_Second,
               Relative_Mass   => 0.0,
               Relative_Volume => 0.0));
      end Update_Fuel;

      Liquid_Tank_Size : Non_Negative_Real := 0.0;
      Cryo_Tank_Size   : Non_Negative_Real := 0.0;
      Liquid_Fuel_Mass : Non_Negative_Real := 0.0;
      Cryo_Fuel_Mass   : Non_Negative_Real := 0.0;
   begin
      for Module of
        Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Harriet.Db, Harriet.Db.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Drive_Component then
               declare
                  Drive : constant Drive_Component.Drive_Component_Type :=
                            Drive_Component.Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  if Drive.Fuel /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Fuel, Drive.Max_Fuel_Burn);
                  end if;

                  if Drive.Oxidiser /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Oxidiser, Drive.Max_Oxidiser_Burn);
                  end if;
               end;
            elsif Rec_Type = R_Container_Component then
               declare
                  use Harriet.Db.Container_Component;
                  Container : constant Container_Component_Type :=
                                Get_Container_Component
                                  (Module.Ship_Component);
               begin
                  if Container.Liquid then
                     if Container.Cryo then
                        Cryo_Tank_Size := Cryo_Tank_Size
                          + Harriet.Quantities.To_Real
                          (Container.Capacity);
                     else
                        Liquid_Tank_Size := Liquid_Tank_Size
                          + Harriet.Quantities.To_Real
                          (Container.Capacity);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      declare
         Total_Liquid_Mass_Consumption   : Non_Negative_Real := 0.0;
         Total_Cryo_Mass_Consumption     : Non_Negative_Real := 0.0;
         Total_Liquid_Volume_Consumption : Non_Negative_Real := 0.0;
         Total_Cryo_Volume_Consumption   : Non_Negative_Real := 0.0;
      begin
         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  Total_Cryo_Mass_Consumption :=
                    Total_Cryo_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Cryo_Volume_Consumption :=
                    Total_Cryo_Volume_Consumption
                    + Item.Kg_Per_Second / Item.Density;
               else
                  Total_Liquid_Mass_Consumption :=
                    Total_Liquid_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Liquid_Volume_Consumption :=
                    Total_Liquid_Volume_Consumption +
                      Item.Kg_Per_Second / Item.Density;
               end if;
            end if;
         end loop;

         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  declare
                     use Harriet.Real_Images;
                     function Img (X : Real) return String
                                   renames Approximate_Image;

                     This_Vol_Per_Second : constant Non_Negative_Real :=
                                             Item.Kg_Per_Second
                                               / Item.Density;
                     Partial_Volume      : constant Non_Negative_Real :=
                                             This_Vol_Per_Second
                                               / Total_Cryo_Volume_Consumption;
                     Required_Volume     : constant Non_Negative_Real :=
                                             Partial_Volume
                                               * Cryo_Tank_Size;
                     Required_Mass       : constant Non_Negative_Real :=
                                             Required_Volume
                                               * Item.Density;
                  begin
                     if False then
                        Ada.Text_IO.Put_Line
                          (Harriet.Db.Commodity.Get (Item.Commodity).Tag
                           & ": density: "
                           & Img (Item.Density)
                           & "kg/m3; consumption: "
                           & Img (Item.Kg_Per_Second)
                           & "kg/s; vol. consumption: "
                           & Img (This_Vol_Per_Second)
                           & "m3/s; partial vol: "
                           & Img (Partial_Volume)
                           & " total vol: "
                           & Img (Required_Volume)
                           & " total mass: "
                           & Img (Required_Mass));
                     end if;

                     Cryo_Fuel_Mass := Cryo_Fuel_Mass + Required_Mass;
                  end;
               else
                  Liquid_Fuel_Mass :=
                    Liquid_Fuel_Mass
                      + Item.Kg_Per_Second / Item.Density
                    * Liquid_Tank_Size;
               end if;
            end if;
         end loop;

         if False then
            Ada.Text_IO.Put_Line
              ("cryo tank size "
               & Harriet.Real_Images.Approximate_Image (Cryo_Tank_Size)
               & "; cryo fuel mass "
               & Harriet.Real_Images.Approximate_Image (Cryo_Fuel_Mass));
         end if;

         return Cryo_Fuel_Mass + Liquid_Fuel_Mass;

      end;

   end Design_Fuel_Mass;

   -----------------
   -- Design_Mass --
   -----------------

   function Design_Mass
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
         loop
            Mass := Mass +
              Harriet.Db.Ship_Component.Get (Module.Ship_Component).Mass;
         end loop;
      end return;
   end Design_Mass;

   -------------------
   -- Design_Thrust --
   -------------------

   function Design_Thrust
     (Design : Harriet.Db.Ship_Design_Reference) return Non_Negative_Real
   is
   begin
      return Thrust : Non_Negative_Real := 0.0 do
         for Module of
           Harriet.Db.Ship_Module_Design.Select_By_Ship_Design (Design)
         loop
            declare
               use Harriet.Db, Harriet.Db.Ship_Component;
               Component : constant Ship_Component_Type :=
                 Get (Module.Ship_Component);
            begin
               if Component.Top_Record = R_Drive_Component then
                  declare
                     use Harriet.Db.Drive_Component;
                     Drive : constant Drive_Component_Type :=
                       Get_Drive_Component
                         (Component.Get_Ship_Component_Reference);
                  begin
                     Thrust := Thrust + Drive.Maximum_Thrust;
                  end;
               end if;
            end;
         end loop;
      end return;
   end Design_Thrust;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Mass
        (Harriet.Db.Ship.Get (Ship.Reference).Ship_Design);
   end Dry_Mass;

   ------------------
   -- Total_Thrust --
   ------------------

   function Total_Thrust
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Thrust
        (Harriet.Db.Ship.Get (Ship.Reference).Ship_Design);
   end Total_Thrust;

end Harriet.Ships;
