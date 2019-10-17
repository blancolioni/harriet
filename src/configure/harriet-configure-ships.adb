with Ada.Text_IO;

with Harriet.Constants;
with Harriet.Quantities;
with Harriet.Real_Images;
with Harriet.Solar_System;

with Harriet.Commodities;
with Harriet.Ships;

with Harriet.Db.Commodity;
with Harriet.Db.Container_Component;
with Harriet.Db.Drive_Component;
with Harriet.Db.Input_Commodity;
with Harriet.Db.Manufactured;
with Harriet.Db.Power_Component;

with Harriet.Db.Attachment;
with Harriet.Db.Ship_Component;
with Harriet.Db.Ship_Design;
with Harriet.Db.Ship_Module_Design;

package body Harriet.Configure.Ships is

   procedure Configure_Component
     (Component_Config : Tropos.Configuration);

   procedure Configure_Design
     (Design_Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Component_Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Real (Float'(Component_Config.Get (Name, 0.0))));

      function Get
        (Child   : String;
         Name    : String;
         Default : Real := 0.0)
         return Real
      is (Real (Float'(Component_Config.Child (Child).Get
                       (Name, Float (Default)))));

      function Get
        (Child : String;
         Index : Positive)
         return Real
      is (if Component_Config.Child (Child).Child_Count >= Index
          then Real (Float'(Component_Config.Child (Child).Get (Index)))
          else 0.0);

      function Get
        (Child : String;
         Name  : String)
         return Boolean
      is (Component_Config.Child (Child).Get (Name));

      function Get
        (Name  : String)
         return Boolean
      is (Component_Config.Get (Name));

      Tag : constant String :=
              Component_Config.Config_Name;
      Mass : constant Non_Negative_Real := Get ("mass");
      Idle_Power : constant Non_Negative_Real := Get ("idle-power");
      Max_Power  : constant Non_Negative_Real := Get ("max-power");
      Start_Power : constant Non_Negative_Real := Get ("start-power");
      Start_Duration : constant Non_Negative_Real := Get ("start-duration");
      Linear_Accel_Limit : constant Non_Negative_Real :=
                             Get ("acceleration-limit", "linear");
      Angular_Accel_Limit : constant Non_Negative_Real :=
                              Get ("acceleration-limit", "angular");
      Normal_Temperature  : constant Non_Negative_Real :=
                              Get ("temperature-limit", "normal")
                              + Harriet.Constants.Freezing_Point_Of_Water;
      Fail_Temperature : constant Non_Negative_Real :=
                           Get ("temperature-limit", "failure")
                           + Harriet.Constants.Freezing_Point_Of_Water;
      Explosion_Power  : constant Non_Negative_Real :=
                              Get ("explosion");
      Throttled           : constant Boolean := Get ("throttle")
        or else Component_Config.Contains ("throttled");
      Min_Throttle        : constant Real := Get ("throttle", "min");
      Max_Throttle        : constant Real := Get ("throttle", "max", 1.0);
      Operating_Heat      : constant Real := Get ("heat", "operating");
      Heat_Sink           : constant Real := Get ("heat", "sink");
      X1 : constant Real := Get ("bounding-box", 1);
      X2 : constant Real := Get ("bounding-box", 2);
      Y1 : constant Real := Get ("bounding-box", 3);
      Y2 : constant Real := Get ("bounding-box", 4);
      Z1 : constant Real := Get ("bounding-box", 5);
      Z2 : constant Real := Get ("bounding-box", 6);
      V  : constant Real := abs (X2 - X1) * abs (Y2 - Y1) * abs (Z2 - Z1);

   begin
      if Component_Config.Contains ("cargo") then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Harriet.Db.Container_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Start_Power         => Start_Power,
            Start_Duration      => Start_Duration,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Harriet.Db.Null_Technology_Reference,
            Mass                => Mass,
            Density             => Mass / V,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Capacity            =>
              Harriet.Quantities.To_Quantity
                (Get ("cargo", "volume")),
            Tank                => Component_Config.Get ("tank", False),
            Liquid              => Get ("cargo", "liquid"),
            Gas                 => Get ("cargo", "gas"),
            Cryo                => Get ("cargo", "cryo"));

      elsif Component_Config.Contains ("thrust") then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Harriet.Db.Drive_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Start_Power         => Start_Power,
            Start_Duration      => Start_Duration,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Harriet.Db.Null_Technology_Reference,
            Mass                => Mass,
            Density             => Mass / V,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Minimum_Thrust      => Get ("thrust", "minimum"),
            Maximum_Thrust      => Get ("thrust", "maximum"),
            Fuel                =>
              Harriet.Db.Commodity.Get_Reference_By_Tag
                (Component_Config.Get ("fuel", "")),
            Oxidiser            =>
              Harriet.Db.Commodity.Get_Reference_By_Tag
                (Component_Config.Get ("oxidiser", "")),
            Max_Fuel_Burn       => Get ("max_fuel_burn"),
            Max_Oxidiser_Burn   => Get ("max_oxidiser_burn"),
            Fuel_Propellant     => Component_Config.Get ("propellent-fuel"),
            Propellant          =>
              Harriet.Db.Commodity.Get_Reference_By_Tag
                (Component_Config.Get ("propellent", "")),
            Exhaust_Velocity    => Get ("ve"));
      elsif Component_Config.Get ("class", "") = "power" then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Harriet.Db.Power_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Start_Power         => Start_Power,
            Start_Duration      => Start_Duration,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Harriet.Db.Null_Technology_Reference,
            Mass                => Mass,
            Density             => Mass / V,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Power_Output        => Get ("generate", "power"));
      end if;

      declare
         Component : constant Harriet.Db.Ship_Component_Reference :=
                       Harriet.Db.Ship_Component.Get_Reference_By_Tag
                         (Component_Config.Config_Name);
      begin
         for Attach_Config of Component_Config.Child ("attachments") loop
            Harriet.Db.Attachment.Create
              (Ship_Component => Component,
               Position_X     => Attach_Config.Get (1),
               Position_Y     => Attach_Config.Get (2),
               Position_Z     => Attach_Config.Get (3));
         end loop;
      end;

      declare
         Manufactured : constant Harriet.Db.Manufactured_Reference :=
           Harriet.Db.Manufactured.Get_Reference_By_Tag
             (Component_Config.Config_Name);
      begin
         for Part_Config of Component_Config.Child ("parts") loop
            declare
               Tag : constant String := Part_Config.Config_Name;
               Quantity : constant Harriet.Quantities.Quantity_Type :=
                 Harriet.Quantities.To_Quantity (Part_Config.Value);
               Commodity : constant Harriet.Db.Commodity_Reference :=
                 (if Harriet.Commodities.Exists (Tag)
                  then Harriet.Commodities.Get (Tag)
                  else raise Constraint_Error with
                    "in configuraton for "
                  & Component_Config.Config_Name
                  & ": no such part: "
                  & Tag);
            begin
               Harriet.Db.Input_Commodity.Create
                 (Manufactured => Manufactured,
                  Commodity    => Commodity,
                  Quantity     => Quantity);
            end;
         end loop;
      end;

   end Configure_Component;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Design_Config : Tropos.Configuration)
   is
      Design : constant Harriet.Db.Ship_Design_Reference :=
                 Harriet.Db.Ship_Design.Create
                   (Name      => Design_Config.Config_Name,
                    Hold_Size => Harriet.Quantities.Zero,
                    Default_Manager =>
                      Design_Config.Get ("default-manager", "ship-default"));
   begin
      for Component_Config of Design_Config.Child ("components") loop
         declare
            use Harriet.Db;
            Component : constant Harriet.Db.Ship_Component_Reference :=
                          Harriet.Db.Ship_Component.Get_Reference_By_Tag
                            (Component_Config.Config_Name);
            Count     : constant Positive :=
                          (if Component_Config.Child_Count = 1
                           then Component_Config.Value
                           else 1);
         begin
            if Component = Null_Ship_Component_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in design '" & Design_Config.Config_Name
                  & "': no such component: "
                  & Component_Config.Config_Name);
            else
               for I in 1 .. Count loop
                  Harriet.Db.Ship_Module_Design.Create
                    (Ship_Design    => Design,
                     Ship_Component => Component,
                     Position_X     => 0.0,
                     Position_Y     => 0.0,
                     Position_Z     => 0.0);
               end loop;
            end if;
         end;
      end loop;

      declare
         Empty_Mass : constant Non_Negative_Real :=
                        Harriet.Ships.Design_Mass (Design);
         Fuel_Mass  : constant Non_Negative_Real :=
                        Harriet.Ships.Design_Fuel_Mass (Design);
         Cargo_Volume : constant Non_Negative_Real :=
                          Harriet.Ships.Design_Cargo_Volume (Design);
         Thrust     : constant Non_Negative_Real :=
                        Harriet.Ships.Design_Thrust (Design);
      begin
         Ada.Text_IO.Put_Line
           ("Name:          " & Design_Config.Config_Name);
         Ada.Text_IO.Put_Line
           ("Mass (empty):  "
            & Harriet.Real_Images.Approximate_Image
              (Empty_Mass / 1000.0)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (fueled): "
            & Harriet.Real_Images.Approximate_Image
              ((Empty_Mass + Fuel_Mass) / 1000.0)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (loaded): "
            & Harriet.Real_Images.Approximate_Image
              ((Empty_Mass + Fuel_Mass) / 1000.0 + Cargo_Volume)
            & "t");
         Ada.Text_IO.Put_Line
           ("Accel (fueled): "
            & Harriet.Real_Images.Approximate_Image
              (Thrust / (Empty_Mass + Fuel_Mass)
               / Harriet.Solar_System.Earth_Gravity)
            & "g");
         Ada.Text_IO.Put_Line
           ("Accel (loaded): "
            & Harriet.Real_Images.Approximate_Image
              (Thrust / (Empty_Mass + Fuel_Mass + Cargo_Volume * 1000.0)
               / Harriet.Solar_System.Earth_Gravity)
            & "g");
         Ada.Text_IO.Put_Line
           ("Delta-v (loaded): "
            & Harriet.Real_Images.Approximate_Image
              (Harriet.Ships.Design_Delta_V (Design, Cargo_Volume * 1000.0))
            & "m/s");

      end;
   end Configure_Design;

   -------------------------------
   -- Configure_Ship_Components --
   -------------------------------

   procedure Configure_Ship_Components
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "ships/components",
         File_Class_Name => "component",
         Process         => Configure_Component'Access);
   end Configure_Ship_Components;

   ----------------------------
   -- Configure_Ship_Designs --
   ----------------------------

   procedure Configure_Ship_Designs
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "ships/designs",
         File_Class_Name => "design",
         Process         => Configure_Design'Access);
   end Configure_Ship_Designs;

end Harriet.Configure.Ships;
