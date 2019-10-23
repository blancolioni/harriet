with Ada.Text_IO;

with Harriet.Real_Images;

with Harriet.Ships;

with Harriet.Db.Component;
with Harriet.Db.Design_Module;
with Harriet.Db.Ship_Design;

with Harriet.Db.Energy_Weapon;
with Harriet.Db.Engine;
with Harriet.Db.Generator;
with Harriet.Db.Hold;
with Harriet.Db.Jump_Drive;
with Harriet.Db.Missile_Launcher;
with Harriet.Db.Scanner;
with Harriet.Db.Shield_Generator;
with Harriet.Db.Tank;

package body Harriet.Configure.Ships is

   type Component_Class is
     (Engine, Jump_Drive, Tank, Hold, Generator,
      Scanner, Sensor, Cloak,
      Shield_Generator,
      Energy_Weapon, Missile_Launcher);

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
      is (Component_Config.Get (Name, 0.0));

      function Get (Name : String) return Natural
      is (Component_Config.Get (Name, 0));

      Class : constant Component_Class :=
        Component_Class'Value
          (Component_Config.Get ("class"));

      Tag : constant String := Component_Config.Config_Name;
      Enabled_By : constant Harriet.Db.Technology_Reference :=
        Harriet.Db.Null_Technology_Reference;
      Crew : constant Natural := Get ("crew");
      Mass : constant Non_Negative_Real := Get ("mass");
      Power : constant Non_Negative_Real := Get ("power");

   begin
      case Class is
         when Engine =>
            Harriet.Db.Engine.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Impulse    => Get ("impulse"));

         when Jump_Drive =>
            Harriet.Db.Jump_Drive.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Jump       => Get ("jump"));

         when Tank =>
            Harriet.Db.Tank.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Capacity   => Get ("fuel"));

         when Hold =>
            Harriet.Db.Hold.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Capacity   => Get ("cargo"));

         when Generator =>
            Harriet.Db.Generator.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By);

         when Scanner =>
            Harriet.Db.Scanner.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Scan       =>  Get ("scan"));

         when Sensor =>
            null;

         when Cloak =>
            null;

         when Shield_Generator =>
            Harriet.Db.Shield_Generator.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Shields    => Get ("shield"));

         when Energy_Weapon =>
            Harriet.Db.Energy_Weapon.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Energy     =>  Get ("energy"));

         when Missile_Launcher =>
            Harriet.Db.Missile_Launcher.Create
              (Crew       => Crew,
               Mass       => Mass,
               Power      => Power,
               Tag        => Tag,
               Enabled_By => Enabled_By,
               Launchers  => Get ("launcher"));

      end case;

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
                    Default_Manager =>
                      Design_Config.Get ("default-manager", "ship-default"));
   begin
      for Component_Config of Design_Config.Child ("components") loop
         declare
            use Harriet.Db;
            Component : constant Harriet.Db.Component_Reference :=
                          Harriet.Db.Component.Get_Reference_By_Tag
                            (Component_Config.Config_Name);
            Count     : constant Positive :=
                          (if Component_Config.Child_Count = 1
                           then Component_Config.Value
                           else 1);
         begin
            if Component = Null_Component_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in design '" & Design_Config.Config_Name
                  & "': no such component: "
                  & Component_Config.Config_Name);
            else
               for I in 1 .. Count loop
                  Harriet.Db.Design_Module.Create
                    (Ship_Design => Design,
                     Component   => Component);
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
      begin
         Ada.Text_IO.Put_Line
           ("Name:          " & Design_Config.Config_Name);
         Ada.Text_IO.Put_Line
           ("Mass (empty):  "
            & Harriet.Real_Images.Approximate_Image
              (Empty_Mass)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (fueled): "
            & Harriet.Real_Images.Approximate_Image
              (Empty_Mass + Fuel_Mass)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (loaded): "
            & Harriet.Real_Images.Approximate_Image
              (Empty_Mass + Fuel_Mass + Cargo_Volume)
            & "t");
         Ada.Text_IO.Put_Line
           ("Max system speed (loaded): "
            & Harriet.Real_Images.Approximate_Image
              (Harriet.Ships.Design_Maximum_System_Speed (Design))
            & " AU/day");
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
