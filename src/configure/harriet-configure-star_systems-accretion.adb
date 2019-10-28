with Ada.Characters.Handling;
with Ada.Numerics;
with Ada.Text_IO;
with Ada.Long_Float_Text_IO;

with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Random;
with Harriet.Roman_Images;

with Harriet.Constants;
with Harriet.Orbits;
with Harriet.Solar_System;

with Harriet.Configure.Resources;

with Harriet.Db.Atmosphere;
with Harriet.Db.Gas;
with Harriet.Db.Palette;
with Harriet.Db.Star;
with Harriet.Db.Star_System;
with Harriet.Db.World;

package body Harriet.Configure.Star_Systems is

   Verbose_Mode       : constant Boolean := False;

   subtype World_Update is Harriet.Db.World.World_Update;

   type Orbit_Zone is range 1 .. 3;

   function Get_Orbit_Zone
     (Luminosity   : Real;
      Orbit_Radius : Real)
     return Orbit_Zone;

   function Get_Orbital_Period
     (Separation  : Real;
      Small_Mass  : Real;
      Large_Mass  : Real)
      return Real;

   function Calculate_Kothari_Radius
     (Earth_Masses : Real;
      Jovian       : Boolean;
      Zone         : Orbit_Zone)
     return Real;
   --  Uses formula from stargen.
   --  Earth_Masses: mass of the World in terms of the mass of Earth
   --  Jovian: true if the World is a gas giant
   --  Zone: the orbit zone (1, 2 or 3; depends on the orbital radius
   --        and the luminosity of the star

   function Calculate_Escape_Velocity
     (Earth_Masses  : Real;
      Earth_Radii   : Real)
     return Real;
   --  mass and radius in terms of earth
   --  result in metres per second

   function Calculate_RMS_Velocity
     (Molecular_Weight : Real;
      Exospheric_Temp  : Real)
     return Real;

   procedure Calculate_Day_Length
     (Star    : in Harriet.Db.Star.Star_Type;
      World  : in out World_Update);

   function Calculate_Water_Boiling_Point
     (Surface_Pressure : Real)
      return Real;

   function Calculate_Gas_Life
     (World : World_Update;
      Gas    : Harriet.Db.Gas.Gas_Type)
      return Real;
   pragma Unreferenced (Calculate_Gas_Life);

   procedure Calculate_Gases
     (Star    : Harriet.Db.Star.Star_Type;
      World  : in out World_Update);

   function Calculate_Opacity
     (Molecular_Weight : Real;
      Surface_Pressure : Real)
      return Real;

   function Calculate_Greenhouse_Rise
     (Optical_Depth    : Real;
      Effective_Temp   : Real;
      Surface_Pressure : Real)
      return Real;

   function Estimated_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real;

   function Effective_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real;

   procedure Calculate_Surface_Temperature
     (Star        : in     Harriet.Db.Star.Star_Type;
      World      : in out World_Update;
      First       : in     Boolean;
      Ecosphere   : in     Real;
      Last_Water  : in     Real;
      Last_Clouds : in     Real;
      Last_Ice    : in     Real;
      Last_Temp   : in     Real;
      Last_Albedo : in     Real);

   function Calculate_Hydro_Fraction
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real)
      return Real;

   function Calculate_Cloud_Fraction
     (Surface_Temperature  : Real;
      Min_Molecular_Weight : Real;
      Earth_Radii          : Real;
      Hydro_Fraction       : Real)
      return Real;

   function Calculate_Ice_Fraction
     (Hydro_Fraction      : Real;
      Surface_Temperature : Real)
      return Real;

   function Calculate_Albedo
     (Hydro_Fraction : Real;
      Cloud_Fraction : Real;
      Ice_Fraction   : Real;
      Surface_Pressure : Real)
      return Real;

   function Has_Greenhouse
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real)
     return Boolean;

   function Molecule_Limit
     (Earth_Masses    : Real;
      Earth_Radii     : Real;
      Exospheric_Temp : Real)
     return Real;

   function Volatile_Inventory
     (Earth_Masses    : Real;
      Escape_Velocity : Real;
      RMS_Velocity    : Real;
      Stellar_Mass    : Real;
      Zone            : Orbit_Zone;
      Greenhouse      : Boolean;
      Accreted_Gas    : Boolean)
     return Real;

   function Volatile_Inventory
     (Star     : Harriet.Db.Star.Star_Type;
      World   : World_Update)
      return Real;

   function Calculate_Surface_Pressure
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real;
      Earth_Gravities        : Real)
     return Real;

   procedure Iterate_Surface_Temperature
     (Star    : in     Harriet.Db.Star.Star_Type;
      World  : in out World_Update);

   procedure Set_Temperature_Range
     (World : in out World_Update);

   procedure Set_Axial_Tilt
     (World : in out World_Update);

   function Soft_Limit (V, Max, Min : Real) return Real;

   function Limit (X : Real) return Real;

   ----------------------
   -- Calculate_Albedo --
   ----------------------

   function Calculate_Albedo
     (Hydro_Fraction : Real;
      Cloud_Fraction : Real;
      Ice_Fraction   : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Harriet.Solar_System;
      Rock             : Real := 1.0 - Hydro_Fraction - Ice_Fraction;
      Water            : Real := Hydro_Fraction;
      Ice              : Real := Ice_Fraction;
      Cloud_Adjustment : Real;
      Components       : Real := 0.0;
      Ice_Part, Water_Part, Cloud_Part, Rock_Part : Real;
   begin

      if Water > 0.0 then
         Components := Components + 1.0;
      end if;

      if Ice > 0.0 then
         Components := Components + 1.0;
      end if;

      if Rock > 0.0 then
         Components := Components + 1.0;
      end if;

      Cloud_Adjustment := Cloud_Fraction / Components;

      Rock  := Real'Max (Rock - Cloud_Adjustment, 0.0);
      Water := Real'Max (Water - Cloud_Adjustment, 0.0);
      Ice   := Real'Max (Ice - Cloud_Adjustment, 0.0);

      Cloud_Part := Cloud_Fraction * Cloud_Albedo;

      if Surface_Pressure = 0.0 then
         Rock_Part := Rock * Rocky_Airless_Albedo;
         Ice_Part := Ice * Airless_Ice_Albedo;
         Water_Part := 0.0;
      else
         Rock_Part := Rock * Rocky_Albedo;
         Ice_Part := Ice * Ice_Albedo;
         Water_Part := Water * Water_Albedo;
      end if;

      return Cloud_Part + Ice_Part + Water_Part + Rock_Part;
   end Calculate_Albedo;

   ------------------------------
   -- Calculate_Cloud_Fraction --
   ------------------------------

   function Calculate_Cloud_Fraction
     (Surface_Temperature  : Real;
      Min_Molecular_Weight : Real;
      Earth_Radii          : Real;
      Hydro_Fraction       : Real)
      return Real
   is
   begin

      if Min_Molecular_Weight > 18.0 then
         --  18.0 = molecular weight of water vapour
         return 0.0;
      end if;

      declare
         use Harriet.Elementary_Functions;
         use Harriet.Solar_System;
         Pi : constant := Ada.Numerics.Pi;
         Q2_36 : constant := 0.0698;
         Surface_Area : constant Real :=
                          4.0 * Pi * Earth_Radii ** 2;
         Hydro_Mass   : constant Real :=
                          Hydro_Fraction * Surface_Area
                            * Earth_Water_Mass_Per_Area;
         Water_Vapour_In_Kg : constant Real :=
                                1.0E-8 * Hydro_Mass
                                  * Exp (Q2_36
                                         * (Surface_Temperature
                                             - Earth_Average_Kelvin));
         Fraction           : constant Real :=
                                Cloud_Coverage_Factor
                                  * Water_Vapour_In_Kg
           / Surface_Area;
      begin
         return Real'Min (Fraction, 1.0);
      end;

   end Calculate_Cloud_Fraction;

   --------------------------
   -- Calculate_Day_Length --
   --------------------------

   procedure Calculate_Day_Length
     (Star    : in Harriet.Db.Star.Star_Type;
      World  : in out World_Update)
   is
      use Harriet.Db;
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
      Pi : constant := Ada.Numerics.Pi;

      World_Mass   : constant Real := World.Mass;
      World_Radius : constant Real := World.Radius;
      World_Year   : constant Real := World.Period;
      Is_Jovian     : constant Boolean :=
                        World.Category in Sub_Jovian | Jovian;
      J             : constant Real := 1.46e-20;
      K2            : constant Real :=
                        (if Is_Jovian then 0.24 else 0.33);

      Base_Angular_V : constant Real :=
                         Sqrt (2.0 * J * World_Mass
                               / (K2 * World_Radius ** 2));
      Change_In_Angular_V : constant Real := Change_In_Earth_Angular_V
        * (World.Density / Earth_Density)
        * (World_Radius / Earth_Radius)
        * (Earth_Mass / World_Mass)
        * (Star.Mass / Solar_Mass) ** 2
        * (World.Semimajor_Axis / Earth_Orbit) ** (-6.0);

      Angular_V     : constant Real :=
                        Base_Angular_V
                          + Change_In_Angular_V * Star.Age;
      Stopped       : constant Boolean :=
                        Angular_V <= 0.0;
      World_Day    : constant Real :=
                        2.0 * Pi / Angular_V;

   begin

      World.Set_Resonant_Period (False);

      if Stopped or else World_Day > World_Year then

         if World.Eccentricity > 0.1 then
            declare
               E : constant Real := World.Eccentricity;
               Spin_Resonance_Factor : constant Real :=
                                         (1.0 - E) / (1.0 + E);
            begin
               World.Set_Resonant_Period (True);
               World.Set_Rotation_Period
                 (Spin_Resonance_Factor * World_Year);
            end;
         else
            World.Set_Rotation_Period (World_Year);
         end if;

      else

         World.Set_Rotation_Period (World_Day);

      end if;

   end Calculate_Day_Length;

   -------------------------------
   -- Calculate_Escape_Velocity --
   -------------------------------

   function Calculate_Escape_Velocity
     (Earth_Masses  : Real;
      Earth_Radii   : Real)
     return Real
   is
      use Harriet.Constants;
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
   begin
      return Sqrt (2.0 * Gravitational_Constant * Earth_Masses * Earth_Mass
                     / (Earth_Radii * Earth_Radius));
   end Calculate_Escape_Velocity;

   ------------------------
   -- Calculate_Gas_Life --
   ------------------------

   function Calculate_Gas_Life
     (World : World_Update;
      Gas    : Harriet.Db.Gas.Gas_Type)
      return Real
   is
      use Harriet.Elementary_Functions;
      V : constant Real :=
            Calculate_RMS_Velocity
              (Gas.Molecular_Weight,
               World.Exospheric_Temp);
      G : constant Real :=
            World.Surface_Acceleration;
      R : constant Real :=
            World.Radius;
      T : constant Real :=
            V ** 3 / (2.0 * G ** 2 * R) * Exp (3.0 * G * R) / V ** 2;
      Years : constant Real := T / 3600.0 / 24.0 / 365.25;

   begin

      if Years > 2.0e10 then
         return Real'Last;
      else
         return Years;
      end if;

   end Calculate_Gas_Life;

   ---------------------
   -- Calculate_Gases --
   ---------------------

   procedure Calculate_Gases
     (Star    : Harriet.Db.Star.Star_Type;
      World  : in out World_Update)
   is
      use Harriet.Elementary_Functions;
      Pressure : constant Real := World.Surface_Pressure / 1000.0;
      Gases    : constant Harriet.Db.Gas.Selection :=
                   Harriet.Db.Gas.Scan_By_Molecular_Weight;
      Count    : Natural := 0;
      YP       : Real;
      MW       : Real;
      Max_Gas  : constant := 16;
      Refs     : array (1 .. Max_Gas)
        of Harriet.Db.Gas_Reference;
      Amounts  : array (1 .. Max_Gas) of Real;
      Total    : Real := 0.0;
   begin

      for Gas of Gases loop

         YP :=
           Gas.Boiling_Point
           / 373.0 * (Log (Pressure + 0.001) / (-5050.5) + (1.0 / 373.0));
         MW := Gas.Molecular_Weight;

         if MW >= World.Min_Molecular_Weight
           and then YP >= 0.0
           and then YP < World.Night_Temperature_Low
         then
            declare
               Formula      : constant String := Gas.Formula;
               VRMS         : constant Real :=
                                Calculate_RMS_Velocity
                                  (MW,
                                   World.Exospheric_Temp);
               PVRMS         : constant Real :=
                                (1.0 / (1.0 +
                                   VRMS / World.Escape_Velocity))
                   ** (Star.Age / 1.0e9);
               Abundance    : constant Real :=
                                Gas.Abundance_S;
               Reactivity   : Real := 1.0;
               Fraction     : Real := 1.0;
               Pressure_2   : Real := 1.0;
               Amount       : Real;
            begin

               if Formula = "Ar" then
                  Reactivity := 0.15 * Star.Age / 4.0e9;
               elsif Formula = "He" then
                  Pressure_2 := 0.75 + Pressure;
                  Reactivity := (1.0 / (1.0 + Gas.Reactivity))
                    ** (Star.Age / 2.0e9 * Pressure_2);
               elsif Formula = "O2"
                 and then Star.Age > 2.0e9
                 and then World.Surface_Temperature in 270.0 .. 400.0
               then
                  Pressure_2 := 0.89 + Pressure / 4.0;
                  Reactivity := (1.0 / (1.0 + Gas.Reactivity))
                    ** (((Star.Age / 2.0e9) ** 0.25) * Pressure_2);
               elsif Formula = "CO2"
                 and then Star.Age > 2.0e9
                 and then World.Surface_Temperature in 270.0 .. 400.0
               then
                  Pressure_2 := 0.75 + Pressure;
                  Reactivity :=
                    (1.0 / (1.0 + Gas.Reactivity))
                    ** (((Star.Age / 2.0e9) ** 0.5) * Pressure_2);
                  Reactivity := Reactivity * 1.5;
               end if;

               Fraction := (1.0 - (World.Min_Molecular_Weight / MW));
               Amount := Abundance * PVRMS * Reactivity * Fraction;

               if Amount > 0.0 then
                  Count := Count + 1;
                  Refs (Count) := Gas.Get_Gas_Reference;
                  Amounts (Count) := Amount;
                  Total := Total + Amount;
               end if;

            end;
         end if;

      end loop;

      if Count > 0 then

         for I in 1 .. Count loop
            Harriet.Db.Atmosphere.Create
              (World      => World.Get_World_Reference,
               Gas        => Refs (I),
               Percentage => Amounts (I) / Total);
         end loop;

      end if;

   end Calculate_Gases;

   -------------------------------
   -- Calculate_Greenhouse_Rise --
   -------------------------------

   function Calculate_Greenhouse_Rise
     (Optical_Depth    : Real;
      Effective_Temp   : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
      Convection_Factor : constant Real :=
          Earth_Convection_Factor
        * (Surface_Pressure / Earth_Surface_Pressure) ** 0.4;
      Rise              : constant Real :=
          ((1.0 + 0.75 * Optical_Depth) ** 0.25 - 1.0)
        * Effective_Temp * Convection_Factor;
   begin

      if Verbose_Mode then
         Ada.Text_IO.Put ("gh: optical depth = ");
         Ada.Long_Float_Text_IO.Put (Optical_Depth, 1, 1, 0);
         Ada.Text_IO.Put ("; eff temp = ");
         Ada.Long_Float_Text_IO.Put (Effective_Temp - 273.0, 1, 1, 0);
         Ada.Text_IO.Put ("; pressure = ");
         Ada.Long_Float_Text_IO.Put (Surface_Pressure, 1, 1, 0);
         Ada.Text_IO.Put ("; rise = ");
         Ada.Long_Float_Text_IO.Put (Rise, 1, 1, 0);
         Ada.Text_IO.New_Line;
      end if;

      return Real'Max (Rise, 0.0);
   end Calculate_Greenhouse_Rise;

   ------------------------------
   -- Calculate_Hydro_Fraction --
   ------------------------------

   function Calculate_Hydro_Fraction
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real)
      return Real
   is
      Result : constant Real :=
                 (0.71 * Volatile_Gas_Inventory / 1000.0)
                 / Earth_Radii ** 2;
   begin
      return Real'Min (Result, 1.0);
   end Calculate_Hydro_Fraction;

   ----------------------------
   -- Calculate_Ice_Fraction --
   ----------------------------

   function Calculate_Ice_Fraction
     (Hydro_Fraction      : Real;
      Surface_Temperature : Real)
      return Real
   is
      Result : Real :=
                 (328.0 - Real'Min (Surface_Temperature, 328.0)) / 90.0;
   begin
      Result := Result ** 5;

      Result := Real'Min (Result, 1.5 * Hydro_Fraction);

      return Real'Min (Result, 1.0);

   end Calculate_Ice_Fraction;

   ------------------------------
   -- Calculate_Kothari_Radius --
   ------------------------------

   function Calculate_Kothari_Radius
     (Earth_Masses : Real;
      Jovian       : Boolean;
      Zone         : Orbit_Zone)
     return Real
   is
      use Harriet.Elementary_Functions;

      Mass_Ratio : constant Real := 1.0 / Earth_Masses;

      --  Some fudge constants
      --  Taste that delicious, sweet fudge

      A1_20      : constant := 6.485e12;
      A2_20      : constant := 4.0032e-8;
      Beta_20    : constant := 5.71e12;
      Jims_Fudge : constant := 1.09;

      --  method uses cgs system, so here's some conversion constants
      Solar_Mass_In_Grams : constant :=
        Harriet.Solar_System.Solar_Mass * 1000.0;
      Cm_Per_Km : constant := 100.0 * 1000.0;

      Weights : constant array (Boolean, Orbit_Zone) of Real :=
        (False => (15.0, 10.0, 10.0),
         True  => (9.5, 2.47, 7.0));
      Numbers : constant array (Boolean, Orbit_Zone) of Real :=
        (False => (8.0, 5.0, 5.0),
         True  => (4.5, 2.0, 4.0));

      Atomic_Weight : constant Real := Weights (Jovian, Zone);
      Atomic_Number : constant Real := Numbers (Jovian, Zone);

      T_1 : constant Real := Atomic_Weight * Atomic_Number;
      T_2 : constant Real := A2_20 * (Atomic_Weight ** (4.0 / 3.0))
        * (Solar_Mass_In_Grams ** (2.0 / 3.0))
        * (Mass_Ratio ** (2.0 / 3.0))
        / (A1_20 * (Atomic_Number ** 2))
        + 1.0;

      Result : constant Real :=
        2.0 * Beta_20 * (Solar_Mass_In_Grams ** (1.0 / 3.0))
        / (A1_20 * (T_1 ** (1.0 / 3.0)))
        / T_2
        * (Mass_Ratio ** (1.0 / 3.0))
        / Cm_Per_Km
        / Jims_Fudge;
   begin
      return Result;
   end Calculate_Kothari_Radius;

   -----------------------
   -- Calculate_Opacity --
   -----------------------

   function Calculate_Opacity
     (Molecular_Weight : Real;
      Surface_Pressure : Real)
      return Real
   is
      use Harriet.Solar_System;
      Optical_Depth : Real := 0.0;
   begin

      if Molecular_Weight in 0.0 .. 10.0 then
         Optical_Depth := Optical_Depth + 3.0;
      elsif Molecular_Weight in 10.0 .. 20.0 then
         Optical_Depth := Optical_Depth + 2.34;
      elsif Molecular_Weight in 20.0 .. 30.0 then
         Optical_Depth := Optical_Depth + 1.0;
      elsif Molecular_Weight in 30.0 .. 45.0 then
         Optical_Depth := Optical_Depth + 0.15;
      elsif Molecular_Weight in 45.0 .. 100.0 then
         Optical_Depth := Optical_Depth + 0.05;
      end if;

      if Surface_Pressure >= 70.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 8.333;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 6.666;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 3.333;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 2.0;
      elsif Surface_Pressure >= 50.0 * Earth_Surface_Pressure then
         Optical_Depth := Optical_Depth * 1.5;
      end if;

      return Optical_Depth;

   end Calculate_Opacity;

   ----------------------------
   -- Calculate_RMS_Velocity --
   ----------------------------

   function Calculate_RMS_Velocity
     (Molecular_Weight : Real;
      Exospheric_Temp  : Real)
     return Real
   is
      use Harriet.Constants;
      use Harriet.Elementary_Functions;
   begin
      return Sqrt (3.0 * Molar_Gas_Constant
                     * Exospheric_Temp / Molecular_Weight);
   end Calculate_RMS_Velocity;

   --------------------------------
   -- Calculate_Surface_Pressure --
   --------------------------------

   function Calculate_Surface_Pressure
     (Volatile_Gas_Inventory : Real;
      Earth_Radii            : Real;
      Earth_Gravities        : Real)
     return Real
   is
   begin
      return Volatile_Gas_Inventory * Earth_Gravities
        * (1013.25 / 1000.0)
        / (Earth_Radii ** 2);
   end Calculate_Surface_Pressure;

   -----------------------------------
   -- Calculate_Surface_Temperature --
   -----------------------------------

   procedure Calculate_Surface_Temperature
     (Star        : in     Harriet.Db.Star.Star_Type;
      World      : in out World_Update;
      First       : in     Boolean;
      Ecosphere   : in     Real;
      Last_Water  : in     Real;
      Last_Clouds : in     Real;
      Last_Ice    : in     Real;
      Last_Temp   : in     Real;
      Last_Albedo : in     Real)
   is
      use Harriet.Constants;
      use Harriet.Solar_System;
      Effective_Temp : Real;
      Greenhouse_Temp : Real;
      Water_Raw       : Real;

      Boil_Off        : Boolean := False;

      Earth_Orbits    : constant Real :=
                          World.Semimajor_Axis / Earth_Orbit;
      Earth_Radii     : constant Real :=
                          World.Radius / Earth_Radius;

   begin

      if First then
         World.Set_Albedo (Harriet.Solar_System.Earth_Albedo);
         Effective_Temp :=
           Effective_Temperature (Ecosphere, Earth_Orbits,
                                  World.Albedo);
         Greenhouse_Temp :=
           Calculate_Greenhouse_Rise
             (Calculate_Opacity
                  (World.Min_Molecular_Weight,
                   World.Surface_Temperature),
              Effective_Temp,
              World.Surface_Pressure);

         World.Set_Greenhouse_Rise (Greenhouse_Temp);
         World.Set_Surface_Temperature (Effective_Temp + Greenhouse_Temp);

         Set_Temperature_Range (World);

      end if;

      if World.Greenhouse_Effect
        and then World.Maximum_Temperature < World.Water_Boiling_Point
      then

--           Ada.Text_IO.Put ("Deluge: " & World.Name & " max ");
--           Ada.Long_Float_Text_IO.Put (World.Max_Temperature, 1, 1, 0);
--           Ada.Text_IO.Put (" < boil ");
--           Ada.Long_Float_Text_IO.Put
--             (World.Water_Boiling_Point, 1, 1, 0);
--           Ada.Text_IO.New_Line;

         World.Set_Greenhouse_Effect (False);
         World.Set_Volatile_Gas_Inventory
           (Volatile_Inventory (Star, World));
         World.Set_Surface_Pressure
           (Calculate_Surface_Pressure
              (World.Volatile_Gas_Inventory,
               World.Radius / Earth_Radius,
               World.Surface_Gravity));
         World.Set_Water_Boiling_Point
           (Calculate_Water_Boiling_Point (World.Surface_Pressure));
      end if;

      Water_Raw :=
        Calculate_Hydro_Fraction
          (World.Volatile_Gas_Inventory,
           Earth_Radii);
      World.Set_Water_Coverage (Water_Raw);

      World.Set_Cloud_Coverage
        (Calculate_Cloud_Fraction
           (World.Surface_Temperature,
            World.Min_Molecular_Weight,
            Earth_Radii,
            Water_Raw));

      World.Set_Ice_Coverage
        (Calculate_Ice_Fraction
           (Water_Raw,
            World.Surface_Temperature));

      if World.Greenhouse_Effect
        and then World.Surface_Pressure > 0.0
      then
         World.Set_Cloud_Coverage (1.0);
      end if;

      if World.Daytime_Temperature_High >= World.Water_Boiling_Point
        and then not First
        and then (World.Rotation_Period = World.Period
                  or else World.Resonant_Period)
      then
         World.Set_Water_Coverage (0.0);
         Boil_Off := True;

         if World.Min_Molecular_Weight > 18.0 then
            --  18.0 = water vapour
            World.Set_Cloud_Coverage (0.0);
         else
            World.Set_Cloud_Coverage (1.0);
         end if;
      end if;

      if World.Surface_Temperature
        < Freezing_Point_Of_Water - 3.0
      then
         World.Set_Water_Coverage (0.0);
      end if;

      World.Set_Albedo
        (Calculate_Albedo
           (World.Water_Coverage,
            World.Cloud_Coverage,
            World.Ice_Coverage,
            World.Surface_Pressure));

      Effective_Temp :=
        Effective_Temperature (Ecosphere,
                               World.Semimajor_Axis / Earth_Orbit,
                               World.Albedo);
      Greenhouse_Temp :=
        Calculate_Greenhouse_Rise
          (Calculate_Opacity
             (World.Min_Molecular_Weight,
              World.Surface_Temperature),
           Effective_Temp,
           World.Surface_Pressure);

      World.Set_Greenhouse_Rise (Greenhouse_Temp);
      World.Set_Surface_Temperature (Effective_Temp + Greenhouse_Temp);

      if not First
        and then not Boil_Off
      then
         World.Set_Water_Coverage
           ((World.Water_Coverage + Last_Water * 2.0) / 3.0);
         World.Set_Cloud_Coverage
           ((World.Cloud_Coverage + Last_Clouds * 2.0) / 3.0);
         World.Set_Ice_Coverage
           ((World.Ice_Coverage + Last_Ice * 2.0) / 3.0);
         World.Set_Albedo
           ((World.Albedo + Last_Albedo * 2.0) / 3.0);
         World.Set_Surface_Temperature
           ((World.Surface_Temperature + Last_Temp * 2.0) / 3.0);
      end if;

      Set_Temperature_Range (World);

   end Calculate_Surface_Temperature;

   -----------------------------------
   -- Calculate_Water_Boiling_Point --
   -----------------------------------

   function Calculate_Water_Boiling_Point
     (Surface_Pressure : Real)
      return Real
   is
      use Harriet.Elementary_Functions;
      Surface_Pressure_In_Bars : constant Real :=
                                   Surface_Pressure / 1000.0;
   begin
      return 1.0
        / ((Log (Surface_Pressure_In_Bars) / (-5050.5))
           + (1.0 / 373.0));
   end Calculate_Water_Boiling_Point;

   ----------------------------
   --  Effective_Temperature --
   ----------------------------

   function Effective_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real
   is
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
   begin
      return Sqrt (Ecosphere_Radius / Orbit_Radius)
        * ((1.0 - Albedo) / (1.0 - Earth_Albedo)) ** 0.25
        * Earth_Effective_Temp;
   end Effective_Temperature;

   ---------------------------
   -- Estimated_Temperature --
   ---------------------------

   function Estimated_Temperature
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real;
      Albedo             : Real)
     return Real
   is
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
   begin
      return Sqrt (Ecosphere_Radius / Orbit_Radius)
        * ((1.0 - Albedo) / (1.0 - Earth_Albedo)) ** 0.25
        * Earth_Average_Kelvin;
   end Estimated_Temperature;

   ----------------------
   -- Generate_Worlds --
   ----------------------

   procedure Generate_Star_System
     (Star_System : Harriet.Db.Star_System_Reference)
   is

      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;

      Star : constant Harriet.Db.Star.Star_Type :=
               Harriet.Db.Star.First_By_Star_System (Star_System);

      Solar_Masses : constant Non_Negative_Real :=
                       Star.Mass / Solar_Mass;

      Rock_Line  : constant Real := 0.3 * Solar_Masses;
      Frost_Line : constant Real := 2.7 * Solar_Masses;
      Current_Orbit : Real;

      World_Index : Positive := 1;

      Width        : Real;
      Density      : Real;
      Solid_Mass   : Real;
      Gas_Mass     : Real;
      Total_Mass   : Real;

      Is_Jovian    : Boolean := False;

      function Random_Accretion_Width
        (Orbit : Non_Negative_Real)
        return Non_Negative_Real;

      function Disk_Density
        (Orbit : Non_Negative_Real)
        return Non_Negative_Real;

      ------------------
      -- Disk_Density --
      ------------------

      function Disk_Density
        (Orbit : Non_Negative_Real)
        return Non_Negative_Real
      is
      begin
         if Orbit < Rock_Line then
            return 0.0;
         elsif Orbit < Frost_Line then
            return Solar_Masses / Orbit / Orbit;
         else
            return (Solar_Masses  +
                      Solar_Masses / Frost_Line * 8.0)
              / Orbit / Orbit;
         end if;
      end Disk_Density;

      ----------------------------
      -- Random_Accretion_Width --
      ----------------------------

      function Random_Accretion_Width
        (Orbit : Non_Negative_Real)
        return Non_Negative_Real
      is
         Min : Real;
         Max : Real;
      begin
         if Orbit < Frost_Line then
            Min := Orbit * 0.25;
            Max := Orbit * 0.75;
         else
            Min := Orbit * 0.25;
            Max := Orbit * 0.75;
         end if;
         return Harriet.Random.Unit_Random * (Max - Min) + Min;
      end Random_Accretion_Width;

      Deposit_Generator : Resources.Random_Deposit_Generator;

   begin

      declare
         Sys : constant Harriet.Db.Star_System.Star_System_Type :=
           Harriet.Db.Star_System.Get (Star_System);
      begin
         Deposit_Generator :=
           Resources.Create_Generator (Sys.X, Sys.Y, Sys.Z);
      end;

      Current_Orbit := Rock_Line + Random_Accretion_Width (Rock_Line);

      loop

         Width := Random_Accretion_Width (Current_Orbit);
         Density := Disk_Density (Current_Orbit);

         exit when Density <= 0.01;

         Solid_Mass :=
           Density * 1.2 * (Width * (Width + 2.0 * Current_Orbit));
         Gas_Mass := 0.0;

         if Solid_Mass > 3.0 then
            --  gas accumulation
            Gas_Mass := Solid_Mass ** 3;
            Is_Jovian := True;
         end if;

         Total_Mass := Solid_Mass + Gas_Mass;

         declare
            use type Harriet.Calendar.Time;
            World : World_Update :=
              Harriet.Db.World.Create;
            Period : constant Non_Negative_Real :=
              Harriet.Orbits.Period
                (Star.Mass, Current_Orbit);
            Epoch  : constant Harriet.Calendar.Time :=
              Harriet.Calendar.Clock
                - Duration (Period * Harriet.Random.Unit_Random);
         begin
            World.Set_Primary (Star);
            World.Set_Star_System (Star_System);
            World.Set_Semimajor_Axis (Current_Orbit);
            World.Set_Eccentricity (0.0);
            World.Set_Epoch (Epoch);

            World.Set_Name (Star.Name & " " &
                              Harriet.Roman_Images.Roman_Image
                              (World_Index));
            World.Set_Mass (Total_Mass * Earth_Mass);
            World.Set_Semimajor_Axis (Current_Orbit * Earth_Orbit);
            World.Set_Period
              (Get_Orbital_Period
                 (World.Semimajor_Axis,
                  World.Mass,
                  Star.Mass));

            World.Set_Radius (1000.0 *
                                 Calculate_Kothari_Radius
                                 (Total_Mass, Is_Jovian,
                                  Get_Orbit_Zone (Star.Luminosity,
                                                  Current_Orbit)));
            World.Set_Density (Ada.Numerics.Pi * World.Radius ** 3 /
                                  World.Mass);
            World.Set_Solid_Mass (Solid_Mass * Earth_Mass);
            World.Set_Gas_Mass (Gas_Mass * Earth_Mass);

            Set_Axial_Tilt (World);

            World.Set_Escape_Velocity
              (Calculate_Escape_Velocity (Total_Mass,
                                          World.Radius / Earth_Radius));
            World.Set_Surface_Acceleration
              (Harriet.Constants.Gravitational_Constant
                 * World.Mass /
                 (World.Radius ** 2));
            World.Set_Surface_Gravity
              (World.Surface_Acceleration
                 / Harriet.Solar_System.Earth_Gravity);
            World.Set_Exospheric_Temp
                 (Earth_Exospheric_Temp
                    / ((Current_Orbit / Sqrt (Star.Luminosity)) ** 2));

            World.Set_Gas_Giant (Is_Jovian);

            Calculate_Day_Length (Star, World);

            if Is_Jovian then

               World.Set_Greenhouse_Effect (False);
               World.Set_Volatile_Gas_Inventory (Real'Last);
               World.Set_Surface_Pressure (Real'Last);
               World.Set_Water_Boiling_Point (Real'Last);
               World.Set_Surface_Temperature (Real'Last);
               World.Set_Greenhouse_Rise (0.0);
               World.Set_Albedo (Gas_Giant_Albedo);
               World.Set_Water_Coverage (1.0);
               World.Set_Cloud_Coverage (1.0);
               World.Set_Ice_Coverage (0.0);
               World.Set_Habitability (0.0);

               World.Set_Surface_Temperature
                 (Estimated_Temperature
                    (Star.Ecosphere, Current_Orbit,
                     World.Albedo));

            else
               World.Set_Rms_Velocity
                 (Calculate_RMS_Velocity (14.0, World.Exospheric_Temp));

               World.Set_Min_Molecular_Weight
                 (Molecule_Limit (Total_Mass,
                  World.Radius / Earth_Radius,
                  World.Exospheric_Temp));

               World.Set_Greenhouse_Effect
                 (Has_Greenhouse (Star.Ecosphere, Current_Orbit));

               World.Set_Volatile_Gas_Inventory
                 (Volatile_Inventory (Total_Mass,
                  World.Escape_Velocity,
                  World.Rms_Velocity,
                  Solar_Masses,
                  Get_Orbit_Zone (Star.Luminosity,
                    Current_Orbit),
                  World.Greenhouse_Effect,
                  False));

               World.Set_Surface_Pressure
                 (Calculate_Surface_Pressure
                    (World.Volatile_Gas_Inventory,
                     World.Radius / Earth_Radius,
                     World.Surface_Gravity));

               World.Set_Albedo (Earth_Albedo);

               if World.Surface_Pressure = 0.0 then
                  World.Set_Water_Boiling_Point (0.0);
               else
                  World.Set_Water_Boiling_Point
                    (Calculate_Water_Boiling_Point
                       (World.Surface_Pressure));
               end if;

               Iterate_Surface_Temperature (Star, World);

               if World.Maximum_Temperature
                 >= Harriet.Constants.Freezing_Point_Of_Water
                 and then World.Minimum_Temperature
                   <= World.Water_Boiling_Point
                 and then World.Surface_Pressure > 0.0
               then
                  Calculate_Gases (Star, World);
               end if;

            end if;

            declare
               use Harriet.Db;
               Pressure : constant Real := World.Surface_Pressure;
               Category : World_Category;
               Habitability : Unit_Real := 0.0;
               Temp_Ice     : constant :=
                 Harriet.Constants.Freezing_Point_Of_Water;
               Temp_Nice    : constant :=
                 Harriet.Constants.Freezing_Point_Of_Water + 15.0;
               Temp_Hot     : constant :=
                 Harriet.Constants.Freezing_Point_Of_Water + 25.0;
               Temp_Too_Hot : constant :=
                 Harriet.Constants.Freezing_Point_Of_Water + 45.0;
               Temperature  : constant Real := World.Surface_Temperature;

            begin
               if World.Gas_Giant then
                  Category := Jovian;
               elsif Pressure < 1.0 then
                  null;
               elsif Pressure > 6000.0
                 and then World.Min_Molecular_Weight <= 2.0
               then
                  Category := Sub_Jovian;
                  World.Set_Gas_Giant (True);
               else
                  if World.Water_Coverage >= 0.95 then
                     Category := Water;
                  elsif World.Ice_Coverage >= 0.95 then
                     Category := Iceball;
                  elsif World.Water_Coverage >= 0.05
                    and then Temperature in Temp_Ice .. Temp_Too_Hot
                  then
                     Category := Temperate;
                     if Temperature < Temp_Nice then
                        Habitability := 0.4
                          + 0.6 * (Temperature - Temp_Ice)
                          / (Temp_Nice - Temp_Ice);
                     elsif Temperature > Temp_Hot then
                        Habitability := 0.4
                          + 0.6 * (Temp_Too_Hot - Temperature)
                          / (Temp_Too_Hot - Temp_Hot);
                     else
                        Habitability := 1.0;
                     end if;
                  elsif World.Maximum_Temperature
                    >= World.Water_Boiling_Point
                  then
                     Category := Venusian;
                  elsif Pressure < 250.0 then
                     Category := Martian;
                  elsif World.Surface_Temperature
                    < Harriet.Constants.Freezing_Point_Of_Water
                  then
                     Category := Iceball;
                     World.Set_Ice_Coverage (1.0);
                  else
                     Category := Desert;
                  end if;
                  if World.Mass < Earth_Mass / 1000.0 then
                     Habitability := Habitability / 1000.0;
                  elsif World.Mass < Earth_Mass / 100.0 then
                     Habitability := Habitability / 100.0;
                  elsif World.Mass < Earth_Mass * 1.5 then
                     null;
                  else
                     Habitability := Habitability / 5.0;
                  end if;
               end if;

               World.Set_Category (Category);
               World.Set_Habitability (Habitability);

               World.Set_Palette
                 (Harriet.Db.Palette.Get_By_Tag
                    (Ada.Characters.Handling.To_Lower (Category'Image)));

               Harriet.Configure.Resources.Create_Deposits
                 (World     => World,
                  Generator => Deposit_Generator);

            end;

            World_Index := World_Index + 1;
         end;

         Current_Orbit := Current_Orbit + Width * 2.0;

      end loop;

   end Generate_Star_System;

   --------------------
   -- Get_Orbit_Zone --
   --------------------

   function Get_Orbit_Zone
     (Luminosity   : Real;
      Orbit_Radius : Real)
     return Orbit_Zone
   is
      use Harriet.Elementary_Functions;
      Sqrt_Lum : constant Real := Sqrt (Luminosity);
   begin
      if Orbit_Radius < 4.0 * Sqrt_Lum then
         return 1;
      elsif Orbit_Radius < 15.0 * Sqrt_Lum then
         return 2;
      else
         return 3;
      end if;
   end Get_Orbit_Zone;

   ------------------------
   -- Get_Orbital_Period --
   ------------------------

   function Get_Orbital_Period
     (Separation  : Real;
      Small_Mass  : Real;
      Large_Mass  : Real)
      return Real
   is
      use Harriet.Elementary_Functions;
      use Harriet.Solar_System;
      AUs : constant Real := Separation / Earth_Orbit;
      Small : constant Real := Small_Mass / Solar_Mass;
      Large : constant Real := Large_Mass / Solar_Mass;
      Years : constant Real :=
                Sqrt (AUs ** 3 / (Small + Large));
   begin
      return Years * Earth_Sidereal_Year * 24.0 * 3600.0;
   end Get_Orbital_Period;

   --------------------
   -- Has_Greenhouse --
   --------------------

   function Has_Greenhouse
     (Ecosphere_Radius   : Real;
      Orbit_Radius       : Real)
     return Boolean
   is
      Temp : constant Real :=
        Effective_Temperature
        (Ecosphere_Radius, Orbit_Radius,
         Harriet.Constants.Greenhouse_Trigger_Albedo);
   begin
      return Temp > Harriet.Constants.Freezing_Point_Of_Water;
   end Has_Greenhouse;

   ---------------------------------
   -- Iterate_Surface_Temperature --
   ---------------------------------

   procedure Iterate_Surface_Temperature
     (Star    : in     Harriet.Db.Star.Star_Type;
      World  : in out World_Update)
   is
--      use Harriet.Solar_System;
--        Initial_Temp : Real :=
--                         Estimated_Temperature
--                           (Star.Ecosphere,
--                            World.Semimajor_Axis / Earth_Orbit,
--                            World.Albedo);
--        H2, H2O, N2, N : Harriet.Db.Atm_Gas.Atm_Gas_Type;

   begin

      Calculate_Surface_Temperature
        (Star, World, True, Star.Ecosphere,
         0.0, 0.0, 0.0, 0.0, 0.0);

      for I in 1 .. 25 loop

         declare
            Last_Water  : constant Real := World.Water_Coverage;
            Last_Clouds : constant Real := World.Cloud_Coverage;
            Last_Ice    : constant Real := World.Ice_Coverage;
            Last_Temp   : constant Real := World.Surface_Temperature;
            Last_Albedo : constant Real := World.Albedo;
         begin

            Calculate_Surface_Temperature
              (Star        => Star,
               World      => World,
               First       => False,
               Ecosphere   => Star.Ecosphere,
               Last_Water  => Last_Water,
               Last_Clouds => Last_Clouds,
               Last_Ice    => Last_Ice,
               Last_Temp   => Last_Temp,
               Last_Albedo => Last_Albedo);

            exit when abs (World.Surface_Temperature - Last_Temp) < 0.25;
         end;

      end loop;

--        H2.By_Formula (Star.Handle, "H2");
--        H2O.By_Formula (Star.Handle, "H2O");
--        N2.By_Formula (Star.Handle, "N2");
--        N.By_Formula (Star.Handle, "N");

--        declare
--           H2_Life  : constant Real := Calculate_Gas_Life (World, H2);
--           H2O_Life : constant Real := Calculate_Gas_Life (World, H2O);
--           N2_Life  : constant Real := Calculate_Gas_Life (World, N2);
--           N_Life   : constant Real := Calculate_Gas_Life (World, N);
--        begin
--
--           null;
--        end;

   end Iterate_Surface_Temperature;

   -----------
   -- Limit --
   -----------

   function Limit (X : Real) return Real is
      use Harriet.Elementary_Functions;
   begin
      return X / Sqrt (Sqrt (1.0 + X * X * X * X));
   end Limit;

   --------------------
   -- Molecule_Limit --
   --------------------

   function Molecule_Limit
     (Earth_Masses    : Real;
      Earth_Radii     : Real;
      Exospheric_Temp : Real)
     return Real
   is
      use Harriet.Constants;
      Gas_Retention_Threshold : constant := 6.0;
      Escape_Velocity         : constant Real :=
        Calculate_Escape_Velocity (Earth_Masses, Earth_Radii);
   begin
      return (3.0 * Molar_Gas_Constant * Exospheric_Temp)
        / (Escape_Velocity / Gas_Retention_Threshold) ** 2;
   end Molecule_Limit;

   --------------------
   -- Set_Axial_Tilt --
   --------------------

   procedure Set_Axial_Tilt
     (World : in out World_Update)
   is
   begin
      World.Set_Tilt (Harriet.Random.Unit_Random * 40.0);
   end Set_Axial_Tilt;

   ---------------------------
   -- Set_Temperature_Range --
   ---------------------------

   procedure Set_Temperature_Range
     (World : in out World_Update)
   is
      use Harriet.Elementary_Functions;
      Pressure_Mod : constant Real :=
          1.0 / Sqrt (1.0 + 20.0 * World.Surface_Pressure / 1000.0);
      PP_Mod       : constant Real :=
          1.0 / Sqrt (10.0 + 5.0 * World.Surface_Pressure / 1000.0);
      Tilt_Mod     : constant Real :=
                       abs (Cos (World.Tilt, 360.0)
                            * (1.0 + World.Eccentricity) ** 2);
      Day_Mod      : constant Real :=
                       1.0 / (200.0 * 3600.0 / World.Rotation_Period + 1.0);
      MH           : constant Real := (1.0 + Day_Mod) ** Pressure_Mod;
      ML           : constant Real := (1.0 - Day_Mod) ** Pressure_Mod;
      Hi           : constant Real := MH * World.Surface_Temperature;
      Max          : constant Real :=
                       World.Surface_Temperature
                         + Sqrt (World.Surface_Temperature) * 10.0;
      Min          : constant Real :=
                       World.Surface_Temperature
                         / Sqrt (World.Rotation_Period / 3600.0 + 24.0);
      Lo           : constant Real :=
                       Real'Max (ML * World.Surface_Temperature, Min);
      SH           : constant Real :=
                       Hi + ((100.0 + Hi) * Tilt_Mod) ** Sqrt (PP_Mod);
      WL           : constant Real :=
                       Real'Max (Lo
                                 - ((150.0 + Lo) * Tilt_Mod) ** Sqrt (PP_Mod),
                                 0.0);
   begin
      World.Set_Daytime_Temperature_High (Soft_Limit (Hi, Max, Min));
      World.Set_Night_Temperature_Low (Soft_Limit (Lo, Max, Min));
      World.Set_Maximum_Temperature (Soft_Limit (SH, Max, Min));
      World.Set_Minimum_Temperature (Soft_Limit (WL, Max, Min));
   end Set_Temperature_Range;

   ----------------
   -- Soft_Limit --
   ----------------

   function Soft_Limit (V, Max, Min : Real) return Real is
      DV : constant Real := V - Min;
      DM : constant Real := Max - Min;
   begin
      return (Limit (2.0 * DV / DM - 1.0) + 1.0) / 2.0 * DM + Min;
   end Soft_Limit;

   ------------------------
   -- Volatile_Inventory --
   ------------------------

   function Volatile_Inventory
     (Earth_Masses    : Real;
      Escape_Velocity : Real;
      RMS_Velocity    : Real;
      Stellar_Mass    : Real;
      Zone            : Orbit_Zone;
      Greenhouse      : Boolean;
      Accreted_Gas    : Boolean)
     return Real
   is
      use Harriet.Constants;
      Velocity_Ratio : constant Real :=
        Escape_Velocity / RMS_Velocity;
      Proportion : Real;
      Result     : Real;
   begin
      if Velocity_Ratio >= Gas_Retention_Threshold then

         case Zone is
            when 1 =>
               Proportion := 140_000.0;
            when 2 =>
               Proportion := 75_000.0;
            when 3 =>
               Proportion := 250.0;
         end case;

         Result := Proportion * Earth_Masses / Stellar_Mass;
         Result := Harriet.Random.About (Result, 0.2);
         if Greenhouse or else Accreted_Gas then
            return Result;
         else
            return Result / 140.0;
         end if;

      else
         return 0.0;
      end if;
   end Volatile_Inventory;

   ------------------------
   -- Volatile_Inventory --
   ------------------------

   function Volatile_Inventory
     (Star     : Harriet.Db.Star.Star_Type;
      World   : World_Update)
      return Real
   is
      use Harriet.Solar_System;
   begin
      return Volatile_Inventory
        (Earth_Masses    => World.Mass / Earth_Mass,
         Escape_Velocity => World.Escape_Velocity,
         RMS_Velocity    => World.Rms_Velocity,
         Stellar_Mass    => Star.Mass / Solar_Mass,
         Zone            =>
           Get_Orbit_Zone
             (Star.Luminosity,
              World.Semimajor_Axis / Earth_Orbit),
         Greenhouse      => World.Greenhouse_Effect,
         Accreted_Gas    => False);
   end Volatile_Inventory;

end Harriet.Configure.Star_Systems;
