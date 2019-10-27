with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.Numerics.Roman;
with WL.Random;

with Harriet.Elementary_Functions;
with Harriet.Random;
with Harriet.Real_Images;

with Harriet.Solar_System;

with Harriet.Db.Star;

package body Harriet.Configure.Star_Systems is

   use all type Harriet.Db.World_Composition;

   subtype World_Composition is Harriet.Db.World_Composition;

   subtype Rocky_World is World_Composition range Ice .. Rock_Iron;

   type Orbit_Zone is
     (Red, Yellow, Green, Blue, Black);

   subtype Planetary_Zone is Orbit_Zone range Yellow .. Black;

   type Atmosphere_Class is (None, Trace, Thin, Standard, Dense);

   type Atmospheric_Gas is
     (Ar, Cl2, CH4, CO2, F2, H2, He, N2, NH3, O2, SO2);

   type Atmospheric_Component is
      record
         Gas : Atmospheric_Gas;
         Partial : Unit_Real;
      end record;

   package Atmospheric_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Atmospheric_Component);

   function More (Left, Right : Atmospheric_Component) return Boolean
   is (Left.Partial > Right.Partial);

   package Atmospheric_Sorting is
     new Atmospheric_Component_Lists.Generic_Sorting (More);

   type Atmosphere is
      record
         List : Atmospheric_Component_Lists.List;
      end record;

   procedure Add_Component
     (Atm     : in out Atmosphere;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real);

   function Get_Zone
     (Star : Harriet.Db.Star.Star_Type;
      AUs  : Non_Negative_Real)
      return Orbit_Zone;

   function D6 return Non_Negative_Real
   is (Harriet.Random.Unit_Random * 5.0 + 1.0);

   function D (Count : Positive) return Non_Negative_Real;
   function DR return Positive
   is (WL.Random.Random_Number (1, 6) + WL.Random.Random_Number (1, 6));

   function TDR return Positive
   is (WL.Random.Random_Number (1, 6) + DR);

   procedure Put (Width : Positive;
                  Value : String);

   procedure Put (Width : Positive;
                  Value : Real);

   procedure Put (Width : Positive;
                  Value : Integer);

   procedure Generate_World
     (Star  : Harriet.Db.Star.Star_Type;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real);

   package Planet_Tables is

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real;

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Orbit_Zone)
         return Harriet.Db.World_Composition;

      function Random_Planet_Density
        (Composition : Harriet.Db.World_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Atmospheric_Class
        (Zone : Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class;

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_World)
         return Atmosphere;

   end Planet_Tables;

   -------------------
   -- Planet_Tables --
   -------------------

   package body Planet_Tables is

      type Mass_Parameters is
         record
            Base   : Non_Negative_Real;
            Random : Natural;
         end record;

      function Z return Mass_Parameters is (0.0, 0);
      function K (N : Non_Negative_Real) return Mass_Parameters is (N, 0);
      function R (N : Positive) return Mass_Parameters is (0.0, N);

      type Mass_Table is array (2 .. 12, Planetary_Zone) of Mass_Parameters;
      Planet_Mass : constant Mass_Table :=
                      (2  => (others => Z),
                       3  => (Black => Z, others => K (0.1)),
                       4  => (K (0.1), K (0.2), K (0.2), K (0.1)),
                       5  => (K (0.2), K (0.5), K (0.5), K (0.2)),
                       6  => (K (0.3), K (0.8), K (0.8), K (0.1)),
                       7  => (K (0.5), K (1.0), R (5), K (0.4)),
                       8  => (K (0.8), K (1.2), R (5), K (0.5)),
                       9  => (K (1.0), K (1.5), R (10), R (5)),
                       10 => (K (1.5), K (2.0), R (50), R (10)),
                       11 => (R (1), R (50), R (50), R (50)),
                       12 => (R (50), R (100), R (100), R (100)));

      type Zone_Composition is
        array (Planetary_Zone) of Harriet.Db.World_Composition;

      type Composition_Parameters is
         record
            Mass        : Non_Negative_Real;
            Composition : Zone_Composition;
         end record;

      Composition_Table : constant array (Positive range <>)
        of Composition_Parameters :=
          ((0.1, (Rock_Iron, Rock, Rock_Ice, Ice)),
           (0.4, (Rock_Iron, Rock_Iron, Rock, Rock_Ice)),
           (0.6, (Rock_Iron, Rock_Iron, Rock_Iron, Rock_Ice)),
           (1.1, (Rock_Iron, Rock_Iron, Rock_Iron, Gaseous)),
           (10.0, (Rock_Iron, Rock_Iron, Gaseous, Gaseous)),
           (50.0, (Gaseous, Gaseous, Gaseous, Gaseous)),
           (100.0, (Gaseous, Gaseous, Hydrogen, Hydrogen)),
           (1000.0, (others => Hydrogen)));

      Density_Table : constant array
        (World_Composition, 1 .. 3, 1 .. 2) of Real :=
                        (Hydrogen => (others => (0.19, 0.21)),
                         Gaseous  => (others => (0.2, 0.3)),
                         Ice      => (others => (0.1, 0.2)),
                         Rock     => (others => (0.6, 0.7)),
                         Rock_Ice => (others => (0.3, 0.5)),
                         Rock_Iron => ((0.5, 0.6), (1.0, 1.6), (1.0, 2.5)));

      -----------------
      -- Composition --
      -----------------

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Orbit_Zone)
         return World_Composition
      is
      begin
         for Item of Composition_Table loop
            if Mass < Item.Mass then
               return Item.Composition (Zone);
            end if;
         end loop;
         return Composition_Table (Composition_Table'Last).Composition (Zone);
      end Composition;

      -----------------------
      -- Random_Atmosphere --
      -----------------------

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_World)
         return Atmosphere
      is
         Atm : Atmosphere;
         type Component_Array is array (Positive range <>) of Atmospheric_Gas;
         Main : constant Component_Array :=
                  (case Composition is
                      when Rock | Rock_Iron =>
                     (case Zone is
                         when Yellow       => (CO2, N2, SO2),
                         when Green | Blue => (CO2, N2, CH4),
                         when Black        => (1 => H2)),
                      when Ice | Rock_Ice   =>
                     (case Zone is
                         when Yellow | Green   =>
                        (raise Constraint_Error with
                           "ice world in yellow or green zone"),
                         when Blue             => (CO2, CH4),
                         when Black            => (1 => H2)));

         Trace : constant Component_Array :=
                   (case Composition is
                       when Rock | Rock_Iron =>
                      (case Zone is
                          when Yellow       => (Ar, Cl2, F2),
                          when Green | Blue => (Ar, NH3, SO2, Cl2, F2),
                          when Black        => (1 => He)),
                       when Ice | Rock_Ice   =>
                      (case Zone is
                          when Yellow | Green   =>
                         (raise Constraint_Error with
                            "ice world in yellow or green zone"),
                          when Blue             => (Ar, N2, NH3),
                          when Black            => (1 => He)));

         Total : Unit_Real := 0.0;

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real);

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real)
         is
         begin
            for Gas of Components loop
               declare
                  Partial : constant Unit_Real :=
                              Real'Min (D (Count) * Scale, 1.0 - Total);
               begin
                  if Partial > 0.0 then
                     Atm.List.Append ((Gas, Partial));
                     Total := Total + Partial;
                  end if;
               end;
            end loop;
         end Add;

      begin
         Add (Main, 1, 0.1);
         Add (Trace, 2, 0.01);

         declare
            Item : Atmospheric_Component renames
                     Atm.List (Atm.List.First);
         begin
            Item.Partial := Item.Partial + 1.0 - Total;
         end;

         Atmospheric_Sorting.Sort (Atm.List);

         return Atm;
      end Random_Atmosphere;

      ------------------------------
      -- Random_Atmospheric_Class --
      ------------------------------

      function Random_Atmospheric_Class
        (Zone : Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class
      is
         Close      : constant Boolean := Zone in Yellow | Green;
         Base_Class : constant Atmosphere_Class :=
                        (if Mass <= 0.3
                         then (if Close then None else Trace)
                         elsif Mass <= 0.5
                         then (if Close then Trace else Thin)
                         elsif Mass <= 0.7 then Thin
                         elsif Mass <= 0.9 then Standard
                         elsif Mass <= 1.3
                         then (if Close then Dense else Standard)
                         else Dense);
         Step_Roll  : constant Unit_Real := Harriet.Random.Unit_Random;
         Class      : constant Atmosphere_Class :=
                        (if Step_Roll <= 0.1 and then Base_Class > None
                         then Atmosphere_Class'Pred (Base_Class)
                         elsif Step_Roll >= 0.9 and then Base_Class < Dense
                         then Atmosphere_Class'Succ (Base_Class)
                         else Base_Class);
      begin
         return Class;
      end Random_Atmospheric_Class;

      ---------------------------
      -- Random_Planet_Density --
      ---------------------------

      function Random_Planet_Density
        (Composition : World_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real
      is
         Index : constant Positive :=
                   (if Mass < 1.0 then 1
                    elsif Mass < 2.0 then 2
                    else 3);
         Low   : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 1);
         High  : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 2);
      begin
         return Harriet.Random.Unit_Random * (High - Low) + Low;
      end Random_Planet_Density;

      ------------------------
      -- Random_Planet_Mass --
      ------------------------

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real
      is
         Roll : constant Positive := DR;
         Parameters : constant Mass_Parameters :=
                        Planet_Mass (Roll, Zone);
         Mass : constant Non_Negative_Real :=
                        (Parameters.Base
                         + Real (Parameters.Random) * D6)
                        * (1.0 - Real (D (2)) / 100.0);
      begin
         if Mass >= 300.0
           and then Harriet.Random.Unit_Random < 0.5
         then
            return Mass * Real (D (2));
         else
            return Mass;
         end if;
      end Random_Planet_Mass;

      ----------------------------
      -- Random_Planet_Rotation --
      ----------------------------

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real
      is
         N : constant Positive :=
               (if Mass <= 0.5 then 6
                elsif Mass < 5.0 then 5
                elsif Mass < 50.0 then 4
                else 3);
         Base : constant Non_Negative_Real :=
                  Real (D (N)) * (0.8 + Harriet.Random.Unit_Random * 0.4);
      begin
         if Mass < 10.0 then
            if Orbit < 0.3 then
               return Year * Harriet.Solar_System.Earth_Sidereal_Year;
            elsif Orbit < 0.4 then
               return Base * Real (D6) * 10.0;
            elsif Orbit < 0.5 then
               return Base * Real (D6);
            else
               return Base;
            end if;
         else
            return Base;
         end if;
      end Random_Planet_Rotation;

      -----------------------------
      -- Random_Surface_Pressure --
      -----------------------------

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real
      is
      begin
         return (case Class is
                    when None => 0.0,
                    when Trace =>
                      Gravity * D (2) * 0.01,
                    when Thin  =>
                      Gravity * D6 * 0.1,
                    when Standard =>
                      Gravity * D (3) * 0.1,
                    when Dense    =>
                      Gravity * D (2) * 10.0);
      end Random_Surface_Pressure;

   end Planet_Tables;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (Atm     : in out Atmosphere;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real)
   is
      Ratio : constant Unit_Real := 1.0 - Partial;
   begin
      for Item of Atm.List loop
         Item.Partial := Item.Partial * Ratio;
      end loop;
      Atm.List.Append ((Gas, Partial));
      Atmospheric_Sorting.Sort (Atm.List);
   end Add_Component;

   -------
   -- D --
   -------

   function D (Count : Positive) return Non_Negative_Real is
      Value : Non_Negative_Real := 0.0;
   begin
      for I in 1 .. Count loop
         Value := Value + D6;
      end loop;
      return Value;
   end D;

   --------------------------
   -- Generate_Star_System --
   --------------------------

   procedure Generate_Star_System
     (Star_System : Harriet.Db.Star_System_Reference)
   is
      Star : constant Harriet.Db.Star.Star_Type :=
               Harriet.Db.Star.First_By_Star_System (Star_System);
      Planet_Count : constant Positive := TDR;
      Ds           : array (1 .. Planet_Count) of Non_Negative_Real;
   begin
      Ds (Ds'First) := Real (D6) / 10.0;
      for I in Ds'First + 1 .. Ds'Last loop
         Ds (I) := Ds (I - 1) * (Real (D (2)) / 10.0 + 1.0);
      end loop;

      Put (16, Star.Name);
      Ada.Text_IO.Put (Harriet.Db.Spectral_Class'Image (Star.Class));
      Ada.Text_IO.Put (Character'Val (48 + Star.Subclass));
      Ada.Text_IO.Set_Col (24);
      Put (8, Planet_Count);

      for D of Ds loop
         declare
            Zone : constant Orbit_Zone :=
                     Get_Zone (Star, D);
         begin
            Ada.Text_IO.Put
              (case Zone is
                  when Red => 'R',
                  when Yellow => 'Y',
                  when Green  => 'G',
                  when Blue   => 'B',
                  when Black  => 'x');
         end;
      end loop;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("  World");
      Ada.Text_IO.Set_Col (16);
      Put (8, "Zone");
      Put (12, "Type");
      Put (8, "Orbit");
      Put (8, "Mass");
      Put (8, "Density");
      Put (8, "Radius");
      Put (8, "Gravity");
      Put (8, "Year");
      Put (8, "Day");
      Put (8, "Temp");
      Put (16, "Life");
      Put (8, "Atm");
      Put (8, "Pressure");
      Ada.Text_IO.New_Line;

      declare
         Count : Natural := 0;
      begin
         for D of Ds loop
            if Get_Zone (Star, D) > Red then
               Count := Count + 1;
               Generate_World (Star, Count, Get_Zone (Star, D), D);
            end if;
         end loop;
      end;

      Ada.Text_IO.New_Line;

   end Generate_Star_System;

   --------------------
   -- Generate_World --
   --------------------

   procedure Generate_World
     (Star  : Harriet.Db.Star.Star_Type;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real)
   is
      use Harriet.Elementary_Functions;
      Name : constant String :=
               Star.Name & " "
               & WL.Numerics.Roman.Roman_Image (Index);
      Year : constant Non_Negative_Real :=
               Sqrt (Orbit ** 3 /
                     (Star.Mass / Harriet.Solar_System.Solar_Mass));
      Mass : constant Non_Negative_Real :=
               Planet_Tables.Random_Planet_Mass (Zone);
      Composition : constant World_Composition :=
                      Planet_Tables.Composition (Mass, Zone);
      Density     : constant Non_Negative_Real :=
                      Planet_Tables.Random_Planet_Density (Composition, Mass);
      Radius      : constant Non_Negative_Real :=
                      (Mass / Density) ** (1.0 / 3.0);
      Gravity     : constant Non_Negative_Real := Radius * Density;
      Day         : constant Non_Negative_Real :=
                      Planet_Tables.Random_Planet_Rotation
                        (Mass  => Mass,
                         Orbit => Orbit,
                         Year  => Year);
      Atmospheric_Class : constant Atmosphere_Class :=
                            Planet_Tables.Random_Atmospheric_Class
                              (Zone, Mass);
      Primordial_Pressure : constant Non_Negative_Real :=
                              Planet_Tables.Random_Surface_Pressure
                                (Atmospheric_Class, Gravity);
      Primordial_Atm      : constant Atmosphere :=
                              (if Composition in Rocky_World
                               and then Atmospheric_Class /= None
                               then Planet_Tables.Random_Atmosphere
                                 (Zone, Composition)
                               else (List => <>));
      Base_Temperature    : constant Non_Negative_Real :=
                              (Star.Luminosity ** 0.25) * 280.0
                              / Sqrt (Orbit);
      Primordial_Temp     : constant Non_Negative_Real :=
                              Real'Max
                                (1.0,
                                 Base_Temperature
                                 - (case Atmospheric_Class is
                                      when None | Trace    => 0.0,
                                      when Thin | Standard => 5.0,
                                      when Dense           => 20.0));

      Life_Bearing : constant Boolean :=
                       (Primordial_Temp in 253.0 .. 323.0
                        and then Atmospheric_Class >= Thin);

      type Life_Complexity_Type is
        (Prebiotic, Single_Celled, Plants, Multicellular);

      Life_Complexity : constant Life_Complexity_Type :=
                          (if Star.Age <= 1.0e9
                           then Prebiotic
                           elsif Star.Age <= 2.0e9
                           then Single_Celled
                           elsif Star.Age <= 3.0e9
                           then Plants
                           else Multicellular);

      Current_Atm      : Atmosphere := Primordial_Atm;
      Current_Pressure : Non_Negative_Real := Primordial_Pressure;
      Current_Temperature : Non_Negative_Real := Primordial_Temp;

   begin

      if Life_Bearing
        and then Life_Complexity >= Plants
      then
         for Item of Current_Atm.List loop
            if Item.Gas in CO2 | CH4 then
               Item.Partial := 0.0;
            end if;
         end loop;
         Add_Component (Current_Atm, O2, D6 * 5.0 / 100.0);
         Atmospheric_Sorting.Sort (Current_Atm.List);
         Current_Pressure := Current_Pressure / 2.0;
      end if;

      for Item of Current_Atm.List loop
         if Item.Gas = CO2 then
            Current_Temperature := Current_Temperature + Item.Partial * 100.0;
         end if;
      end loop;

      Ada.Text_IO.Put ("  " & Name);
      Ada.Text_IO.Set_Col (16);
      Ada.Text_IO.Put
        (case Zone is
            when Yellow => "Yellow",
            when Green  => "Green",
            when Blue   => "Blue",
            when Black  => "Black");
      Ada.Text_IO.Set_Col (24);
      Ada.Text_IO.Put
        (case Composition is
            when Hydrogen => "Hydrogen",
            when Gaseous  => "Gas",
            when Ice      => "Ice",
            when Rock     => "Rock",
            when Rock_Ice => "Rock-Ice",
            when Rock_Iron => "Rock-Iron");

      Ada.Text_IO.Set_Col (36);
      Put (8, Orbit);
      Put (8, Mass);
      Put (8, Density);
      Put (8, Radius);
      Put (8, Gravity);
      Put (8, Year);
      Put (8, Day);
      Put (8, Current_Temperature - 273.0);

      if Composition not in Hydrogen | Gaseous
        and then Life_Bearing
      then
         Put (16,
              (case Life_Complexity is
                  when Prebiotic     => "prebiotic",
                  when Single_Celled => "single-celled",
                  when Plants        => "plants",
                  when Multicellular => "multi-cellular"));
      else
         Put (16, "none");
      end if;

      if Composition not in Hydrogen | Gaseous then
         Put (8,
              (case Atmospheric_Class is
                  when None => "None",
                  when Trace => "Trace",
                  when Thin  => "Thin",
                  when Standard => "Std",
                  when Dense    => "Dense"));
      else
         Put (8, " - ");
      end if;

      Put (8, Current_Pressure);
      for Item of Current_Atm.List loop
         Ada.Text_IO.Put (" " & Item.Gas'Image);
         Ada.Text_IO.Put (Natural'Image (Natural (Item.Partial * 100.0)));
      end loop;

      Ada.Text_IO.New_Line;
   end Generate_World;

   --------------
   -- Get_Zone --
   --------------

   function Get_Zone
     (Star : Harriet.Db.Star.Star_Type;
      AUs  : Non_Negative_Real)
      return Orbit_Zone
   is
      Lum : constant Non_Negative_Real :=
              Harriet.Elementary_Functions.Sqrt (Star.Luminosity);
   begin
      if AUs < Lum * 0.25 then
         return Red;
      elsif AUs < Lum * 0.75 then
         return Yellow;
      elsif AUs < Lum * 1.5 then
         return Green;
      elsif AUs < Lum * 20.0 then
         return Blue;
      else
         return Black;
      end if;
   end Get_Zone;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : String)
   is
      use Ada.Text_IO;
      Target : constant Count := Col + Count (Width);
   begin
      Put (Value);
      Set_Col (Target);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Real)
   is
   begin
      Put (Width, Harriet.Real_Images.Approximate_Image (Value));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Integer)
   is
   begin
      Put (Width, Value'Image);
   end Put;

end Harriet.Configure.Star_Systems;
