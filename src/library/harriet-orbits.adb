with Ada.Numerics;

with Harriet.Constants;
with Harriet.Elementary_Functions;

package body Harriet.Orbits is

   function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                  renames Harriet.Elementary_Functions.Sqrt;

   -------------------------
   -- Calculate_Longitude --
   -------------------------

   function Calculate_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Duration)
      return Real
   is
      Period      : constant Non_Negative_Real :=
        Harriet.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
        Orbit_Count - Real'Floor (Orbit_Count);
      Longitude   : Real := Partial * 360.0;
   begin
      Longitude := Partial * 360.0;
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      if Longitude >= 180.0 then
         Longitude := -(360.0 - Longitude);
      end if;
      return Longitude;
   end Calculate_Longitude;

   ------------------------
   -- Calculate_Position --
   ------------------------

   procedure Calculate_Position
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Duration;
      Latitude   : out Real;
      Longitude  : out Real)
   is
      use Harriet.Elementary_Functions;
      Period      : constant Non_Negative_Real :=
        Harriet.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Non_Negative_Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
        Orbit_Count - Real'Truncation (Orbit_Count);
   begin
      Longitude := Partial * 360.0;
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      if Longitude >= 180.0 then
         Longitude := -(360.0 - Longitude);
      end if;
      Latitude := Arcsin (Partial, 360.0);
   end Calculate_Position;

   ------------
   -- Period --
   ------------

   function Period
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real)
      return Non_Negative_Real
   is
      Pi : constant := Ada.Numerics.Pi;
      G  : constant := Harriet.Constants.Gravitational_Constant;
      M  : constant Non_Negative_Real := Large_Mass;
   begin
      return 2.0 * Pi * Sqrt (Orbit ** 3 / G / M);
   end Period;

end Harriet.Orbits;
