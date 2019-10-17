with Ada.Numerics;

with Harriet.Constants;
with Harriet.Elementary_Functions;

package body Harriet.Orbits is

   function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                  renames Harriet.Elementary_Functions.Sqrt;

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
