with Ada.Numerics;

package Harriet.Constants is

   Pi : constant := Ada.Numerics.Pi;

   Gravitational_Constant  : constant := 6.67300e-11;
   Molar_Gas_Constant      : constant := 8314.41;
   Freezing_Point_Of_Water : constant := 273.15;

   Greenhouse_Trigger_Albedo : constant := 0.2;
   Gas_Retention_Threshold   : constant := 6.0;

   function To_Celsius (Kelvin : Non_Negative_Real) return Real
   is (Kelvin - Freezing_Point_Of_Water);

end Harriet.Constants;
