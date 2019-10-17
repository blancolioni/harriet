with WL.Random.Names;

package Harriet.Configure.Galaxies is

   type Biased_Coordinate_Generator is access
     procedure (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Centre_Bias_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Cube_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   procedure Generate_Galaxy
     (Number_Of_Systems  : Positive;
      Radius_X           : Non_Negative_Real;
      Radius_Y           : Non_Negative_Real;
      Radius_Z           : Non_Negative_Real;
      Create_Coordinates : Biased_Coordinate_Generator;
      Names              : WL.Random.Names.Name_Generator);

   procedure Generate_Palettes;

end Harriet.Configure.Galaxies;
