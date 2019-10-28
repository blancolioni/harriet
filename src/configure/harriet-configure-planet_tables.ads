private package Harriet.Configure.Planet_Tables is

   function Random_Planet_Mass
     (Zone : Planetary_Zone)
         return Non_Negative_Real;

private

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

end Harriet.Configure.Planet_Tables;
