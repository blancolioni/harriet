with Harriet.Elementary_Functions;
with Harriet.Random;
with Harriet.Solar_System;

with Harriet.Db.Deposit;
with Harriet.Db.Gas;
with Harriet.Db.Resource_Sphere;

package body Harriet.Configure.Resources is

   -------------------------------------
   -- Configure_Atmosphere_Components --
   -------------------------------------

   procedure Configure_Atmosphere_Components
     (Config : Tropos.Configuration)
   is
   begin
      for Cfg of Config loop
         declare
            use Harriet.Solar_System;
            function Get (Name : String)
                          return Non_Negative_Real
            is (Get_Real (Cfg, Name));

            Formula      : constant String :=
                             Cfg.Get ("formula", Cfg.Config_Name);
            Weight       : constant Non_Negative_Real := Get ("weight");
            MP           : constant Non_Negative_Real := Get ("mp");
            BP           : constant Non_Negative_Real := Get ("bp");
            Density      : constant Non_Negative_Real := Get ("density");
            Abund_E      : constant Non_Negative_Real := Get ("abunde");
            Abund_S      : constant Non_Negative_Real := Get ("abunds");
            React        : constant Non_Negative_Real := Get ("react");
            Max_IPP_HG   : constant Non_Negative_Real := Get ("max_ipp_hg");
            Max_IPP_PPM  : constant Non_Negative_Real := Get ("max_ipp_ppm");

         begin
            Harriet.Db.Gas.Create
              (Formula          => Formula,
               Molecular_Weight => Weight,
               Melting_Point    => MP,
               Boiling_Point    => BP,
               Density          => Density,
               Abundance_E      => Abund_E,
               Abundance_S      => Abund_S,
               Reactivity       => React,
               Max_Ipp          =>
                 (if Max_IPP_HG /= 0.0
                  then Max_IPP_HG * Earth_Surface_Pressure / 760.0
                  else Max_IPP_PPM * Earth_Surface_Pressure / 1.0E6),
               Name             => Cfg.Get ("name", Formula));
         end;
      end loop;
   end Configure_Atmosphere_Components;

   ---------------------
   -- Create_Deposits --
   ---------------------

   procedure Create_Deposits
     (World     : Harriet.Db.World.World_Type;
      Generator : Random_Deposit_Generator)
   is
      use Harriet.Elementary_Functions;
      Gen : Random_Deposit_Generator := Generator;
      Initial_Concentration : constant Unit_Real :=
        (0.5 + Harriet.Random.Unit_Random / 2.0)
        ** (World.Radius
            / Harriet.Solar_System.Earth_Radius);
      Concentration         : Unit_Real :=
        Initial_Concentration;
   begin

      while Concentration > Initial_Concentration / 20.0 loop
         declare
            Pick   : Non_Negative_Real :=
              Harriet.Random.Unit_Random * Gen.Total_Strength;
            Choice : Positive := 1;
         begin
            while Pick > Gen.Resources (Choice).Strength loop
               Pick := Pick - Gen.Resources (Choice).Strength;
               Choice := Choice + 1;
            end loop;

            Harriet.Db.Deposit.Create
              (World         => World.Get_World_Reference,
               Resource      => Gen.Resources (Choice).Reference,
               Concentration =>
                 Harriet.Random.About (Concentration, Concentration / 10.0),
               Available     =>
                 Harriet.Quantities.To_Quantity
                   (Harriet.Random.About
                        ((2.0e5
                         + Gen.Resources (Choice).Strength * 1.0E6),
                         1.0e5)));
            Gen.Total_Strength :=
              Gen.Total_Strength - Gen.Resources (Choice).Strength;
            exit when Gen.Resources.Is_Empty;
            Gen.Resources (Choice) := Gen.Resources.Last_Element;
            Gen.Resources.Delete_Last;
            Concentration := Concentration
              * (Harriet.Random.Unit_Random / 4.0 + 0.25);
         end;
      end loop;
   end Create_Deposits;

   ----------------------
   -- Create_Generator --
   ----------------------

   function Create_Generator
     (X, Y, Z : Real)
      return Random_Deposit_Generator
   is
      Gen : Random_Deposit_Generator;
   begin
      for Sphere of Harriet.Db.Resource_Sphere.Scan_By_Top_Record loop
         declare
            use Harriet.Elementary_Functions;
            use type Harriet.Db.Resource_Reference;
            Distance : constant Non_Negative_Real :=
              Sqrt ((X - Sphere.Centre_X) ** 2
                    + (Y - Sphere.Centre_Y) ** 2
                    + (Z - Sphere.Centre_Z) ** 2);
            Strength : constant Non_Negative_Real :=
              (if Distance <= Sphere.Radius
               then Sphere.Strength
               else Sphere.Strength
               / ((Distance - Sphere.Radius + 1.0) ** Sphere.Attenuation));
            Found    : Boolean := False;

         begin
            for Item of Gen.Resources loop
               if Item.Reference = Sphere.Resource then
                  Item.Strength := Item.Strength + Strength;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Gen.Resources.Append ((Sphere.Resource, Strength));
            end if;

            Gen.Total_Strength := Gen.Total_Strength + Strength;
         end;

      end loop;
      return Gen;
   end Create_Generator;

end Harriet.Configure.Resources;
