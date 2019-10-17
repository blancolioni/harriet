private with Ada.Containers.Vectors;

with Harriet.Db.World;

with Tropos;

package Harriet.Configure.Resources is

   procedure Configure_Atmosphere_Components
     (Config : Tropos.Configuration);

   type Random_Deposit_Generator is private;

   function Create_Generator
     (X, Y, Z : Real)
      return Random_Deposit_Generator;

   procedure Create_Deposits
     (World     : Harriet.Db.World.World_Type;
      Generator : Random_Deposit_Generator);

private

   type Resource_Record is
      record
         Reference : Harriet.Db.Resource_Reference;
         Strength  : Non_Negative_Real;
      end record;

   package Resource_Vectors is
     new Ada.Containers.Vectors (Positive, Resource_Record);

   type Random_Deposit_Generator is
      record
         Resources      : Resource_Vectors.Vector;
         Total_Strength : Non_Negative_Real := 0.0;
      end record;

end Harriet.Configure.Resources;
