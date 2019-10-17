with Tropos.Reader;

with Harriet.Db.Commodity;
with Harriet.Db.Utility_Class;
with Harriet.Db.Utility_Function;

package body Harriet.Configure.Utility is

   procedure Configure_Utility_Class
     (Utility_Class_Config : Tropos.Configuration);

   -----------------------
   -- Configure_Utility --
   -----------------------

   procedure Configure_Utility
     (Scenario_Name : String)
   is
   begin
      for Utility_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "utility"),
           "utility")
      loop
         Configure_Utility_Class (Utility_Config);
      end loop;
   end Configure_Utility;

   -----------------------------
   -- Configure_Utility_Class --
   -----------------------------

   procedure Configure_Utility_Class
     (Utility_Class_Config : Tropos.Configuration)
   is
      Reference : constant Harriet.Db.Utility_Class_Reference :=
                    Harriet.Db.Utility_Class.Create
                      (Utility_Class_Config.Config_Name);
   begin
      for Function_Config of Utility_Class_Config loop
         declare
            Tag : constant String :=
                    Function_Config.Get (1);

            function Arg (Index : Positive) return Real
            is (if Index < Function_Config.Child_Count
                then Real (Float'(Function_Config.Get (Index + 1)))
                else 0.0);
         begin
            Harriet.Db.Utility_Function.Create
              (Tag           => Tag,
               Commodity     =>
                 Harriet.Db.Commodity.Get_Reference_By_Tag
                   (Function_Config.Config_Name),
               Utility_Class => Reference,
               A1            => Arg (1),
               A2            => Arg (2),
               A3            => Arg (3),
               A4            => Arg (4),
               A5            => Arg (5),
               A6            => Arg (6));
         end;
      end loop;
   end Configure_Utility_Class;

end Harriet.Configure.Utility;
