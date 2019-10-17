with Ada.Text_IO;

with Tropos.Reader;

with Harriet.Configure.Commodities;

with Harriet.Db.Movement;
with Harriet.Db.Skill;
with Harriet.Db.Unit;

package body Harriet.Configure.Units is

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration);

   function Get_Movement
     (Tag : String)
      return Harriet.Db.Movement_Reference;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Real (Float'(Unit_Config.Get (Name, 0.0))));

      function Get_Skill return Harriet.Db.Skill_Reference;

      ---------------
      -- Get_Skill --
      ---------------

      function Get_Skill return Harriet.Db.Skill_Reference is
         Skills_Config : constant Tropos.Configuration :=
           Unit_Config.Child ("skills");
      begin
         for Config of Skills_Config loop
            declare
               use Harriet.Db;
               Tag : constant String := Config.Config_Name;
               Ref : constant Skill_Reference :=
                 Harriet.Db.Skill.Get_Reference_By_Tag (Tag);
            begin
               if Ref = Null_Skill_Reference then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "warning: " & Unit_Config.Config_Name
                     & ": no such skill: " & Tag);
               else
                  return Ref;
               end if;
            end;
         end loop;
         return Harriet.Db.Null_Skill_Reference;
      end Get_Skill;

      Unit : constant Harriet.Db.Unit_Reference :=
        Harriet.Db.Unit.Create
          (Tag        => Unit_Config.Config_Name,
           Enabled_By => Harriet.Db.Null_Technology_Reference,
           Movement   =>
             Get_Movement (Unit_Config.Get ("movement-type")),
           Base_Speed => Get ("movement-speed"),
           Armour     => Get ("armour"),
           Attack     => Get ("attack"),
           Discipline => Get ("discipline"),
           Recon      => Get ("recon"),
           Camoflage  => Get ("camoflage"),
           Skill      => Get_Skill);

      Constructed : constant Harriet.Db.Constructed_Reference :=
        Harriet.Db.Unit.Get (Unit).Get_Constructed_Reference;
      Supplied    : constant Harriet.Db.Supplied_Reference :=
        Harriet.Db.Unit.Get (Unit).Get_Supplied_Reference;
   begin
      Harriet.Configure.Commodities.Configure_Constructed
        (Constructed, Unit_Config);
      Harriet.Configure.Commodities.Configure_Supplied
        (Supplied, Unit_Config);
   end Configure_Unit;

   ---------------------
   -- Configure_Units --
   ---------------------

   procedure Configure_Units (Scenario_Name : String) is
   begin
      for Unit_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "units"),
           "unit")
      loop
         Configure_Unit (Unit_Config);
      end loop;
   end Configure_Units;

   ------------------
   -- Get_Movement --
   ------------------

   function Get_Movement
     (Tag : String)
      return Harriet.Db.Movement_Reference
   is
      use Harriet.Db;
      Ref : constant Movement_Reference :=
        Movement.Get_Reference_By_Tag (Tag);
   begin
      if Ref = Null_Movement_Reference then
         return Movement.Create (Tag);
      else
         return Ref;
      end if;
   end Get_Movement;

end Harriet.Configure.Units;
