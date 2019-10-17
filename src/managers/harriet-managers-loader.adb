with Harriet.Managers.Colonies;
with Harriet.Managers.Factions;
with Harriet.Managers.Installations;

package body Harriet.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
      Register.Insert
        ("default-colony",
         Harriet.Managers.Colonies.Create_Default_Manager'Access);
      Register.Insert
        ("default-faction",
         Harriet.Managers.Factions.Create_Default_Manager'Access);
      Register.Insert
        ("default-mine",
         Harriet.Managers.Installations.Create_Default_Mine_Manager'Access);
   end Register_Managers;

end Harriet.Managers.Loader;
