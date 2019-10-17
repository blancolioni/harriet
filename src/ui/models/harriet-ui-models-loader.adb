with WL.String_Maps;

with Harriet.UI.Models.Galaxy;
with Harriet.UI.Models.Shell;
with Harriet.UI.Models.Star_System;
with Harriet.UI.Models.Worlds;

package body Harriet.UI.Models.Loader is

   package Model_Maps is
     new WL.String_Maps (Root_Harriet_Model'Class);

   Map : Model_Maps.Map;

   procedure Check_Map;

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is

      procedure Add
        (Name    : String;
         Model   : Root_Harriet_Model'Class);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name  : String;
         Model : Root_Harriet_Model'Class)
      is
      begin
         Map.Insert (Name, Model);
      end Add;

   begin
      if Map.Is_Empty then
         Add ("shell", Harriet.UI.Models.Shell.Shell_Model);
         Add ("galaxy",
              Harriet.UI.Models.Galaxy.Galaxy_Model);
         Add ("star-system",
              Harriet.UI.Models.Star_System.Star_System_Model);
         Add ("world",
              Harriet.UI.Models.Worlds.World_Model);
      end if;
   end Check_Map;

   ------------
   -- Exists --
   ------------

   function Exists (Model_Name : String) return Boolean is
   begin
      Check_Map;
      return Map.Contains (Model_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Model_Name : String)
      return Root_Harriet_Model'Class
   is
   begin
      Check_Map;
      return Map.Element (Model_Name);
   end Get;

end Harriet.UI.Models.Loader;
