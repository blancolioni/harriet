with Harriet.Updates.Events;

with Harriet.Colonies.Updates;
with Harriet.Db.Colony;

with Harriet.Installations.Updates;
with Harriet.Db.Installation;

package body Harriet.Updates.Loader is

   ------------------
   -- Load_Updates --
   ------------------

   procedure Load_Updates is
   begin
      for Colony of Harriet.Db.Colony.Scan_By_Top_Record loop
         Harriet.Updates.Events.Update_At
           (Colony.Next_Update,
            Harriet.Colonies.Updates.Daily_Update
              (Colony.Get_Colony_Reference));
      end loop;
      for Installation of Harriet.Db.Installation.Scan_By_Top_Record loop
         Harriet.Updates.Events.Update_At
           (Installation.Next_Update,
            Harriet.Installations.Updates.Daily_Update
              (Installation.Get_Installation_Reference));
      end loop;
   end Load_Updates;

end Harriet.Updates.Loader;
