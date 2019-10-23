with Harriet.Db.Faction;
with Harriet.Db.Goal;

package body Harriet.Managers.Fleets is

   type Default_Fleet_Manager_Type is
     new Root_Manager_Type with
      record
         Faction : Harriet.Db.Faction_Reference;
      end record;

   overriding function Identifier
     (Manager : Default_Fleet_Manager_Type)
      return String
   is (Harriet.Db.Faction.Get (Manager.Faction).Name & " Fleet Manager");

   overriding procedure Activate
     (Manager : not null access Default_Fleet_Manager_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Default_Fleet_Manager_Type)
   is
   begin
      Manager.Log
        (Manager.Identifier & " activating");

      for Goal of
        Harriet.Db.Goal.Select_Faction_Priority_Bounded_By_Priority
          (Active          => True,
           Faction         => Manager.Faction,
           Start_Priority  => 1,
           Finish_Priority => 99)
      loop
         Manager.Log
           ("priority" & Natural'Image (Goal.Priority)
            & " goal: " & Harriet.Db.Record_Type'Image (Goal.Top_Record));
      end loop;

   end Activate;

   ---------------------------
   -- Default_Fleet_Manager --
   ---------------------------

   function Default_Fleet_Manager
     (Faction : Harriet.Db.Faction_Reference) return Manager_Type
   is
   begin
      return new Default_Fleet_Manager_Type'
        (Root_Manager_Type with
           Faction         => Faction);
   end Default_Fleet_Manager;

end Harriet.Managers.Fleets;
