with Harriet.Logging;
with Harriet.Updates.Events;

package body Harriet.Managers is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
      use type Harriet.Db.Managed_Reference;
      Has_Managed : constant Boolean :=
        Update.Manager.Managed /= Harriet.Db.Null_Managed_Reference;
   begin
      Update.Manager.Has_Next_Update := False;
      Update.Manager.Activate;
      Update.Manager.Signaled := False;

      if Update.Manager.Has_Next_Update then
         Update.Manager.Is_Active := True;
         Harriet.Updates.Events.Update_At
           (Clock  => Update.Manager.Next_Update,
            Update => Update);
         if Has_Managed then
            Harriet.Db.Managed.Update_Managed (Update.Manager.Managed)
              .Set_Next_Event (Update.Manager.Next_Update)
              .Set_Active (True)
              .Done;
         end if;
      else
         Harriet.Logging.Log
           (Category => Update.Manager.Identifier,
            Message  => "deactivating");
         if Has_Managed then
            Harriet.Db.Managed.Update_Managed (Update.Manager.Managed)
              .Set_Active (False)
              .Done;
         end if;
      end if;

   end Activate;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Manager_Type;
      Message : String)
   is
   begin
      Harriet.Logging.Log
        (Category => Root_Manager_Type'Class (Manager).Identifier,
         Message  => Message);
   end Log;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration)
   is
      use type Harriet.Calendar.Time;
   begin
      Manager.Set_Next_Update_Time (Harriet.Calendar.Clock + Update_Delay);
   end Set_Next_Update_Delay;

   --------------------------
   -- Set_Next_Update_Time --
   --------------------------

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Harriet.Calendar.Time)
   is
   begin
      if Manager.Is_Active then
         Manager.Has_Next_Update := True;
         Manager.Next_Update := Update_Time;
      else
         declare
            Update : constant Manager_Update :=
                       (Manager => Manager_Type (Manager));
         begin
            Manager.Is_Active := True;
            Harriet.Updates.Events.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;

   end Set_Next_Update_Time;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (Faction : Harriet.Db.Faction_Reference;
      Area    : Middle_Manager_Area)
   is
      Manager : constant Manager_Type :=
        Active_Middle (Area).Element (Harriet.Db.To_String (Faction));
   begin
      if not Manager.Signaled then
         Manager.Log ("signal");
         Manager.Signaled := True;
         Manager.Is_Active := True;
         Manager.Has_Next_Update := True;
         Manager.Next_Update := Harriet.Calendar.Clock;
         Harriet.Updates.Events.Update_At
           (Clock  => Harriet.Calendar.Clock,
            Update => Manager_Update'
              (Manager => Manager));
      end if;
   end Signal;

end Harriet.Managers;
