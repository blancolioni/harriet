with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Harriet.Random;
with Harriet.Updates.Events;

with Harriet.Db.Managed;

package body Harriet.Managers.Execution is

   type Check_Manager_Update is
     new Harriet.Updates.Update_Interface with null record;

   overriding procedure Activate
     (Update : Check_Manager_Update);

   package Managed_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.Managed_Reference,
        Harriet.Db."=");

   function Get_Manager_Name
     (Managed : Harriet.Db.Managed_Reference)
      return String;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Check_Manager_Update)
   is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of
        Harriet.Db.Managed.Select_By_Active_Scheduled (True, False)
      loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed.Get_Managed_Reference);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;

      Harriet.Updates.Events.Update_With_Delay
        (Wait   => Harriet.Calendar.Days (1.0),
         Update => Update);
   end Activate;

   ----------------------
   -- Get_Manager_Name --
   ----------------------

   function Get_Manager_Name
     (Managed : Harriet.Db.Managed_Reference)
      return String
   is
      Rec : constant Harriet.Db.Managed.Managed_Type :=
              Harriet.Db.Managed.Get (Managed);
   begin
      return Rec.Manager;
   end Get_Manager_Name;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
      List : Managed_Reference_Lists.List;
   begin
      for Managed of Harriet.Db.Managed.Scan_By_Top_Record loop
         if Register.Contains (Managed.Manager) then
            List.Append (Managed.Get_Managed_Reference);
         end if;
      end loop;

      for Managed of List loop
         Start_Manager (Managed);
      end loop;
      declare
         Update : Check_Manager_Update;
      begin
         Harriet.Updates.Events.Update_With_Delay
           (Wait   =>
              Harriet.Calendar.Days (Harriet.Random.Unit_Random + 0.5),
            Update => Update);
      end;

   end Load_Managers;

   -------------------
   -- Start_Manager --
   -------------------

   procedure Start_Manager
     (Managed : Harriet.Db.Managed_Reference)
   is
      Key  : constant String := Harriet.Db.To_String (Managed);
      Name : constant String := Get_Manager_Name (Managed);
      Manager : constant Manager_Type :=
                  Register.Element (Name) (Managed);

   begin
      if Manager /= null then
         declare
            Rec : constant Harriet.Db.Managed.Managed_Type :=
                    Harriet.Db.Managed.Get (Managed);
         begin
            Manager.Is_Active := Rec.Active;
            Manager.Managed := Rec.Get_Managed_Reference;
            Active_Map.Insert (Key, Manager);
            if Rec.Active then
               declare
                  Update : constant Manager_Update :=
                             (Manager => Manager);
               begin
                  Harriet.Updates.Events.Update_At
                    (Clock  => Rec.Next_Event,
                     Update => Update);
               end;
            end if;

            Harriet.Db.Managed.Update_Managed (Managed)
              .Set_Scheduled (Rec.Active)
              .Done;
         end;
      else
         declare
            Rec : constant Harriet.Db.Managed.Managed_Type :=
                    Harriet.Db.Managed.Get (Managed);
         begin
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot create manager '"
               & Rec.Manager
               & "' for "
               & Harriet.Db.Record_Type'Image
                 (Rec.Top_Record));
            Harriet.Db.Managed.Update_Managed (Managed)
              .Set_Active (False)
              .Set_Scheduled (False)
              .Done;
         end;
      end if;
   end Start_Manager;

end Harriet.Managers.Execution;
