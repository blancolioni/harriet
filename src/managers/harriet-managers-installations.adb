with Harriet.Db.Colony;
with Harriet.Db.Installation;
with Harriet.Db.Resource;

package body Harriet.Managers.Installations is

   type Root_Installation_Manager is abstract new Root_Manager_Type with
      record
         Has_Stock     : Harriet.Db.Has_Stock_Reference;
         Facility      : Harriet.Db.Facility_Reference;
         Installation  : Harriet.Db.Installation_Reference;
         World         : Harriet.Db.World_Reference;
      end record;

   type Root_Mine_Manager is new Root_Installation_Manager with
      record
         Resource  : Harriet.Db.Resource_Reference;
         Commodity : Harriet.Db.Commodity_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Mine_Manager)
      return String
   is ("mine manager");

   overriding procedure Activate
     (Manager : not null access Root_Mine_Manager);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Mine_Manager)
   is
   begin
      Manager.Set_Next_Update_Delay (Harriet.Calendar.Days (1));
   end Activate;

   ---------------------------------
   -- Create_Default_Mine_Manager --
   ---------------------------------

   function Create_Default_Mine_Manager
     (Managed : Harriet.Db.Managed_Reference)
      return Manager_Type
   is
      Mine  : constant Harriet.Db.Installation.Installation_Type :=
                  Harriet.Db.Installation.Get_Installation (Managed);
      Manager : constant Root_Mine_Manager :=
                    Root_Mine_Manager'
                      (Root_Manager_Type with
                       Installation     => Mine.Get_Installation_Reference,
                       World            => Mine.World,
                       Resource         => Mine.Resource,
                       Facility         => Mine.Facility,
                       Has_Stock        =>
                         Harriet.Db.Colony.Get (Mine.Colony)
                       .Get_Has_Stock_Reference,
                       Commodity        =>
                         Harriet.Db.Resource.Get (Mine.Resource)
                       .Get_Commodity_Reference);
   begin
      return new Root_Mine_Manager'(Manager);
   end Create_Default_Mine_Manager;

end Harriet.Managers.Installations;
