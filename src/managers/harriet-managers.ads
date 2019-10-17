private with WL.String_Maps;

with Harriet.Calendar;
with Harriet.Updates;

with Harriet.Db.Managed;

package Harriet.Managers is

   type Root_Manager_Type is abstract tagged private;

   function Identifier
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   procedure Activate
     (Manager : not null access Root_Manager_Type)
   is abstract;

   procedure Log
     (Manager : Root_Manager_Type;
      Message : String);

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Harriet.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Constructor_Function is access
     function (Managed : Harriet.Db.Managed_Reference)
               return Manager_Type;

private

   type Root_Manager_Type is abstract tagged
      record
         Managed         : Harriet.Db.Managed_Reference :=
                             Harriet.Db.Null_Managed_Reference;
         Is_Active       : Boolean := False;
         Has_Next_Update : Boolean := False;
         Next_Update     : Harriet.Calendar.Time;
      end record;

   package Register_Maps is
     new WL.String_Maps (Constructor_Function);

   Register : Register_Maps.Map;

   package Manager_Maps is
     new WL.String_Maps (Manager_Type);

   Active_Map : Manager_Maps.Map;

   type Manager_Update is
     new Harriet.Updates.Update_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Activate
     (Update : Manager_Update);

   function Managed_Key
     (Managed : Harriet.Db.Managed.Managed_Type)
      return String
   is (Managed.Manager & Managed.Identity);

end Harriet.Managers;
