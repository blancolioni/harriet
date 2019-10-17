with Harriet.Updates.Tasks;

package body Harriet.Updates.Events is

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Harriet.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Harriet.Updates.Tasks.Update_Map.Add_Update (Clock, Update);
   end Update_At;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class)
   is
      use type Harriet.Calendar.Time;
   begin
      Harriet.Updates.Tasks.Update_Map.Add_Update
        (Harriet.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Harriet.Updates.Events;
