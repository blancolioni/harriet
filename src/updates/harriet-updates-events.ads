with Harriet.Calendar;

package Harriet.Updates.Events is

   procedure Update_At
     (Clock  : Harriet.Calendar.Time;
      Update : Update_Interface'Class);

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class);

end Harriet.Updates.Events;
