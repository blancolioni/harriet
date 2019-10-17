with Ada.Calendar;

with Harriet.Updates.Control;

package body Harriet.Sessions.Status is

   Advance_Speed : constant array (1 .. 2) of Duration :=
     (1 => 3600.0,
      2 => 84_600.0);

   ----------------------
   -- Get_Update_Speed --
   ----------------------

   function Get_Update_Speed
     (Session : Root_Harriet_Session'Class)
      return Json.Json_Value'Class
   is
      pragma Unreferenced (Session);
      Start_Time         : Ada.Calendar.Time;
      Paused             : Boolean;
      Advance_Per_Second : Duration;
      Speed              : Natural := 0;
   begin
      Updates.Control.Get_Status (Start_Time, Paused, Advance_Per_Second);
      if not Paused then
         for I in Advance_Speed'Range loop
            if Advance_Per_Second <= Advance_Speed (I) then
               Speed := I;
               exit;
            end if;
         end loop;
         if Speed = 0 then
            Speed := Advance_Speed'Last;
         end if;
      end if;

      return Json.Integer_Value (Speed);

   end Get_Update_Speed;

   ----------------------
   -- Set_Update_Speed --
   ----------------------

   procedure Set_Update_Speed
     (Session : in out Root_Harriet_Session'Class;
      Value   :        Json.Json_Value'Class)
   is
      pragma Unreferenced (Session);
      New_Speed : Natural;
   begin
      New_Speed := Natural'Value (Json.Image (Value));
      if New_Speed = 0 then
         Updates.Control.Pause_Updates;
      elsif New_Speed in Advance_Speed'Range then
         Updates.Control.Set_Advance_Speed (Advance_Speed (New_Speed));
         Updates.Control.Resume_Updates;
      else
         raise Constraint_Error with
           "invalid update speed:" & New_Speed'Image;
      end if;

   exception
      when others =>
         raise Constraint_Error with
           "invalid value for update speed: "
           & Json.Image (Value);
   end Set_Update_Speed;

end Harriet.Sessions.Status;
