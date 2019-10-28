with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Orbits;

with Harriet.Db.Star_System_Object;
with Harriet.Db.World;

package body Harriet.Worlds is

   -----------
   -- Clear --
   -----------

   procedure Clear (Selection : in out World_Selection'Class) is
   begin
      Selection.List.Clear;
   end Clear;

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Harriet.Db.World_Reference)
      return Non_Negative_Real
   is
      use Harriet.Db, Harriet.Calendar, Harriet.Elementary_Functions;
      W1 : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (From);
      W2 : constant Harriet.Db.World.World_Type :=
        Harriet.Db.World.Get (To);
      R1 : constant Non_Negative_Real := W1.Semimajor_Axis;
      R2 : constant Non_Negative_Real := W2.Semimajor_Axis;
      pragma Assert (W1.Primary = W2.Primary);
      Primary_Mass : constant Non_Negative_Real :=
        Harriet.Db.Star_System_Object.Get (W1.Primary).Mass;
      From_Angle : constant Real :=
        Harriet.Orbits.Calculate_Longitude
          (Large_Mass => Primary_Mass,
           Orbit      => W1.Semimajor_Axis,
           Elapsed    =>
             Harriet.Calendar.Clock - W1.Epoch);
      To_Angle     : constant Real :=
        Harriet.Orbits.Calculate_Longitude
          (Large_Mass => Primary_Mass,
           Orbit      => W2.Semimajor_Axis,
           Elapsed    =>
             Harriet.Calendar.Clock - W1.Epoch);
      X1           : constant Real := R1 * Cos (From_Angle, 360.0);
      Y1           : constant Real := R1 * Sin (From_Angle, 360.0);
      X2           : constant Real := R2 * Cos (To_Angle, 360.0);
      Y2           : constant Real := R2 * Sin (To_Angle, 360.0);
      D            : constant Non_Negative_Real :=
        Sqrt ((X1 - X2) ** 2 + (Y1 - Y2) ** 2);
   begin
      return D;
   end Distance;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Harriet.Db.World_Reference)
      return Boolean)
   is
      New_List : World_Lists.List;
      Changed  : Boolean := False;
   begin
      for World of Selection.List loop
         if Test (World) then
            New_List.Append (World);
         else
            Changed := True;
         end if;
      end loop;
      if Changed then
         Selection.List := New_List;
      end if;
   end Filter;

   ----------------
   -- Get_Worlds --
   ----------------

   function Get_Worlds
     (Selection : World_Selection'Class)
      return World_Array
   is
      Index : Natural := 0;
   begin
      return Arr : World_Array (1 .. Natural (Selection.List.Length)) do
         for World of Selection.List loop
            Index := Index + 1;
            Arr (Index) := World;
         end loop;
      end return;
   end Get_Worlds;

   ------------------
   -- Habitability --
   ------------------

   function Habitability
     (World : Harriet.Db.World_Reference)
      return Unit_Real
   is
   begin
      return Harriet.Db.World.Get (World).Habitability;
   end Habitability;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Harriet.Db.World_Reference)
   is
   begin
      Selection.List.Append (World);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Selection : World_Selection'Class) return Boolean is
   begin
      return Selection.List.Is_Empty;
   end Is_Empty;

   --------------------
   -- Is_Terrestrial --
   --------------------

   function Is_Terrestrial
     (World : Harriet.Db.World_Reference)
      return Boolean
   is
      use type Harriet.Db.World_Climate;
   begin
      return Harriet.Db.World.Get (World).Climate =
        Harriet.Db.Temperate;
   end Is_Terrestrial;

   ----------
   -- Mass --
   ----------

   function Mass
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.World.Get (World).Mass;
   end Mass;

   ----------
   -- Name --
   ----------

   function Name
     (World : Harriet.Db.World_Reference)
      return String
   is
   begin
      return Harriet.Db.World.Get (World).Name;
   end Name;

   ------------
   -- Radius --
   ------------

   function Radius
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.World.Get (World).Radius;
   end Radius;

   -----------------
   -- Star_System --
   -----------------

   function Star_System
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.World.Get (World).Star_System;
   end Star_System;

end Harriet.Worlds;
