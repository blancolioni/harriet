with Harriet.Elementary_Functions;
with Harriet.Real_Images;
with Harriet.Solar_System;

with Harriet.Star_Systems;

with Harriet.Db.Can_Carry;
with Harriet.Db.Has_Orbits;
with Harriet.Db.Location;
with Harriet.Db.Ship;
with Harriet.Db.Star;
with Harriet.Db.Star_System;
with Harriet.Db.Star_System_Object;
with Harriet.Db.World;

package body Harriet.Locations is

   procedure Location_XYZ
     (Location : Harriet.Db.Location_Reference;
      X, Y, Z  : out Real);

   procedure System_XYZ
     (Location : Harriet.Db.Location_Reference;
      X, Y, Z  : out Real);

   -----------
   -- Clear --
   -----------

   procedure Clear (Location : Harriet.Db.Location_Reference) is
   begin
      Harriet.Db.Location.Update_Location (Location)
        .Set_Class (Harriet.Db.Nowhere)
        .Set_Has_Orbits (Harriet.Db.Null_Has_Orbits_Reference)
        .Set_Can_Carry (Harriet.Db.Null_Can_Carry_Reference)
        .Done;
   end Clear;

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Harriet.Db.Location_Reference) return Non_Negative_Real
   is
      use Harriet.Elementary_Functions;
      X1, Y1, Z1 : Real;
      X2, Y2, Z2 : Real;
   begin
      if Same_System (From, To) then
         Location_XYZ (From, X1, Y1, Z1);
         Location_XYZ (To, X2, Y2, Z2);
      else
         System_XYZ (From, X1, Y1, Z1);
         System_XYZ (To, X2, Y2, Z2);
      end if;
      return Sqrt ((X1 - X2) ** 2 + (Y1 - Y2) ** 2 + (Z1 - Z2) ** 2);
   end Distance;

   -------------------------
   -- Get_Primary_Massive --
   -------------------------

   function Get_Primary_Massive
     (Location : Harriet.Db.Location_Reference) return Harriet.Db
     .Massive_Object_Reference
   is
   begin
      return Harriet.Db.Has_Orbits.Get
        (Harriet.Db.Location.Get (Location).Has_Orbits)
          .Get_Massive_Object_Reference;
   end Get_Primary_Massive;

   ----------------
   -- Get_System --
   ----------------

   function Get_System
     (Location : Harriet.Db.Location_Reference)
      return Harriet.Db.Star_System_Reference
   is
      use all type Harriet.Db.Location_Class;
      use all type Harriet.Db.Record_Type;
      Rec : constant Harriet.Db.Location.Location_Type :=
        Harriet.Db.Location.Get (Location);
   begin
      case Rec.Class is
         when Nowhere =>
            return Harriet.Db.Null_Star_System_Reference;
         when In_Orbit =>
            declare
               Has_Orbits : constant Harriet.Db.Has_Orbits.Has_Orbits_Type :=
                 Harriet.Db.Has_Orbits.Get (Rec.Has_Orbits);
            begin
               case Has_Orbits.Top_Record is
                  when R_World =>
                     return Harriet.Db.World.Get_World (Rec.Has_Orbits)
                       .Star_System;
                  when R_Star  =>
                     return Harriet.Db.Star.Get_Star (Rec.Has_Orbits)
                       .Star_System;
                  when others =>
                     return Harriet.Db.Null_Star_System_Reference;
               end case;
            end;
         when System_Point =>
            return Harriet.Db.Star.Get_Star (Rec.Has_Orbits).Star_System;
         when Carried =>
            declare
               Can_Carry : constant Harriet.Db.Can_Carry.Can_Carry_Type :=
                 Harriet.Db.Can_Carry.Get (Rec.Can_Carry);
            begin
               case Can_Carry.Top_Record is
                  when R_Ship =>
                     return Get_System
                       (Harriet.Db.Ship.Get_Ship (Rec.Can_Carry).Location);
                  when others =>
                     return Harriet.Db.Null_Star_System_Reference;
               end case;
            end;
      end case;
   end Get_System;

   ---------------
   -- Get_World --
   ---------------

   function Get_World
     (Location : Harriet.Db.Location_Reference) return Harriet.Db
     .World_Reference
   is
   begin
      return Harriet.Db.World.Get_World
        (Harriet.Db.Location.Get (Location).Has_Orbits)
          .Get_World_Reference;
   end Get_World;

   ------------------
   -- Has_Location --
   ------------------

   function Has_Location
     (Location : Harriet.Db.Location_Reference)
      return Boolean
   is
      use Harriet.Db;
   begin
      return Location /= Harriet.Db.Null_Location_Reference
        and then Harriet.Db.Location.Get (Location).Class /= Nowhere;
   end Has_Location;

   ---------------
   -- Has_World --
   ---------------

   function Has_World
     (Location : Harriet.Db.Location_Reference)
      return Boolean
   is
   begin
      if not Has_Location (Location) then
         return False;
      end if;

      declare
         use Harriet.Db;
         Rec : constant Harriet.Db.Location.Location_Type :=
           Harriet.Db.Location.Get (Location);
      begin
         return Rec.Class = In_Orbit
           and then Harriet.Db.Has_Orbits.Get (Rec.Has_Orbits).Top_Record
           = R_World;
      end;
   end Has_World;

   ------------------
   -- Location_XYZ --
   ------------------

   procedure Location_XYZ
     (Location : Harriet.Db.Location_Reference;
      X, Y, Z  : out Real)
   is
      use all type Harriet.Db.Location_Class;
      Rec : constant Harriet.Db.Location.Location_Type :=
        Harriet.Db.Location.Get (Location);
   begin
      X := 0.0;
      Y := 0.0;
      Z := 0.0;

      case Rec.Class is
         when Nowhere =>
            null;

         when In_Orbit =>
            declare
               use Harriet.Db.Star_System_Object;
               Object : constant Star_System_Object_Type :=
                 Get_Star_System_Object (Rec.Has_Orbits);
            begin
               if Object.Has_Element then
                  Harriet.Star_Systems.Get_Current_System_Position
                    (Object.Get_Star_System_Object_Reference,
                     X, Y, Z);
               end if;
            end;

         when System_Point =>
            X := Rec.X;
            Y := Rec.Y;
            Z := Rec.Z;

         when Carried =>
            declare
               Can_Carry : constant Harriet.Db.Can_Carry.Can_Carry_Type :=
                 Harriet.Db.Can_Carry.Get (Rec.Can_Carry);
            begin
               case Can_Carry.Top_Record is
                  when Harriet.Db.R_Ship =>
                     Location_XYZ
                       (Harriet.Db.Ship.Get_Ship (Rec.Can_Carry).Location,
                        X, Y, Z);
                  when others =>
                     null;
               end case;
            end;
      end case;
   end Location_XYZ;

   ------------------
   -- New_Location --
   ------------------

   function New_Location return Harriet.Db.Location_Reference is
      use Harriet.Db;
   begin
      return Harriet.Db.Location.Create
        (Class      => Nowhere,
         Has_Orbits => Null_Has_Orbits_Reference,
         Can_Carry  => Null_Can_Carry_Reference,
         Orbit      => 0.0,
         X          => 0.0,
         Y          => 0.0,
         Z          => 0.0);
   end New_Location;

   -----------------
   -- Same_System --
   -----------------

   function Same_System
     (Location_1, Location_2 : Harriet.Db.Location_Reference) return Boolean
   is
      use type Harriet.Db.Star_System_Reference;
   begin
      return Get_System (Location_1) = Get_System (Location_2);
   end Same_System;

   -------------------------
   -- Set_System_Location --
   -------------------------

   procedure Set_System_Location
     (Location    : Harriet.Db.Location_Reference;
      Star_System : Harriet.Db.Star_System_Reference;
      X, Y, Z     : Real)
   is
   begin
      Harriet.Db.Location.Update_Location (Location)
        .Set_Class (Harriet.Db.System_Point)
        .Set_Has_Orbits
          (Harriet.Db.Star.First_By_Star_System (Star_System)
           .Get_Has_Orbits_Reference)
        .Set_Can_Carry (Harriet.Db.Null_Can_Carry_Reference)
        .Set_X (X)
        .Set_Y (Y)
        .Set_Z (Z)
        .Done;
   end Set_System_Location;

   ------------------------------
   -- Set_World_Orbit_Location --
   ------------------------------

   procedure Set_World_Orbit_Location
     (Location : Harriet.Db.Location_Reference;
      World    : Harriet.Db.World_Reference;
      Orbit    : Non_Negative_Real)
   is
   begin
      Harriet.Db.Location.Update_Location (Location)
        .Set_Class (Harriet.Db.In_Orbit)
        .Set_Has_Orbits
          (Harriet.Db.World.Get (World).Get_Has_Orbits_Reference)
        .Set_Orbit (Orbit)
        .Set_Can_Carry (Harriet.Db.Null_Can_Carry_Reference)
        .Done;
   end Set_World_Orbit_Location;

   ----------
   -- Show --
   ----------

   function Show (Location : Harriet.Db.Location_Reference) return String is
      use Harriet.Db;
      AU  : constant Non_Negative_Real :=
        Harriet.Solar_System.Earth_Orbit;
      Rec : constant Harriet.Db.Location.Location_Type :=
        Harriet.Db.Location.Get (Location);

      function Image (X : Real) return String
                      renames Harriet.Real_Images.Approximate_Image;

   begin
      case Rec.Class is
         when Nowhere =>
            return "nowhere";
         when In_Orbit =>
            declare
               Has_Orbits : constant Harriet.Db.Has_Orbits.Has_Orbits_Type :=
                 Harriet.Db.Has_Orbits.Get (Rec.Has_Orbits);
               Name       : constant String :=
                 (case Has_Orbits.Top_Record is
                     when R_World =>
                       Harriet.Db.World.Get_World (Rec.Has_Orbits).Name,
                     when R_Star  =>
                       Harriet.Db.Star.Get_Star (Rec.Has_Orbits).Name,
                     when others  =>
                       "some sort of object");
               Distance   : constant String :=
                 (if Rec.Orbit < AU / 100.0
                  then Image (Rec.Orbit / 1000.0) & "km"
                  else Image (Rec.Orbit / AU) & "AU");
            begin
               return "orbiting " & Name & " at " & Distance;
            end;
         when System_Point =>
            declare
               use Harriet.Elementary_Functions;
               Distance   : constant Non_Negative_Real :=
                 Sqrt (Rec.X ** 2 + Rec.Y ** 2 + Rec.Z ** 2);
               Distance_Image : constant String :=
                 (if Distance < AU / 100.0
                  then Image (Distance / 1000.0) & "km"
                  else Image (Distance / AU) & "AU");
            begin
               return Distance_Image & " from "
                 & Harriet.Db.Star.Get_Star (Rec.Has_Orbits).Name;
            end;

         when Carried =>
            return "being carried";
      end case;
   end Show;

   ----------------
   -- System_XYZ --
   ----------------

   procedure System_XYZ
     (Location : Harriet.Db.Location_Reference;
      X, Y, Z  : out Real)
   is
      use type Harriet.Db.Star_System_Reference;
      System : constant Harriet.Db.Star_System_Reference :=
        Get_System (Location);
   begin
      if System = Harriet.Db.Null_Star_System_Reference then
         X := 0.0;
         Y := 0.0;
         Z := 0.0;
      else
         declare
            Rec : constant Harriet.Db.Star_System.Star_System_Type :=
              Harriet.Db.Star_System.Get (System);
         begin
            X := Rec.X;
            Y := Rec.Y;
            Z := Rec.Z;
         end;
      end if;
   end System_XYZ;

end Harriet.Locations;
