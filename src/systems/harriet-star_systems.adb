with Harriet.Elementary_Functions;
with Harriet.Orbits;

with Harriet.Db.Faction;
with Harriet.Db.Massive_Object;
with Harriet.Db.Scenario;
with Harriet.Db.Star;
with Harriet.Db.Star_System;
with Harriet.Db.Star_System_Object;
with Harriet.Db.World;

package body Harriet.Star_Systems is

   First_Star_System : Harriet.Db.Star_System_Reference :=
                         Harriet.Db.Null_Star_System_Reference;

   -----------
   -- Claim --
   -----------

   procedure Claim
     (Star_System : Harriet.Db.Star_System_Reference)
   is
   begin
      Harriet.Db.Star_System.Update_Star_System (Star_System)
        .Set_Claimed (True)
        .Done;
   end Claim;

   -------------
   -- Claimed --
   -------------

   function Claimed
     (Star_System : Harriet.Db.Star_System_Reference)
      return Boolean
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System).Claimed;
   end Claimed;

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Harriet.Db.Star_System_Reference)
      return Non_Negative_Real
   is
      S1 : constant Harriet.Db.Star_System.Star_System_Type :=
        Harriet.Db.Star_System.Get (From);
      S2 : constant Harriet.Db.Star_System.Star_System_Type :=
        Harriet.Db.Star_System.Get (To);
   begin
      return Harriet.Elementary_Functions.Sqrt
        ((S1.X - S2.X) ** 2 + (S1.Y - S2.Y) ** 2 + (S1.Z - S2.Z) ** 2);
   end Distance;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact
     (Name : String)
      return Harriet.Db.Star_System_Reference
   is
   begin
      return Harriet.Db.Star_System.First_Reference_By_Name (Name);
   end Find_Exact;

   -----------
   -- First --
   -----------

   function First return Harriet.Db.Star_System_Reference is
      use Harriet.Db;
   begin
      if First_Star_System = Null_Star_System_Reference then
         declare
            Scenario : constant Harriet.Db.Scenario.Scenario_Type :=
                         Harriet.Db.Scenario.Get_By_Active (True);
         begin
            if Scenario.Has_Element then
               First_Star_System := Scenario.Central_System;
            end if;
         end;
      end if;

      if First_Star_System = Null_Star_System_Reference then
         First_Star_System :=
           Harriet.Db.Star_System.First_Reference_By_Top_Record
             (Harriet.Db.R_Star_System);
      end if;

      return First_Star_System;
   end First;

   ---------
   -- Get --
   ---------

   function Get
     (Reference : Harriet.Db.Star_System_Reference)
      return Star_System_Type
   is
   begin
      return Star_System_Type'(Reference => Reference);
   end Get;

   procedure Get_Current_System_Position
     (Object  : Harriet.Db.Star_System_Object_Reference;
      X, Y, Z : out Real)
   is
      use Harriet.Db, Harriet.Db.Star_System_Object;
      Rec : constant Star_System_Object_Type := Get (Object);
   begin
      if Rec.Primary = Null_Star_System_Object_Reference then
         X := 0.0;
         Y := 0.0;
         Z := 0.0;
         return;
      end if;

      Get_Current_System_Position (Rec.Primary, X, Y, Z);

      declare
         use Harriet.Elementary_Functions;
         Longitude : constant Real :=
           Harriet.Orbits.Calculate_Current_Longitude
             (Harriet.Db.Massive_Object.Get (Rec.Primary_Massive).Mass,
              Rec.Semimajor_Axis,
              Rec.Epoch);
      begin
         X := X + Rec.Semimajor_Axis * Cos (Longitude, 360.0);
         Y := Y + Rec.Semimajor_Axis * Sin (Longitude, 360.0);
      end;

   end Get_Current_System_Position;

   ---------------------
   -- Has_Star_System --
   ---------------------

   function Has_Star_System
     (Star_System : Star_System_Type'Class)
      return Boolean
   is
      use type Harriet.Db.Star_System_Reference;
   begin
      return Star_System.Reference /= Harriet.Db.Null_Star_System_Reference;
   end Has_Star_System;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Star_System_Type'Class)
      return String
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System.Reference).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Star_System : Harriet.Db.Star_System_Reference)
      return String
   is
   begin
      return Harriet.Db.Star_System.Get (Star_System).Name;
   end Name;

   function Owner
     (Star_System : Star_System_Type'Class)
      return Harriet.Factions.Faction_Type'Class
   is
   begin
      return Harriet.Factions.Get
        (Harriet.Db.Faction.First_Reference_By_Capital_System
           (Star_System.Reference));
   end Owner;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Harriet.Db.Star_System_Reference)
      return Interstellar_Position
   is
      Rec : constant Harriet.Db.Star_System.Star_System_Type :=
              Harriet.Db.Star_System.Get (Star_System);
   begin
      return (Rec.X, Rec.Y, Rec.Z);
   end Position;

   --------------
   -- Position --
   --------------

   function Position
     (Star_System : Star_System_Type'Class)
      return Interstellar_Position
   is
   begin
      return Position (Star_System.Reference);
   end Position;

   -------------
   -- Primary --
   -------------

   function Primary
     (Star_System : Star_System_Type'Class)
      return Harriet.Stars.Star_Type'Class
   is
   begin
      return Harriet.Stars.Get
        (Harriet.Db.Star.First_Reference_By_Star_System
           (Star_System.Reference));
   end Primary;

   ------------------------
   -- Terrestrial_Worlds --
   ------------------------

   function Terrestrial_Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection
   is
   begin
      return Selection : Harriet.Worlds.World_Selection :=
        Worlds (Star_System)
      do
         Selection.Filter (Harriet.Worlds.Is_Terrestrial'Access);
      end return;
   end Terrestrial_Worlds;

   ------------
   -- Worlds --
   ------------

   function Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection
   is
   begin
      return Selection : Harriet.Worlds.World_Selection do
         for World of
           Harriet.Db.World.Select_By_Star_System
             (Star_System)
         loop
            Selection.Insert (World.Get_World_Reference);
         end loop;
      end return;
   end Worlds;

end Harriet.Star_Systems;
