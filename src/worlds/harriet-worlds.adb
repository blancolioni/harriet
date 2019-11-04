with Harriet.Calendar;
with Harriet.Elementary_Functions;
with Harriet.Logging;
with Harriet.Orbits;

with Harriet.Configure.Worlds;

with Harriet.Db.Generation;
with Harriet.Db.Massive_Object;
with Harriet.Db.Sector_Neighbour;
with Harriet.Db.Sector_Use;
with Harriet.Db.Sector_Vertex;
with Harriet.Db.World;

package body Harriet.Worlds is

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.World_Sector_Reference,
        Harriet.Db."=");

   -----------------
   -- Best_Sector --
   -----------------

   function Best_Sector
     (World : Harriet.Db.World_Reference;
      Score : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Real)
      return Harriet.Db.World_Sector_Reference
   is
      Best_Reference : Harriet.Db.World_Sector_Reference :=
                         Harriet.Db.Null_World_Sector_Reference;
      Best_Score     : Real := Real'First;
   begin
      Check_Surface (World);
      for Sector of
        Harriet.Db.World_Sector.Select_By_World
          (World)
      loop
         declare
            This_Score : constant Real :=
                           Score (Sector);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Reference := Sector.Get_World_Sector_Reference;
            end if;
         end;
      end loop;
      return Best_Reference;
   end Best_Sector;

   -------------------
   -- Check_Surface --
   -------------------

   procedure Check_Surface
     (World : Harriet.Db.World_Reference)
   is
      Is_Gen : constant Harriet.Db.Is_Generated_Reference :=
                 Harriet.Db.World.Get (World).Get_Is_Generated_Reference;
      Gen    : constant Harriet.Db.Generation.Generation_Type :=
                 Harriet.Db.Generation.Get_By_Is_Generated
                   (Is_Gen);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Harriet.Logging.Log
           ("generate", "starting " & Name (World));
         Harriet.Configure.Worlds.Generate_Surface (World);
         if Gen.Has_Element then
            Harriet.Db.Generation.Update_Generation
              (Gen.Get_Generation_Reference)
              .Set_Ready (True)
              .Done;
         else
            Harriet.Db.Generation.Create (Is_Gen, True);
         end if;
         Harriet.Logging.Log
           ("generate", "finished " & Name (World));
      end if;
   end Check_Surface;

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
        Harriet.Db.Massive_Object.Get (W1.Primary).Mass;
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

   -----------------
   -- Find_Sector --
   -----------------

   function Find_Sector
     (World : Harriet.Db.World_Reference;
      Test  : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Boolean)
      return Harriet.Db.World_Sector_Reference
   is
   begin
      for Sector of
        Harriet.Db.World_Sector.Select_By_World
          (World)
      loop
         if Test (Sector) then
            return Sector.Get_World_Sector_Reference;
         end if;
      end loop;
      return Harriet.Db.Null_World_Sector_Reference;
   end Find_Sector;

   -----------------------------
   -- Get_Average_Temperature --
   -----------------------------

   function Get_Average_Temperature
     (Sector : Harriet.Db.World_Sector_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Average_Temperature;
   end Get_Average_Temperature;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex
   is
      Rec : constant Harriet.Db.World_Sector.World_Sector_Type :=
              Harriet.Db.World_Sector.Get (Sector);
   begin
      return Sector_Vertex'
        (Rec.X, Rec.Y, Rec.Z);
   end Get_Centre;

   -------------------
   -- Get_Elevation --
   -------------------

   function Get_Elevation
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Elevation_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Elevation;
   end Get_Elevation;

   --------------------
   -- Get_Neighbours --
   --------------------

   function Get_Neighbours
     (Sector : Harriet.Db.World_Sector_Reference)
      return World_Sector_Array
   is
      Result : World_Sector_Array (1 .. 20);
      Count  : Natural := 0;
   begin
      for Neighbour of
        Harriet.Db.Sector_Neighbour.Select_By_Sector
          (Harriet.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         declare
            Neighbour_Ref : constant Db.Sector_Reference :=
                              Neighbour.Neighbour;
            Neighbour_Sec : constant Db.World_Sector.World_Sector_Type :=
                              Db.World_Sector.Get_World_Sector
                                (Neighbour_Ref);
            World_Sec_Ref : constant Db.World_Sector_Reference :=
                              Neighbour_Sec.Get_World_Sector_Reference;
         begin
            Result (Count) := World_Sec_Ref;
         end;
      end loop;
      return Result (1 .. Count);
   end Get_Neighbours;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Faction_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Faction;
   end Get_Owner;

   -----------------
   -- Get_Terrain --
   -----------------

   function Get_Terrain
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Terrain_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).Terrain;
   end Get_Terrain;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex_Array
   is
      Count  : Natural := 0;
      Result : Sector_Vertex_Array (1 .. 10);
   begin
      for Vertex of
        Harriet.Db.Sector_Vertex.Select_By_Sector
          (Harriet.Db.World_Sector.Get (Sector).Get_Sector_Reference)
      loop
         Count := Count + 1;
         Result (Count) := (Vertex.X, Vertex.Y, Vertex.Z);
      end loop;
      return Result (1 .. Count);
   end Get_Vertices;

   ---------------
   -- Get_World --
   ---------------

   function Get_World
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.World_Reference
   is
   begin
      return Harriet.Db.World_Sector.Get (Sector).World;
   end Get_World;

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

   ------------------
   -- Is_Gas_Giant --
   ------------------

   function Is_Gas_Giant
     (World : Harriet.Db.World_Reference)
      return Boolean
   is
   begin
      return Harriet.Db.World.Get (World).Gas_Giant;
   end Is_Gas_Giant;

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

   --------------
   -- Is_Urban --
   --------------

   function Is_Urban
     (Sector : Harriet.Db.World_Sector_Reference)
      return Boolean
   is
   begin
      return Harriet.Db.Sector_Use.Get
        (Harriet.Db.World_Sector.Get (Sector).Sector_Use)
          .Tag = "urban";
   end Is_Urban;

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

   ------------------
   -- Scan_Surface --
   ------------------

   procedure Scan_Surface
     (World   : Harriet.Db.World_Reference;
      Process : not null access
        procedure (Sector : Harriet.Db.World_Sector_Reference))
   is
      List : World_Sector_Lists.List;
   begin
      Check_Surface (World);
      for Sector of Harriet.Db.World_Sector.Select_By_World (World) loop
         List.Append (Sector.Get_World_Sector_Reference);
      end loop;

      for Sector of List loop
         Process (Sector);
      end loop;

   end Scan_Surface;

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
