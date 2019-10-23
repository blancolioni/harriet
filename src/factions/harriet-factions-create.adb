with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Sets;

with Harriet.Configure.Commodities;

with Harriet.Calendar;
with Harriet.Configure;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Random;

with Harriet.Ships;
with Harriet.Star_Systems;
with Harriet.Worlds;

with Harriet.Db.Colony;
with Harriet.Db.Facility;
with Harriet.Db.Faction;
with Harriet.Db.Installation;
with Harriet.Db.Script;
with Harriet.Db.Script_Line;
with Harriet.Db.Ship_Design;
with Harriet.Db.Star_Gate;
with Harriet.Db.Star_System;
with Harriet.Db.System_Knowledge;
with Harriet.Db.User;
with Harriet.Db.World;
with Harriet.Db.World_Knowledge;

package body Harriet.Factions.Create is

   function Find_Homeworld
     return Harriet.Db.World_Reference;

   procedure Initial_Colony
     (Config  : Tropos.Configuration;
      Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference);

   procedure Initial_Facilities
     (Config  : Tropos.Configuration;
      Colony  : Harriet.Db.Colony_Reference;
      World   : Harriet.Db.World_Reference);

   procedure Initial_Ships
     (Config  : Tropos.Configuration;
      Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference);

   procedure Initial_Installations
     (Count    : Natural;
      Facility : Harriet.Db.Facility_Reference;
      Colony   : Harriet.Db.Colony_Reference;
      World    : Harriet.Db.World_Reference);

   --------------------
   -- Create_Faction --
   --------------------

   function Create_Faction
     (User        : Harriet.Db.User_Reference;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Harriet.Color.Harriet_Color;
      Setup       : Tropos.Configuration)
      return Harriet.Db.Faction_Reference
   is
      use Harriet.Db;
      Capital : constant Harriet.Db.World_Reference :=
        Find_Homeworld;
   begin
      if Capital = Null_World_Reference then
         return Null_Faction_Reference;
      end if;

      declare
         Cash    : constant Harriet.Money.Money_Type :=
           Harriet.Configure.Configure_Money
             (Setup, "cash", 1000.0);
         System  : constant Harriet.Db.Star_System_Reference :=
           Harriet.Worlds.Star_System (Capital);
         Faction : constant Harriet.Db.Faction_Reference :=
           Harriet.Db.Faction.Create
             (Name             => Name,
              Adjective        =>
                (if Adjective = "" then Name else Adjective),
              Plural_Name      =>
                (if Plural_Name = "" then Name else Plural_Name),
              Active           => True,
              Scheduled        => False,
              Next_Event       => Harriet.Calendar.Clock,
              Manager          => "default-faction",
              Cash             => Cash,
              Red              => Color.Red,
              Green            => Color.Green,
              Blue             => Color.Blue,
              User             => User,
              Capital_System   => System,
              Capital_World    => Capital,
              Fleet_Manager    => "default-fleet",
              Army_Manager     => "default-army",
              Explore_Manager  => "default-exploration",
              Colonise_Manager => "default-colonisation");

         Script           : constant Harriet.Db.Script_Reference :=
           Harriet.Db.Script.Create ("rc", User);
         Line_Index       : Natural := 0;
         Has_Knowledge    : constant Harriet.Db.Has_Knowledge_Reference :=
           Harriet.Db.Faction.Get (Faction).Get_Has_Knowledge_Reference;
      begin

         Harriet.Db.System_Knowledge.Create
           (Has_Knowledge => Has_Knowledge,
            Knowable      =>
              Harriet.Db.Star_System.Get (System).Get_Knowable_Reference,
            Existence     => True,
            Current       => True,
            Faction       => Faction,
            Star_System   =>
              Harriet.Worlds.Star_System (Capital));

         for World of
           Harriet.Db.World.Select_By_Star_System (System)
         loop
            declare
               Is_Home_World : constant Boolean :=
                 World.Get_World_Reference = Capital;
            begin
               Harriet.Db.World_Knowledge.Create
                 (Faction        => Faction,
                  Has_Knowledge  => Has_Knowledge,
                  Knowable       => World.Get_Knowable_Reference,
                  Existence      => True,
                  Current        => Is_Home_World,
                  World          => World.Get_World_Reference,
                  Classification => True,
                  Orbit          => True,
                  Deposits       => Is_Home_World);
            end;

         end loop;

         Initial_Colony (Setup.Child ("home-colony"), Faction, Capital);

         if not Setup.Contains ("init-script") then
            Ada.Text_IO.Put_Line
              ("warning: no initial script in " & Setup.Config_Name);
         end if;

         for Command of Setup.Child ("init-script") loop
            Line_Index := Line_Index + 1;
            Harriet.Db.Script_Line.Create
              (Script => Script,
               Index  => Line_Index,
               Line   => Command.Config_Name);
         end loop;

         return Faction;
      end;
   end Create_Faction;

   ---------------------
   -- Create_Factions --
   ---------------------

   procedure Create_Factions
     (Faction_Config : Tropos.Configuration;
      Setup_Config   : Tropos.Configuration)
   is
   begin
      for Config of Faction_Config loop
         Ada.Text_IO.Put_Line
           ("new faction: " & Config.Get ("name"));
         declare
            use type Harriet.Db.Faction_Reference;
            User : constant Harriet.Db.User_Reference :=
                     Harriet.Db.User.Create
                       (Login         => Config.Config_Name,
                        Password      => "",
                        Administrator => False);
            Faction : constant Harriet.Db.Faction_Reference :=
                        Create_Faction
                          (User        => User,
                           Name        => Config.Get ("name"),
                           Adjective   =>
                             Config.Get ("adjective", Config.Get ("name")),
                           Plural_Name =>
                             Config.Get ("plural", Config.Get ("name")),
                           Color       =>
                             Harriet.Color.From_String
                               (Config.Get ("color", "#ff0000")),
                           Setup       => Setup_Config);
         begin
            if Faction = Harriet.Db.Null_Faction_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "failed to create faction");
            end if;
         end;
      end loop;
   end Create_Factions;

   --------------------
   -- Find_Homeworld --
   --------------------

   function Find_Homeworld
     return Harriet.Db.World_Reference
   is

      package Star_System_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Harriet.Db.Star_System_Reference,
           Harriet.Db."=");

      Queue : Star_System_Lists.List;
      Checked : WL.String_Sets.Set;

      function Check_World
        (World : Harriet.Db.World_Reference)
         return Boolean;

      -----------------
      -- Check_World --
      -----------------

      function Check_World
        (World : Harriet.Db.World_Reference)
         return Boolean
      is
      begin
         return Harriet.Worlds.Habitability (World) > 0.7;
      end Check_World;

   begin

      Queue.Append (Harriet.Star_Systems.First);
      Checked.Insert
        (Harriet.Star_Systems.Name (Harriet.Star_Systems.First));

      while not Queue.Is_Empty loop
         declare
            use Harriet.Star_Systems;
            Star_System : constant Harriet.Db.Star_System_Reference :=
                            Queue.First_Element;
         begin
            Queue.Delete_First;

            if not Claimed (Star_System) then
               declare
                  Selection : constant Harriet.Worlds.World_Selection :=
                                Harriet.Star_Systems.Terrestrial_Worlds
                                  (Star_System);
               begin
                  if not Selection.Is_Empty then
                     for W of Selection.Get_Worlds loop
                        if Check_World (W) then
                           Claim (Star_System);
                           return W;
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            for Neighbour of
              Harriet.Db.Star_Gate.Select_By_From (Star_System)
            loop
               declare
                  Neighbour_Name : constant String :=
                                     Name (Neighbour.To);
               begin
                  if not Checked.Contains (Neighbour_Name) then
                     Checked.Insert (Neighbour_Name);
                     Queue.Append (Neighbour.To);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Harriet.Db.Null_World_Reference;
   end Find_Homeworld;

   --------------------
   -- Initial_Colony --
   --------------------

   procedure Initial_Colony
     (Config  : Tropos.Configuration;
      Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference)
   is
      use type Harriet.Calendar.Time;

      function Get (Field : String) return Real
      is (Real (Float'(Config.Get (Field))));

      Colony : constant Harriet.Db.Colony_Reference :=
                 Harriet.Db.Colony.Create
                   (Faction          => Faction,
                    Original_Faction => Faction,
                    Active           => True,
                    Scheduled        => False,
                    Next_Event       =>
                      Harriet.Calendar.Clock
                    + Harriet.Calendar.Days (Harriet.Random.Unit_Random),
                    Updates_Started  => True,
                    Next_Update      =>
                      Harriet.Calendar.Clock
                    + Harriet.Calendar.Days (Harriet.Random.Unit_Random),
                    Manager          => "default-colony",
                    World            => World,
                    Loyalty          => 1.0,
                    Happiness        => 1.0,
                    Population       =>
                      Harriet.Quantities.To_Quantity (Get ("population")),
                    Wealth           =>
                      Harriet.Money.To_Money (Get ("wealth")),
                    Tax_Rate         => Get ("tax-rate"),
                    Tax_Evasion      => Get ("tax-rate") / 10.0,
                    Economy          => Get ("economy"));

   begin

      Harriet.Configure.Commodities.Configure_Stock
        (Has_Stock => Harriet.Db.Colony.Get (Colony).Get_Has_Stock_Reference,
         Config    => Config);

      Initial_Facilities
        (Config => Config.Child ("facilities"),
         Colony => Colony,
         World  => World);

      Initial_Ships
        (Config  => Config.Child ("ships"),
         Faction => Faction,
         World   => World);

   end Initial_Colony;

   ------------------------
   -- Initial_Facilities --
   ------------------------

   procedure Initial_Facilities
     (Config  : Tropos.Configuration;
      Colony  : Harriet.Db.Colony_Reference;
      World   : Harriet.Db.World_Reference)
   is
   begin
      for Installation_Config of Config loop
         declare
            use Harriet.Db;
            Name : constant String := Installation_Config.Config_Name;
            Facility : constant Harriet.Db.Facility_Reference :=
                         Harriet.Db.Facility.Get_Reference_By_Tag (Name);
         begin
            if Facility = Null_Facility_Reference then
               raise Constraint_Error with
                 "no such facility: " & Name;
            end if;

            Initial_Installations
              (Count    => Installation_Config.Value,
               Facility => Facility,
               Colony   => Colony,
               World    => World);
         end;
      end loop;
   end Initial_Facilities;

   ----------------------
   -- Initial_Industry --
   ----------------------

   procedure Initial_Installations
     (Count    : Natural;
      Facility : Harriet.Db.Facility_Reference;
      Colony   : Harriet.Db.Colony_Reference;
      World    : Harriet.Db.World_Reference)
   is
      use type Harriet.Calendar.Time;
   begin
      for I in 1 .. Count loop
         Harriet.Db.Installation.Create
           (Facility   => Facility,
            World      => World,
            Colony     => Colony,
            Efficiency => 1.0,
            Active     => True,
            Scheduled  => False,
            Next_Event =>
              Harriet.Calendar.Clock
            + Harriet.Calendar.Days (Harriet.Random.Unit_Random),
            Updates_Started  => True,
            Next_Update      =>
              Harriet.Calendar.Clock
            + Harriet.Calendar.Days (Harriet.Random.Unit_Random),
            Manager          => "default-industry",
            Commodity  => Harriet.Db.Null_Commodity_Reference,
            Resource   => Harriet.Db.Null_Resource_Reference,
            Technology => Harriet.Db.Null_Technology_Reference);
      end loop;
   end Initial_Installations;

   -------------------
   -- Initial_Ships --
   -------------------

   procedure Initial_Ships
     (Config  : Tropos.Configuration;
      Faction : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference)
   is
   begin
      for Design_Config of Config loop
         Harriet.Ships.Create_Ship
           (Owner   => Faction,
            World   => World,
            Design  =>
              Harriet.Db.Ship_Design.First_Reference_By_Name
                (Design_Config.Config_Name),
            Manager => "default-ship",
            Name    => Design_Config.Config_Name);
      end loop;
   end Initial_Ships;

end Harriet.Factions.Create;
