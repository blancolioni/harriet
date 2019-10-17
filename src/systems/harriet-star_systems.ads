with Harriet.Factions;
with Harriet.Stars;
with Harriet.Worlds;

with Harriet.Db;

package Harriet.Star_Systems is

   type Interstellar_Position is
      record
         X, Y, Z : Real;
      end record;

   type Star_System_Type is tagged private;

   function Get
     (Reference : Harriet.Db.Star_System_Reference)
      return Star_System_Type;

   function Has_Star_System
     (Star_System : Star_System_Type'Class)
      return Boolean;

   function Reference
     (Star_System : Star_System_Type'Class)
      return Harriet.Db.Star_System_Reference;

   function Owner
     (Star_System : Star_System_Type'Class)
      return Harriet.Factions.Faction_Type'Class;

   function Position
     (Star_System : Star_System_Type'Class)
      return Interstellar_Position;

   function Primary
     (Star_System : Star_System_Type'Class)
      return Harriet.Stars.Star_Type'Class;

   function First return Harriet.Db.Star_System_Reference;
   function Find_Exact
     (Name : String)
      return Harriet.Db.Star_System_Reference;

   function Name
     (Star_System : Harriet.Db.Star_System_Reference)
      return String;

   function Name
     (Star_System : Star_System_Type'Class)
      return String;

   function Position
     (Star_System : Harriet.Db.Star_System_Reference)
      return Interstellar_Position;

   function Claimed
     (Star_System : Harriet.Db.Star_System_Reference)
      return Boolean;

   procedure Claim
     (Star_System : Harriet.Db.Star_System_Reference);

   function Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection;

   function Terrestrial_Worlds
     (Star_System : Harriet.Db.Star_System_Reference)
      return Harriet.Worlds.World_Selection;

private

   type Star_System_Type is tagged
      record
         Reference : Harriet.Db.Star_System_Reference;
      end record;

   function Reference
     (Star_System : Star_System_Type'Class)
      return Harriet.Db.Star_System_Reference
   is (Star_System.Reference);

end Harriet.Star_Systems;
