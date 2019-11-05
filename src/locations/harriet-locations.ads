with Harriet.Db;

package Harriet.Locations is

   function Has_Location
     (Location : Harriet.Db.Location_Reference)
      return Boolean;

   function Has_World
     (Location : Harriet.Db.Location_Reference)
      return Boolean;

   function Get_World
     (Location : Harriet.Db.Location_Reference)
      return Harriet.Db.World_Reference
     with Pre => Has_World (Location);

   function Get_Primary_Massive
     (Location : Harriet.Db.Location_Reference)
      return Harriet.Db.Massive_Object_Reference;

   function Get_System
     (Location : Harriet.Db.Location_Reference)
      return Harriet.Db.Star_System_Reference;

   function Same_System
     (Location_1, Location_2 : Harriet.Db.Location_Reference)
      return Boolean;

   function Distance
     (From, To : Harriet.Db.Location_Reference)
      return Non_Negative_Real;

   function Show (Location : Harriet.Db.Location_Reference)
                  return String;

   function New_Location return Harriet.Db.Location_Reference;

   procedure Clear
     (Location : Harriet.Db.Location_Reference);

   procedure Set_World_Orbit_Location
     (Location : Harriet.Db.Location_Reference;
      World    : Harriet.Db.World_Reference;
      Orbit    : Non_Negative_Real);

   procedure Set_System_Location
     (Location    : Harriet.Db.Location_Reference;
      Star_System : Harriet.Db.Star_System_Reference;
      X, Y, Z     : Real);

end Harriet.Locations;
