with Harriet.Db.Ship;

package Harriet.Ships is

   type Ship_Type is tagged private;

   function Get (Reference : Harriet.Db.Ship_Reference) return Ship_Type;
   function Get (Ship : Harriet.Db.Ship.Ship_Type) return Ship_Type;

   function Name (Ship : Ship_Type'Class) return String;

   function Owner
     (Ship : Ship_Type'Class)
      return Harriet.Db.Faction_Reference;

   function Star_System
     (Ship : Ship_Type'Class)
      return Harriet.Db.Star_System_Reference;

   function World
     (Ship : Ship_Type'Class)
      return Harriet.Db.World_Reference;

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Tank_Size
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Total_Impulse
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Maximum_System_Speed
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Current_Scan_Capability
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Journey_Time
     (Ship : Ship_Type'Class;
      Distance : Non_Negative_Real)
      return Duration;

   function Design_Maximum_System_Speed
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Cargo_Volume
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Fuel_Mass
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Impulse
     (Design : Harriet.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   procedure Create_Ship
     (Owner   : Harriet.Db.Faction_Reference;
      World   : Harriet.Db.World_Reference;
      Design  : Harriet.Db.Ship_Design_Reference;
      Manager : String;
      Name    : String);

private

   type Ship_Type is tagged
      record
         Reference : Harriet.Db.Ship_Reference;
      end record;

   function Get (Reference : Harriet.Db.Ship_Reference) return Ship_Type
   is (Ship_Type'(Reference => Reference));

   function Get (Ship : Harriet.Db.Ship.Ship_Type) return Ship_Type
   is (Ship_Type'(Reference => Ship.Get_Ship_Reference));

   function Name (Ship : Ship_Type'Class) return String
   is (Harriet.Db.Ship.Get (Ship.Reference).Name);

   function Owner
     (Ship : Ship_Type'Class)
      return Harriet.Db.Faction_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).Faction);

   function Star_System
     (Ship : Ship_Type'Class)
      return Harriet.Db.Star_System_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).Star_System);

   function World
     (Ship : Ship_Type'Class)
      return Harriet.Db.World_Reference
   is (Harriet.Db.Ship.Get (Ship.Reference).World);

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is (Harriet.Db.Ship.Get (Ship.Reference).Semimajor_Axis);

end Harriet.Ships;
