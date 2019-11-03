private with Ada.Containers.Doubly_Linked_Lists;

with Harriet.Json;

with Harriet.Db;

with Harriet.Db.World_Sector;

package Harriet.Worlds is

   type World_Selection is tagged private;

   function Is_Empty (Selection : World_Selection'Class) return Boolean;

   procedure Clear (Selection : in out World_Selection'Class);
   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : Harriet.Db.World_Reference);

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : Harriet.Db.World_Reference)
      return Boolean);

   type World_Array is
     array (Positive range <>) of Harriet.Db.World_Reference;

   function Get_Worlds (Selection : World_Selection'Class) return World_Array;

   function Star_System
     (World : Harriet.Db.World_Reference)
      return Harriet.Db.Star_System_Reference;

   function Name
     (World : Harriet.Db.World_Reference)
      return String;

   function Mass
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real;

   function Radius
     (World : Harriet.Db.World_Reference)
      return Non_Negative_Real;

   function Habitability
     (World : Harriet.Db.World_Reference)
      return Unit_Real;

   function Is_Terrestrial
     (World : Harriet.Db.World_Reference)
      return Boolean;

   function Distance
     (From, To : Harriet.Db.World_Reference)
      return Non_Negative_Real;

   type World_Sector_Array is
     array (Positive range <>) of Harriet.Db.World_Sector_Reference;

   function Get_Neighbours
     (Sector : Harriet.Db.World_Sector_Reference)
      return World_Sector_Array;

   procedure Scan_Surface
     (World : Harriet.Db.World_Reference;
      Process : not null access
        procedure (Sector : Harriet.Db.World_Sector_Reference));

   function Best_Sector
     (World : Harriet.Db.World_Reference;
      Score : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Real)
      return Harriet.Db.World_Sector_Reference;

   function Find_Sector
     (World : Harriet.Db.World_Reference;
      Test : not null access
        function (Sector : Harriet.Db.World_Sector.World_Sector_Type)
      return Boolean)
      return Harriet.Db.World_Sector_Reference;

   function Get_World
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.World_Reference;

   type Sector_Vertex is
      record
         X, Y, Z : Signed_Unit_Real;
      end record;

   function Get_Centre
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex;

   function Get_Terrain
     (Sector : Harriet.Db.World_Sector_Reference)
      return Harriet.Db.Terrain_Reference;

   function Get_Average_Temperature
     (Sector : Harriet.Db.World_Sector_Reference)
      return Non_Negative_Real;

   type Sector_Vertex_Array is array (Positive range <>) of Sector_Vertex;

   function Get_Vertices
     (Sector : Harriet.Db.World_Sector_Reference)
      return Sector_Vertex_Array;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Harriet.Db.World_Reference, Harriet.Db."=");

   type World_Selection is tagged
      record
         List : World_Lists.List;
      end record;

end Harriet.Worlds;
