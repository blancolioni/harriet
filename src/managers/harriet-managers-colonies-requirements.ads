private with WL.Heaps;

package Harriet.Managers.Colonies.Requirements is

   type Colony_Requirements is private;

   procedure Add_Requirement
     (Requirements : Colony_Requirements;
      Facility     : Harriet.Db.Facility_Reference;
      Count        : Positive;
      Priority     : Positive);

   procedure Add_Requirement
     (Requirements : Colony_Requirements;
      Commodity    : Harriet.Db.Commodity_Reference;
      Count        : Positive;
      Priority     : Positive);

   procedure Scan_Commodity_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Count     : Positive));

   procedure Scan_Facility_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access
        procedure (Facility : Harriet.Db.Facility_Reference;
                   Count    : Positive));

private

   type Facility_Requirement is
      record
         Facility : Harriet.Db.Facility_Reference;
         Count    : Positive;
      end record;

   package Facility_Queues is
     new WL.Heaps (Positive, Facility_Requirement, "<");

   type Commodity_Requirement is
      record
         Commodity : Harriet.Db.Commodity_Reference;
         Count     : Positive;
      end record;

   package Commodity_Queues is
     new WL.Heaps (Positive, Commodity_Requirement, "<");

   type Colony_Requirements is
      record
         Facilities  : Facility_Queues.Heap;
         Commodities : Commodity_Queues.Heap;
      end record;

end Harriet.Managers.Colonies.Requirements;
