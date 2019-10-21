private with Harriet.Managers.Requirement_Queues;

with Harriet.Quantities;

package Harriet.Managers.Colonies.Requirements is

   type Colony_Requirements is private;

   procedure Clear_Requirements
     (Requirements : in out Colony_Requirements);

   procedure Add_Requirement
     (Requirements : in out Colony_Requirements;
      Facility     : Harriet.Db.Facility_Reference;
      Count        : Positive;
      Priority     : Priority_Type);

   procedure Add_Requirement
     (Requirements : in out Colony_Requirements;
      Commodity    : Harriet.Db.Commodity_Reference;
      Quantity     : Harriet.Quantities.Quantity_Type;
      Priority     : Priority_Type);

   procedure Add_Population_Requirement
     (Requirements : in out Colony_Requirements;
      Quantity     : Harriet.Quantities.Quantity_Type;
      Priority     : Priority_Type);

   procedure Scan_Commodity_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access
        procedure (Commodity : Harriet.Db.Commodity_Reference;
                   Quantity  : Harriet.Quantities.Quantity_Type;
                   Priority  : Priority_Type));

   procedure Scan_Facility_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access
        procedure (Facility : Harriet.Db.Facility_Reference;
                   Count    : Positive;
                   Priority  : Priority_Type));

private

   package Facility_Queues is
     new Harriet.Managers.Requirement_Queues
       (Harriet.Db.Facility_Reference, Harriet.Db."=");

   package Commodity_Queues is
     new Harriet.Managers.Requirement_Queues
       (Harriet.Db.Commodity_Reference, Harriet.Db."=");

   type Population_Requirement is null record;

   package Population_Queues is
     new Harriet.Managers.Requirement_Queues
       (Population_Requirement);

   type Colony_Requirements is
      record
         Commodities : Commodity_Queues.Requirement_Queue;
         Facilities  : Facility_Queues.Requirement_Queue;
         Population  : Population_Queues.Requirement_Queue;
      end record;

end Harriet.Managers.Colonies.Requirements;
