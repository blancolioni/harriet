package body Harriet.Managers.Colonies.Requirements is

   --------------------------------
   -- Add_Population_Requirement --
   --------------------------------

   procedure Add_Population_Requirement
     (Requirements : in out Colony_Requirements;
      Quantity     : Harriet.Quantities.Quantity_Type;
      Priority     : Priority_Type)
   is
   begin
      Population_Queues.Add_Requirement
        (Queue    => Requirements.Population,
         Priority => Priority,
         Item     => (null record),
         Quantity => Quantity);

   end Add_Population_Requirement;

   ---------------------
   -- Add_Requirement --
   ---------------------

   procedure Add_Requirement
     (Requirements : in out Colony_Requirements;
      Facility     : Harriet.Db.Facility_Reference;
      Count        : Positive;
      Priority     : Priority_Type)
   is
   begin
      Facility_Queues.Add_Requirement
        (Queue    => Requirements.Facilities,
         Priority => Priority,
         Item     => Facility,
         Quantity => Quantities.To_Quantity (Real (Count)));
   end Add_Requirement;

   ---------------------
   -- Add_Requirement --
   ---------------------

   procedure Add_Requirement
     (Requirements : in out Colony_Requirements;
      Commodity    : Harriet.Db.Commodity_Reference;
      Quantity : Harriet.Quantities.Quantity_Type; Priority : Priority_Type)
   is
   begin
      Commodity_Queues.Add_Requirement
        (Queue    => Requirements.Commodities,
         Priority => Priority,
         Item     => Commodity,
         Quantity => Quantity);
   end Add_Requirement;

   ------------------------
   -- Clear_Requirements --
   ------------------------

   procedure Clear_Requirements
     (Requirements : in out Colony_Requirements)
   is
   begin
      Commodity_Queues.Clear (Requirements.Commodities);
      Facility_Queues.Clear (Requirements.Facilities);
      Population_Queues.Clear (Requirements.Population);
   end Clear_Requirements;

   ---------------------------------
   -- Scan_Commodity_Requirements --
   ---------------------------------

   procedure Scan_Commodity_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access procedure
        (Commodity : Harriet.Db.Commodity_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Priority  : Priority_Type))
   is
   begin
      Commodity_Queues.Scan_Requirements
        (Queue    => Requirements.Commodities,
         Process  => Process);
   end Scan_Commodity_Requirements;

   --------------------------------
   -- Scan_Facility_Requirements --
   --------------------------------

   procedure Scan_Facility_Requirements
     (Requirements : Colony_Requirements;
      Process      : not null access procedure
        (Facility : Harriet.Db.Facility_Reference;
         Count : Positive;
         Priority : Priority_Type))
   is
      procedure Local_Process
        (Facility : Harriet.Db.Facility_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Priority  : Priority_Type);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process
        (Facility  : Harriet.Db.Facility_Reference;
         Quantity  : Harriet.Quantities.Quantity_Type;
         Priority  : Priority_Type)
      is
      begin
         Process (Facility,
                  Natural (Harriet.Quantities.To_Real (Quantity)),
                  Priority);
      end Local_Process;

   begin
      Facility_Queues.Scan_Requirements (Requirements.Facilities,
                                         Local_Process'Access);
   end Scan_Facility_Requirements;

end Harriet.Managers.Colonies.Requirements;
