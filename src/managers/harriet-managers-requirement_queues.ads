private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;

with Harriet.Quantities;

generic
   type Required_Item_Type is private;
   with function "=" (Left, Right : Required_Item_Type) return Boolean is <>;
package Harriet.Managers.Requirement_Queues is

   type Requirement_Queue is private;

   procedure Clear
     (Queue : in out Requirement_Queue);

   procedure Scan_Requirements
     (Queue   : Requirement_Queue;
      Process : not null access
        procedure (Item : Required_Item_Type;
                   Quantity : Harriet.Quantities.Quantity_Type;
                   Priority : Priority_Type));

   procedure Add_Requirement
     (Queue       : in out Requirement_Queue;
      Priority    : Priority_Type;
      Item        : Required_Item_Type;
      Quantity    : Harriet.Quantities.Quantity_Type);

private

   type Requirement_Record is
      record
         Item     : Required_Item_Type;
         Quantity : Harriet.Quantities.Quantity_Type;
      end record;

   package Requirement_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Requirement_Record);

   package Requirement_Maps is
     new Ada.Containers.Ordered_Maps (Priority_Type, Requirement_Lists.List,
                                      "=" => Requirement_Lists."=");

   type Requirement_Queue is
      record
         Map : Requirement_Maps.Map;
      end record;

end Harriet.Managers.Requirement_Queues;
