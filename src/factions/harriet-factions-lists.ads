with Ada.Containers.Doubly_Linked_Lists;

package Harriet.Factions.Lists is
  new Ada.Containers.Doubly_Linked_Lists
    (Harriet.Db.Faction_Reference, Harriet.Db."=");
