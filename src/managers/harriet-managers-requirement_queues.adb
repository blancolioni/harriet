package body Harriet.Managers.Requirement_Queues is

   ---------------------
   -- Add_Requirement --
   ---------------------

   procedure Add_Requirement
     (Queue    : in out Requirement_Queue;
      Priority : Priority_Type;
      Item     : Required_Item_Type;
      Quantity : Harriet.Quantities.Quantity_Type)
   is
      use type Harriet.Quantities.Quantity_Type;
   begin
      if not Queue.Map.Contains (Priority) then
         Queue.Map.Insert (Priority, Requirement_Lists.Empty_List);
      end if;

      declare
         List  : Requirement_Lists.List renames Queue.Map (Priority);
         Found : Boolean := False;
      begin
         for Element of List loop
            if Element.Item = Item then
               Element.Quantity := Element.Quantity + Quantity;
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            List.Append ((Item, Quantity));
         end if;
      end;

   end Add_Requirement;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Queue : in out Requirement_Queue)
   is
   begin
      Queue.Map.Clear;
   end Clear;

   -----------------------
   -- Scan_Requirements --
   -----------------------

   procedure Scan_Requirements
     (Queue   : Requirement_Queue;
      Process : not null access procedure
        (Item     : Required_Item_Type;
         Quantity : Harriet.Quantities.Quantity_Type;
         Priority : Priority_Type))
   is
      use Requirement_Maps;
   begin
      for Position in Queue.Map.Iterate loop
         declare
            Priority : constant Priority_Type := Key (Position);
            List     : constant Requirement_Lists.List := Element (Position);
         begin
            for Element of List loop
               Process (Element.Item, Element.Quantity, Priority);
            end loop;
         end;
      end loop;
   end Scan_Requirements;

end Harriet.Managers.Requirement_Queues;
