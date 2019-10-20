with Harriet.Db.Colony;

package body Harriet.Colonies is

   ---------
   -- Get --
   ---------

   function Get (Reference : Harriet.Db.Colony_Reference) return Colony_Handle
   is
   begin
      return Colony_Handle'
        (Reference => Reference);
   end Get;

   ----------------
   -- Population --
   ----------------

   function Population
     (Handle : Colony_Handle)
      return Harriet.Quantities.Quantity_Type
   is
   begin
      return Harriet.Db.Colony.Get (Handle.Reference).Population;
   end Population;

end Harriet.Colonies;
