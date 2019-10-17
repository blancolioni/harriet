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

end Harriet.Colonies;
