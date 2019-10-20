with Harriet.Quantities;

with Harriet.Db;

package Harriet.Colonies is

   type Colony_Handle is tagged private;

   function Get
     (Reference : Harriet.Db.Colony_Reference)
      return Colony_Handle;

   function Population
     (Handle : Colony_Handle)
      return Harriet.Quantities.Quantity_Type;

private

   type Colony_Handle is tagged
      record
         Reference : Harriet.Db.Colony_Reference;
      end record;

end Harriet.Colonies;
