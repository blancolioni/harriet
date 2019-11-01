with Harriet.Color;

with Harriet.Db;

package Harriet.Stars is

   type Star_Type is tagged private;

   function Get
     (Reference : Harriet.Db.Star_Reference)
      return Star_Type'Class;

   function Color
     (Star : Star_Type'Class)
      return Harriet.Color.Harriet_Color;

   function Temperature
     (Star : Star_Type'Class)
      return Non_Negative_Real;

   function Name (Star : Harriet.Db.Star_Reference) return String;
   function Mass (Star : Harriet.Db.Star_Reference) return Non_Negative_Real;
   function Solar_Masses
     (Star : Harriet.Db.Star_Reference)
      return Non_Negative_Real;
   function Spectral_Type
     (Star : Harriet.Db.Star_Reference)
      return String;

private

   type Star_Type is tagged
      record
         Reference : Harriet.Db.Star_Reference;
      end record;

end Harriet.Stars;
