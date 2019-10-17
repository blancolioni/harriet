with Harriet.Solar_System;

with Harriet.Db.Star;

package body Harriet.Stars is

   -----------
   -- Color --
   -----------

   function Color
     (Star : Star_Type'Class)
      return Harriet.Color.Harriet_Color
   is
      Rec : constant Harriet.Db.Star.Star_Type :=
              Harriet.Db.Star.Get (Star.Reference);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   function Get
     (Reference : Harriet.Db.Star_Reference)
      return Star_Type'Class
   is
   begin
      return Star_Type'(Reference => Reference);
   end Get;

   ----------
   -- Mass --
   ----------

   function Mass
     (Star : Harriet.Db.Star_Reference)
      return Non_Negative_Real
   is
   begin
      return Harriet.Db.Star.Get (Star).Mass;
   end Mass;

   ----------
   -- Name --
   ----------

   function Name (Star : Harriet.Db.Star_Reference) return String is
   begin
      return Harriet.Db.Star.Get (Star).Name;
   end Name;

   ------------------
   -- Solar_Masses --
   ------------------

   function Solar_Masses
     (Star : Harriet.Db.Star_Reference)
      return Non_Negative_Real
   is
   begin
      return Mass (Star) / Harriet.Solar_System.Solar_Mass;
   end Solar_Masses;

   -------------------
   -- Spectral_Type --
   -------------------

   function Spectral_Type
     (Star : Harriet.Db.Star_Reference)
      return String
   is
      Rec : constant Harriet.Db.Star.Star_Type :=
              Harriet.Db.Star.Get (Star);
   begin
      return Harriet.Db.Spectral_Class'Image (Rec.Class)
        & Character'Val (Rec.Subclass + 48);
   end Spectral_Type;

end Harriet.Stars;
