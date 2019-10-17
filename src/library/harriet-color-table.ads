private package Harriet.Color.Table is

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Harriet_Color;
   procedure Add (Name : String;
                  Color : Harriet_Color);

end Harriet.Color.Table;
