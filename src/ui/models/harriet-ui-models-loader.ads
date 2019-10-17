package Harriet.UI.Models.Loader is

   function Exists (Model_Name : String) return Boolean;

   function Get
     (Model_Name : String)
      return Root_Harriet_Model'Class
     with Pre => Exists (Model_Name);

end Harriet.UI.Models.Loader;
