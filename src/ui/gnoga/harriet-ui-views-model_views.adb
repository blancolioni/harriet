package body Concorde.UI.Views.Model_Views is

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (View  : not null access View_Type'Class;
      Model : not null access View_Model_Type'Class)
   is
   begin
      View.Model := Model_Access (Model);
      Model.Add_Watcher (View);
   end Set_Model;

end Concorde.UI.Views.Model_Views;
