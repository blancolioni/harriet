package Harriet.Updates is

   type Update_Interface is interface;

   procedure Activate
     (Update : Update_Interface)
   is abstract;

end Harriet.Updates;
