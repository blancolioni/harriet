with Harriet.Updates;

package Harriet.Colonies.Updates is

   function Daily_Update
     (Reference : Harriet.Db.Colony_Reference)
      return Harriet.Updates.Update_Interface'Class;

end Harriet.Colonies.Updates;
