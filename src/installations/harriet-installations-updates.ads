with Harriet.Updates;

package Harriet.Installations.Updates is

   function Daily_Update
     (Reference : Harriet.Db.Installation_Reference)
      return Harriet.Updates.Update_Interface'Class;

end Harriet.Installations.Updates;
