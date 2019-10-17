with Harriet.Db;

package Harriet.Stars.Tables is

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Harriet.Db.Spectral_Class;
      Subclass     : out Natural;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      R, G, B      : out Unit_Real);

end Harriet.Stars.Tables;
