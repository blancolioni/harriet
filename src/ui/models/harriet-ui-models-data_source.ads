private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

with Harriet.UI.Models.Renderers;
with Harriet.UI.Models.Values;

package Harriet.UI.Models.Data_Source is

   type Root_Data_Source_Model is
     abstract new Root_Harriet_Model with private;

   overriding function Default_View_Name
     (Model : Root_Data_Source_Model)
      return String;

   function Column_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Row_Count
     (Data_Source : Root_Data_Source_Model)
      return Natural
      is abstract;

   function Column_Heading_Id
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return String
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Heading_Label
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return String
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Renderer
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return Renderers.Render_Interface'Class
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Column_Type
     (Data_Source : Root_Data_Source_Model;
      Column      : Positive)
      return Values.Model_Value_Data_Type
      is abstract
     with Pre'Class => Column <= Data_Source.Column_Count;

   function Cell_Value
     (Data_Source : Root_Data_Source_Model;
      Row         : Positive;
      Column      : Positive)
      return Values.Model_Value_Type
      is abstract
     with Pre'Class => Row <= Data_Source.Row_Count
     and then Column <= Data_Source.Column_Count;

   overriding function Handle
     (Model   : in out Root_Data_Source_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   overriding function Get
     (Model   : Root_Data_Source_Model;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class;

   type Model_Value_Array is
     array (Positive range <>) of Values.Model_Value_Type;

   type Simple_Data_Source_Model is
     abstract new Root_Data_Source_Model with private;

   procedure Add_Column
     (Model    : in out Simple_Data_Source_Model'Class;
      Id       : String;
      Label    : String;
      Col_Type : Values.Model_Value_Data_Type;
      Renderer : Renderers.Render_Interface'Class :=
        Renderers.Default_Renderer);

   procedure Add_Row
     (Model : in out Simple_Data_Source_Model'Class;
      Row   : Model_Value_Array);

   overriding function Column_Count
     (Model : Simple_Data_Source_Model)
      return Natural;

   overriding function Row_Count
     (Model : Simple_Data_Source_Model)
      return Natural;

   overriding function Column_Heading_Id
     (Model : Simple_Data_Source_Model;
      Column      : Positive)
      return String;

   overriding function Column_Heading_Label
     (Model : Simple_Data_Source_Model;
      Column      : Positive)
      return String;

   overriding function Column_Renderer
     (Model : Simple_Data_Source_Model;
      Column      : Positive)
      return Renderers.Render_Interface'Class;

   overriding function Column_Type
     (Model : Simple_Data_Source_Model;
      Column      : Positive)
      return Values.Model_Value_Data_Type;

   overriding function Cell_Value
     (Model       : Simple_Data_Source_Model;
      Row         : Positive;
      Column      : Positive)
      return Values.Model_Value_Type;

   procedure On_Fetched
     (Model : in out Simple_Data_Source_Model)
   is null;

private

   type Root_Data_Source_Model is
     abstract new Root_Harriet_Model with
      record
         null;
      end record;

   overriding function Default_View_Name
     (Model : Root_Data_Source_Model)
      return String
   is ("Table");

   package Simple_Data_Source_Rows is
     new Ada.Containers.Vectors (Positive, Values.Model_Value_Type,
                                 Values."=");

   package Simple_Table is
     new Ada.Containers.Vectors
       (Positive, Simple_Data_Source_Rows.Vector, Simple_Data_Source_Rows."=");

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Type_Vectors is
     new Ada.Containers.Vectors (Positive, Values.Model_Value_Data_Type,
                                 Values."=");

   package Renderer_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Renderers.Render_Interface'Class, Renderers."=");

   type Simple_Data_Source_Model is
     abstract new Root_Data_Source_Model with
      record
         Column_Ids       : String_Vectors.Vector;
         Column_Labels    : String_Vectors.Vector;
         Column_Types     : Type_Vectors.Vector;
         Column_Renderers : Renderer_Vectors.Vector;
         Data             : Simple_Table.Vector;
      end record;

   overriding function Column_Count
     (Model : Simple_Data_Source_Model)
      return Natural
   is (Model.Column_Ids.Last_Index);

   overriding function Row_Count
     (Model : Simple_Data_Source_Model)
      return Natural
   is (Model.Data.Last_Index);

   overriding function Column_Heading_Id
     (Model       : Simple_Data_Source_Model;
      Column      : Positive)
      return String
   is (Model.Column_Ids.Element (Column));

   overriding function Column_Heading_Label
     (Model       : Simple_Data_Source_Model;
      Column      : Positive)
      return String
   is (Model.Column_Labels.Element (Column));

   overriding function Column_Type
     (Model       : Simple_Data_Source_Model;
      Column      : Positive)
      return Values.Model_Value_Data_Type
   is (Model.Column_Types.Element (Column));

   overriding function Column_Renderer
     (Model       : Simple_Data_Source_Model;
      Column      : Positive)
      return Renderers.Render_Interface'Class
   is (case Column is
          when others => Renderers.Default_Renderer);

   overriding function Cell_Value
     (Model       : Simple_Data_Source_Model;
      Row         : Positive;
      Column      : Positive)
      return Values.Model_Value_Type
   is (Model.Data.Element (Row).Element (Column));

end Harriet.UI.Models.Data_Source;
