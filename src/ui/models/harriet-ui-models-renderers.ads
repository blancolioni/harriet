with Harriet.UI.Models.Values;

package Harriet.UI.Models.Renderers is

   type Render_Interface is interface;

   function To_String
     (Render : Render_Interface;
      Value  : Values.Model_Value_Type)
      return String
      is abstract;

   function To_Json
     (Render : Render_Interface;
      Value  : Values.Model_Value_Type)
      return Harriet.Json.Json_Value'Class
      is abstract;

   type Real_Renderer_Type is
     abstract new Render_Interface with null record;

   overriding function To_String
     (Render : Real_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return String;

   overriding function To_Json
     (Render : Real_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return Harriet.Json.Json_Value'Class;

   function Real_To_String
     (Renderer : Real_Renderer_Type;
      Value    : Real)
      return String
      is abstract;

   function Default_Renderer return Render_Interface'Class;

   function Quantity_Renderer return Render_Interface'Class;
   function Money_Renderer return Render_Interface'Class;
   function Price_Renderer return Render_Interface'Class;
   function Time_Renderer return Render_Interface'Class;

end Harriet.UI.Models.Renderers;
