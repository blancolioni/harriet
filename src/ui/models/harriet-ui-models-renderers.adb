with Ada.Strings.Fixed;

with Harriet.Calendar;
with Harriet.Money;
with Harriet.Quantities;
with Harriet.Real_Images;

package body Harriet.UI.Models.Renderers is

   type Default_Renderer_Type is
     new Render_Interface with null record;

   overriding function To_String
     (Render : Default_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return String;

   overriding function To_Json
     (Render : Default_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return Harriet.Json.Json_Value'Class;

   type Quantity_Renderer_Type is
     new Real_Renderer_Type with null record;

   overriding function Real_To_String
     (Renderer : Quantity_Renderer_Type;
      Value    : Real)
      return String
   is (Harriet.Quantities.Show (Harriet.Quantities.To_Quantity (Value)));

   type Money_Renderer_Type is
     new Real_Renderer_Type with null record;

   overriding function Real_To_String
     (Renderer : Money_Renderer_Type;
      Value    : Real)
      return String
   is (Harriet.Money.Show (Harriet.Money.To_Money (Value)));

   type Price_Renderer_Type is
     new Real_Renderer_Type with null record;

   overriding function Real_To_String
     (Renderer : Price_Renderer_Type;
      Value    : Real)
      return String
   is (Harriet.Money.Show (Harriet.Money.To_Price (Value)));

   type Time_Renderer_Type is
     new Real_Renderer_Type with null record;

   overriding function Real_To_String
     (Renderer : Time_Renderer_Type;
      Value    : Real)
      return String
   is (Harriet.Calendar.Image (Harriet.Calendar.To_Time (Value)));

   ----------------------
   -- Default_Renderer --
   ----------------------

   function Default_Renderer return Render_Interface'Class is
   begin
      return Renderer : Default_Renderer_Type;
   end Default_Renderer;

   --------------------
   -- Money_Renderer --
   --------------------

   function Money_Renderer return Render_Interface'Class is
   begin
      return Renderer : Money_Renderer_Type;
   end Money_Renderer;

   --------------------
   -- Price_Renderer --
   --------------------

   function Price_Renderer return Render_Interface'Class is
   begin
      return Renderer : Price_Renderer_Type;
   end Price_Renderer;

   -----------------------
   -- Quantity_Renderer --
   -----------------------

   function Quantity_Renderer return Render_Interface'Class is
   begin
      return Renderer : Quantity_Renderer_Type;
   end Quantity_Renderer;

   -------------------
   -- Time_Renderer --
   -------------------

   function Time_Renderer return Render_Interface'Class is
   begin
      return Renderer : Time_Renderer_Type;
   end Time_Renderer;

   overriding function To_Json
     (Render : Default_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return Json.Json_Value'Class
   is
      pragma Unreferenced (Render);
   begin
      if Values.Is_Null (Value) then
         return Json.Null_Value;
      elsif Values.Is_Boolean (Value) then
         return Json.Boolean_Value (Values.To_Boolean (Value));
      elsif Values.Is_Integer (Value) then
         return Json.Integer_Value (Values.To_Integer (Value));
      elsif Values.Is_Real (Value) then
         return Json.String_Value
           (Harriet.Real_Images.Approximate_Image
              (Values.To_Real (Value)));
      else
         return Json.String_Value (Values.To_Text (Value));
      end if;
   end To_Json;

   -------------
   -- To_Json --
   -------------

   overriding function To_Json
     (Render : Real_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return Harriet.Json.Json_Value'Class
   is
   begin
      return Harriet.Json.String_Value (Render.To_String (Value));
   end To_Json;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Render : Default_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return String
   is
      pragma Unreferenced (Render);
   begin
      if Values.Is_Null (Value) then
         return "";
      elsif Values.Is_Boolean (Value) then
         return (if Values.To_Boolean (Value) then "true" else "false");
      elsif Values.Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Integer'Image (Values.To_Integer (Value)),
            Ada.Strings.Left);
      elsif Values.Is_Real (Value) then
         return Harriet.Real_Images.Approximate_Image
           (Values.To_Real (Value));
      else
         return Values.To_Text (Value);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Render : Real_Renderer_Type;
      Value  : Values.Model_Value_Type)
      return String
   is
   begin
      return Real_Renderer_Type'Class (Render).Real_To_String
        (Values.To_Real (Value));
   end To_String;

end Harriet.UI.Models.Renderers;
