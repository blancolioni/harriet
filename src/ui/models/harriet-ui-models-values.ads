private with Ada.Strings.Unbounded;

package Harriet.UI.Models.Values is

   type Model_Value_Data_Type is
     (Boolean_Type, Integer_Type, Real_Type, Text_Type);

   type Model_Value_Type is private;

   function Null_Value return Model_Value_Type;
   function Boolean_Value (Value : Boolean) return Model_Value_Type;
   function Integer_Value (Value : Integer) return Model_Value_Type;
   function Real_Value (Value : Real) return Model_Value_Type;
   function Text_Value (Value : String) return Model_Value_Type;

   function "<" (Left, Right : Model_Value_Type) return Boolean;
   function ">" (Left, Right : Model_Value_Type) return Boolean;

   function Is_Null (Value : Model_Value_Type) return Boolean;
   function Is_Boolean (Value : Model_Value_Type) return Boolean;
   function Is_Integer (Value : Model_Value_Type) return Boolean;
   function Is_Real (Value : Model_Value_Type) return Boolean;
   function Is_Text (Value : Model_Value_Type) return Boolean;

   function To_Boolean (Value : Model_Value_Type) return Boolean;
   function To_Integer (Value : Model_Value_Type) return Integer;
   function To_Real (Value : Model_Value_Type) return Real;
   function To_Text (Value : Model_Value_Type) return String;

private

   type Model_Value_Type
     (Data_Type : Model_Value_Data_Type := Text_Type)
   is
      record
         Is_Null : Boolean := True;
         case Data_Type is
            when Boolean_Type =>
               Bool_Value    : Boolean;
            when Integer_Type =>
               Integer_Value : Integer;
            when Real_Type =>
               Real_Value    : Real;
            when Text_Type =>
               Text_Value    : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   function Null_Value return Model_Value_Type
   is (Text_Type, True, Ada.Strings.Unbounded.Null_Unbounded_String);

   function Boolean_Value (Value : Boolean) return Model_Value_Type
   is (Boolean_Type, False, Value);

   function Integer_Value (Value : Integer) return Model_Value_Type
   is (Integer_Type, False, Value);

   function Real_Value (Value : Real) return Model_Value_Type
   is (Real_Type, False, Value);

   function Text_Value (Value : String) return Model_Value_Type
   is (Text_Type, False, Ada.Strings.Unbounded.To_Unbounded_String (Value));

   function Is_Null (Value : Model_Value_Type) return Boolean
   is (Value.Is_Null);

   function Is_Boolean (Value : Model_Value_Type) return Boolean
   is (Value.Data_Type = Boolean_Type);

   function Is_Integer (Value : Model_Value_Type) return Boolean
   is (Value.Data_Type = Integer_Type);

   function Is_Real (Value : Model_Value_Type) return Boolean
   is (Value.Data_Type = Real_Type);

   function Is_Text (Value : Model_Value_Type) return Boolean
   is (Value.Data_Type = Text_Type);

   function To_Boolean (Value : Model_Value_Type) return Boolean
   is (if Value.Is_Null then False else Value.Bool_Value);

   function To_Integer (Value : Model_Value_Type) return Integer
   is (if Value.Is_Null then 0 else Value.Integer_Value);

   function To_Real (Value : Model_Value_Type) return Real
   is (if Value.Is_Null then 0.0 else Value.Real_Value);

   function To_Text (Value : Model_Value_Type) return String
   is (if Value.Is_Null then ""
       else Ada.Strings.Unbounded.To_String (Value.Text_Value));

end Harriet.UI.Models.Values;
