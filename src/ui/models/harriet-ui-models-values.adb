package body Harriet.UI.Models.Values is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Model_Value_Type) return Boolean is
   begin
      pragma Assert (Left.Data_Type = Right.Data_Type);
      case Left.Data_Type is
         when Boolean_Type =>
            return Left.Bool_Value < Right.Bool_Value;
         when Integer_Type =>
            return Left.Integer_Value < Right.Integer_Value;
         when Real_Type =>
            return Left.Real_Value < Right.Real_Value;
         when Text_Type =>
            return Ada.Strings.Unbounded."<"
              (Left.Text_Value, Right.Text_Value);
      end case;
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Model_Value_Type) return Boolean
   is
   begin
      return Left /= Right and then not (Left < Right);
   end ">";

end Harriet.UI.Models.Values;
