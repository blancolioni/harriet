with Ada.Exceptions;

package body Harriet.UI.Models.Data_Source is

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column
     (Model    : in out Simple_Data_Source_Model'Class;
      Id       : String;
      Label    : String;
      Col_Type : Values.Model_Value_Data_Type;
      Renderer : Renderers.Render_Interface'Class :=
        Renderers.Default_Renderer)
   is
   begin
      Model.Column_Ids.Append (Id);
      Model.Column_Labels.Append (Label);
      Model.Column_Types.Append (Col_Type);
      Model.Column_Renderers.Append (Renderer);
   end Add_Column;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Model : in out Simple_Data_Source_Model'Class;
      Row   : Model_Value_Array)
   is
      New_Row : Simple_Data_Source_Rows.Vector;
   begin
      for Value of Row loop
         New_Row.Append (Value);
      end loop;
      Model.Data.Append (New_Row);
   end Add_Row;

   overriding function Get
     (Model   : Root_Data_Source_Model;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
      M           : Root_Data_Source_Model'Class renames
        Root_Data_Source_Model'Class (Model);
      Response    : Harriet.Json.Json_Object;
      Table       : Harriet.Json.Json_Object;
      Headings    : Harriet.Json.Json_Array;
      Rows        : Harriet.Json.Json_Array;
      Json_Sort   : constant Harriet.Json.Json_Value'Class :=
        Request.Get_Property ("sort");
      Sort_Column : constant Natural :=
        (if Json_Sort.Is_Null then 0
         else Integer'Value (Json_Sort.Image));
      Json_Asc    : constant Harriet.Json.Json_Value'Class :=
        Request.Get_Property ("ascending");
      Ascending   : constant Boolean :=
        (Json_Asc.Is_Null or else Boolean'Value (Json_Asc.Image));
      package Sort_Maps is new Ada.Containers.Vectors (Positive, Positive);
      Sorted_Map  : Sort_Maps.Vector;
   begin

      for I in 1 .. M.Row_Count loop
         Sorted_Map.Append (I);
      end loop;

      if Sort_Column > 0 then

         declare
            use type Values.Model_Value_Type;
            function "<" (Left, Right : Positive) return Boolean
            is (if Ascending
                then M.Cell_Value (Left, Sort_Column)
                < M.Cell_Value (Right, Sort_Column)
                else M.Cell_Value (Left, Sort_Column)
                > M.Cell_Value (Right, Sort_Column));

            package Sorting is
              new Sort_Maps.Generic_Sorting ("<");
         begin
            Sorting.Sort (Sorted_Map);
         end;
      end if;

      for I in 1 .. M.Column_Count loop
         declare
            Heading : Harriet.Json.Json_Object;
         begin
            Heading.Set_Property ("id", M.Column_Heading_Id (I));
            Heading.Set_Property ("label", M.Column_Heading_Label (I));
            Headings.Append (Heading);
         end;
      end loop;

      Table.Set_Property ("headings", Headings);

      for Sorted_Row_Index in 1 .. M.Row_Count loop
         declare
            Source_Row_Index : constant Positive :=
              Sorted_Map.Element (Sorted_Row_Index);
            Row              : Harriet.Json.Json_Object;
         begin
            for Col_Index in 1 .. M.Column_Count loop
               declare
                  Prop_Value : constant Values.Model_Value_Type :=
                    M.Cell_Value (Source_Row_Index, Col_Index);
                  Json_Value : constant Harriet.Json.Json_Value'Class :=
                    M.Column_Renderer (Col_Index).To_Json
                    (Prop_Value);
               begin
                  Row.Set_Property
                    (Name  => M.Column_Heading_Id (Col_Index),
                     Value => Json_Value);
               end;
            end loop;
            Rows.Append (Row);
         end;
      end loop;
      Table.Set_Property ("data", Rows);

      Response.Set_Property ("table", Table);
      return Response;

   exception
      when E : others =>
         return M.Error
           (State   => State,
            Client  => Client,
            Request => Request,
            Message => Ada.Exceptions.Exception_Message (E));

   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Root_Data_Source_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class)
      return Harriet.Json.Json_Value'Class
   is
--     begin
--   return Root_Data_Source_Model'Class (Model).Get (State, Client, Request);

      M : Root_Data_Source_Model'Class renames
        Root_Data_Source_Model'Class (Model);
      Response  : Harriet.Json.Json_Object;
      Table     : Harriet.Json.Json_Object;
      Headings  : Harriet.Json.Json_Array;
      Rows      : Harriet.Json.Json_Array;
      Json_Sort : constant Harriet.Json.Json_Value'Class :=
        Request.Get_Property ("sort");
      Sort_Column : constant Natural :=
        (if Json_Sort.Is_Null then 0
         else Integer'Value (Json_Sort.Image));
      Json_Asc    : constant Harriet.Json.Json_Value'Class :=
        Request.Get_Property ("ascending");
      Ascending   : constant Boolean :=
        (Json_Asc.Is_Null or else Boolean'Value (Json_Asc.Image));
      package Sort_Maps is new Ada.Containers.Vectors (Positive, Positive);
      Sorted_Map  : Sort_Maps.Vector;
   begin

      for I in 1 .. M.Row_Count loop
         Sorted_Map.Append (I);
      end loop;

      if Sort_Column > 0 then

         declare
            use type Values.Model_Value_Type;
            function "<" (Left, Right : Positive) return Boolean
            is (if Ascending
                then M.Cell_Value (Left, Sort_Column)
                < M.Cell_Value (Right, Sort_Column)
                else M.Cell_Value (Left, Sort_Column)
                > M.Cell_Value (Right, Sort_Column));

            package Sorting is
              new Sort_Maps.Generic_Sorting ("<");
         begin
            Sorting.Sort (Sorted_Map);
         end;
      end if;

      for I in 1 .. M.Column_Count loop
         declare
            Heading : Harriet.Json.Json_Object;
         begin
            Heading.Set_Property ("id", M.Column_Heading_Id (I));
            Heading.Set_Property ("label", M.Column_Heading_Label (I));
            Headings.Append (Heading);
         end;
      end loop;

      Table.Set_Property ("headings", Headings);

      for Sorted_Row_Index in 1 .. M.Row_Count loop
         declare
            Source_Row_Index : constant Positive :=
              Sorted_Map.Element (Sorted_Row_Index);
            Row : Harriet.Json.Json_Object;
         begin
            for Col_Index in 1 .. M.Column_Count loop
               declare
                  Prop_Value : constant Values.Model_Value_Type :=
                    M.Cell_Value (Source_Row_Index, Col_Index);
                  Json_Value : constant Harriet.Json.Json_Value'Class :=
                    M.Column_Renderer (Col_Index).To_Json
                    (Prop_Value);
               begin
                  Row.Set_Property
                    (Name  => M.Column_Heading_Id (Col_Index),
                     Value => Json_Value);
               end;
            end loop;
            Rows.Append (Row);
         end;
      end loop;
      Table.Set_Property ("data", Rows);

      Response.Set_Property ("table", Table);
      return Response;

   exception
      when E : others =>
         return M.Error
           (State   => State,
            Client  => Client,
            Request => Request,
            Message => Ada.Exceptions.Exception_Message (E));

   end Handle;

end Harriet.UI.Models.Data_Source;
