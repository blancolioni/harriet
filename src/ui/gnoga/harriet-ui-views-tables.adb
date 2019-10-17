with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Ada.Text_IO;

with WL.Localisation;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Table;

with Concorde.UI.Views.Model_Views;

package body Concorde.UI.Views.Tables is

   subtype Row_Index is Concorde.UI.Models.Tables.Table_Row_Index;
   subtype Col_Index is Concorde.UI.Models.Tables.Table_Column_Index;

   package Base_View is
     new Concorde.UI.Views.Model_Views
       (Base_View_Type  => Root_View_Type,
        View_Model_Type => Concorde.UI.Models.Tables.Root_Table_Model);

   type Table_Properties is
      record
         Headings_Down : Boolean;
      end record;

   task type Table_View_Task is
      entry Start (Model : Concorde.UI.Models.Tables.Table_Model;
                   View  : Gnoga.Gui.View.Pointer_To_View_Class;
                   Props : Table_Properties);
      entry Update_Table;
   end Table_View_Task;

   type Table_View_Task_Access is access Table_View_Task;

   package Table_Cell_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Col_Index, Gnoga.Gui.Element.Table.Pointer_To_Table_Column_Class,
        Gnoga.Gui.Element.Table."=");

   package Table_Row_Vectors is
     new Ada.Containers.Vectors
       (Row_Index, Table_Cell_Vectors.Vector, Table_Cell_Vectors."=");

   type Root_Table_View is
     new Base_View.View_Type with
      record
         Properties    : Table_Properties;
         View_Task     : Table_View_Task_Access;
      end record;

   overriding procedure Model_Changed
     (View : in out Root_Table_View);

   type Table_View_Access is access all Root_Table_View'Class;

   overriding procedure Create
     (View    : not null access Root_Table_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String);

   procedure Load_Table
     (Table      : in out Gnoga.Gui.Element.Table.Table_Type;
      Model      : Concorde.UI.Models.Tables.Table_Model;
      Rows       : in out Table_Row_Vectors.Vector;
      Start      :    out Concorde.UI.Models.Tables.Table_Change_Cursor;
      Properties : Table_Properties);

   procedure Update_Table
     (Table      : in out Gnoga.Gui.Element.Table.Table_Type;
      Model      : Concorde.UI.Models.Tables.Table_Model;
      Rows       : in out Table_Row_Vectors.Vector;
      Previous   : in out Concorde.UI.Models.Tables.Table_Change_Cursor;
      Properties : Table_Properties);

   type Table_Gnoga_View is
     new Gnoga.Gui.View.View_Type with
      record
         Table : Table_View_Access;
      end record;

   type Table_Gnoga_View_Access is access all Table_Gnoga_View'Class;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (View    : not null access Root_Table_View;
      Session : not null access Concorde.Sessions.Root_Concorde_Session'Class;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Id      : String)
   is
      Gnoga_View   : constant Table_Gnoga_View_Access :=
                       new Table_Gnoga_View;
   begin
      Gnoga_View.Create (Parent, Id);
      Gnoga_View.Table := Table_View_Access (View);
      View.Create_With_Gnoga_View (Session, Gnoga_View);
      View.View_Task := new Table_View_Task;
      View.View_Task.Start
        (Model => View.Model,
         View  => Gnoga.Gui.View.Pointer_To_View_Class (Gnoga_View),
         Props => View.Properties);
   end Create;

   -----------------------
   -- Create_Table_View --
   -----------------------

   function Create_Table_View
     (Model         : not null access
        Concorde.UI.Models.Tables.Root_Table_Model'Class;
      Headings_Down : Boolean := False)
      return View_Type
   is
      View : constant Table_View_Access := new Root_Table_View;
   begin
      View.Properties :=
        (Headings_Down => Headings_Down);

      View.Set_Model (Model);
      return View_Type (View);
   end Create_Table_View;

   ----------------
   -- Load_Table --
   ----------------

   procedure Load_Table
     (Table      : in out Gnoga.Gui.Element.Table.Table_Type;
      Model      : Concorde.UI.Models.Tables.Table_Model;
      Rows       : in out Table_Row_Vectors.Vector;
      Start      :    out Concorde.UI.Models.Tables.Table_Change_Cursor;
      Properties : Table_Properties)
   is
      use Gnoga.Gui.Element.Table;
      Header      : Table_Header_Type;
      Tbody       : Table_Body_Type;
   begin
      Ada.Text_IO.Put_Line ("load table");
      Table.Inner_HTML ("");
      Rows.Clear;

      if Properties.Headings_Down then
         Tbody.Create (Table);

         for Column_Index in 1 .. Model.Column_Count loop
            declare
               Row         : Table_Row_Type;
               Heading     : Table_Heading_Type;
               Cell_Vector : Table_Cell_Vectors.Vector;
            begin
               Row.Create (Tbody);
               Heading.Create
                 (Row         => Row,
                  Content     =>
                    WL.Localisation.Local_Text
                      (Model.Column_Name (Column_Index)));

               for Row_Index in 1 .. Model.Row_Count loop
                  declare
                     Cell : Pointer_To_Table_Column_Class;
                  begin
                     Cell := new Table_Column_Type;
                     Cell.Create
                       (Row, Model.Image (Row_Index, Column_Index));
                     Cell_Vector.Append (Cell);
                  end;
               end loop;
               Rows.Append (Cell_Vector);
            end;
         end loop;
      else
         declare
            Heading_Row : Table_Row_Type;
         begin
            Header.Create (Table);
            Heading_Row.Create (Header);

            for Column_Index in 1 .. Model.Column_Count loop
               declare
                  Heading : Table_Heading_Type;
               begin
                  Heading.Create
                    (Row         => Heading_Row,
                     Content     =>
                       WL.Localisation.Local_Text
                         (Model.Column_Name (Column_Index)));
               end;
            end loop;
         end;

         Tbody.Create (Table);

         for Row_Index in 1 .. Model.Row_Count loop
            declare
               Row : Table_Row_Type;
               Cells : Table_Cell_Vectors.Vector;
            begin
               Row.Create (Tbody);
               for Column_Index in 1 .. Model.Column_Count loop
                  declare
                     Cell : Pointer_To_Table_Column_Class;
                  begin
                     Cell := new Table_Column_Type;
                     Cells.Append (Cell);
                     Cell.Create
                       (Row, Model.Image (Row_Index, Column_Index));
                  end;
               end loop;
               Rows.Append (Cells);
            end;
         end loop;
      end if;

      Start := Model.Current_Change;

   end Load_Table;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Root_Table_View)
   is
   begin
      View.View_Task.Update_Table;
   end Model_Changed;

   ---------------------
   -- Table_View_Task --
   ---------------------

   task body Table_View_Task is
      Gnoga_View      : Gnoga.Gui.View.Pointer_To_View_Class;
      Properties      : Table_Properties;
      M               : Concorde.UI.Models.Tables.Table_Model;
      Div             : Gnoga.Gui.Element.Common.DIV_Type;
      Table           : Gnoga.Gui.Element.Table.Table_Type;
      Rows            : Table_Row_Vectors.Vector;
      Previous_Change : Concorde.UI.Models.Tables.Table_Change_Cursor;
   begin
      accept Start (Model : in Concorde.UI.Models.Tables.Table_Model;
                    View : in Gnoga.Gui.View.Pointer_To_View_Class;
                    Props : in Table_Properties)
      do
         M := Model;
         Gnoga_View := View;
         Properties := Props;
      end Start;

      Div.Create (Gnoga_View.all);
      Div.Class_Name ("table-view");
      Div.Overflow_Y (Gnoga.Gui.Element.Auto);
      Table.Create (Div);
      Table.Class_Name ("darkTable");

      Load_Table (Table, M, Rows, Previous_Change, Properties);

      loop
         select
            accept Update_Table;
            Update_Table (Table, M, Rows, Previous_Change, Properties);
         or
            terminate;
         end select;
      end loop;
   end Table_View_Task;

   ------------------
   -- Update_Table --
   ------------------

   procedure Update_Table
     (Table      : in out Gnoga.Gui.Element.Table.Table_Type;
      Model      : Concorde.UI.Models.Tables.Table_Model;
      Rows       : in out Table_Row_Vectors.Vector;
      Previous   : in out Concorde.UI.Models.Tables.Table_Change_Cursor;
      Properties : Table_Properties)
   is
      pragma Unreferenced (Table, Properties);
      use Concorde.UI.Models.Tables;

      procedure Apply_Change (Change : Table_Change);

      ------------------
      -- Apply_Change --
      ------------------

      procedure Apply_Change (Change : Table_Change) is
      begin
         case Change.Change_Type is
            when Cell_Contents_Changed =>
               declare
                  Position : constant Cell_Position :=
                               Get_Cell_Position (Change);
               begin
                  Rows.Element (Position.Row).Element (Position.Col).Text
                    (Model.Image (Position.Row, Position.Col));
               end;
            when Row_Added =>
               null;
            when Row_Deleted =>
               null;
         end case;
      end Apply_Change;

      Changes : Table_Change_List;

   begin
      Model.Get_Changes (Changes, Previous);
      Scan_Changes (Changes, Apply_Change'Access);
   end Update_Table;

end Concorde.UI.Views.Tables;
