with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Numerics;

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with WL.Images.Bitmaps;

with Harriet.Calendar;
with Harriet.Color;
with Harriet.Orbits;
with Harriet.Solar_System;

with Harriet.Stars;

with Harriet.UI.Models.Data_Source;

with Harriet.Db.Massive_Object;
with Harriet.Db.Star;
with Harriet.Db.Star_System_Object;
with Harriet.Db.World;

with Harriet.Paths;

package body Harriet.UI.Models is

   package Palette_Vectors is
     new Ada.Containers.Vectors (Natural, Harriet.Color.Harriet_Color,
                                 Harriet.Color."=");

   Star_Spectrum_Palette : Palette_Vectors.Vector;

   procedure Load_Spectrum_Palette;

   -----------
   -- Close --
   -----------

   procedure Close
     (Model : in out Harriet_Model)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Harriet_Model'Class, Harriet_Model);
   begin
      Free (Model);
      Model := null;
   end Close;

   -----------
   -- Error --
   -----------

   function Error
     (Model   : Root_Harriet_Model'class;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Harriet.Json.Json_Value'Class;
      Message : String)
      return Harriet.Json.Json_Value'Class
   is
      pragma Unreferenced (State);
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error,
                "error in model " & Model.Name & " client" & Client'Image);
      Put_Line ("request: " & Request.Image);
      Put_Line (Standard_Error,
                "message: " & Message);
      return Response : Harriet.Json.Json_Object do
         Response.Set_Property ("error", Message);
      end return;

   end Error;

   ---------------------------
   -- Load_Spectrum_Palette --
   ---------------------------

   procedure Load_Spectrum_Palette is
      Tag               : constant String := "star_spectrum";
      Palette_File_Name : constant String :=
        Harriet.Paths.Config_File
          ("star-systems/palettes/" & Tag & ".bmp");
      Reader            : WL.Images.Bitmaps.Bitmap_Image_Reader;
      Image             : WL.Images.Image_Type;
   begin
      Reader.Read (Palette_File_Name, Image);

      for X in 1 .. Image.Width loop
         declare
            Color : constant WL.Images.Image_Color :=
              Image.Color (X, 1);
         begin
            Star_Spectrum_Palette.Append
              (Harriet.Color.Harriet_Color'
                 (Red   => Real (Color.Red) / 255.0,
                  Green => Real (Color.Green) / 255.0,
                  Blue  => Real (Color.Blue) / 255.0,
                  Alpha => 1.0));
         end;
      end loop;
   end Load_Spectrum_Palette;

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Object : Harriet.Db.Orbiting_Object.Orbiting_Object_Type)
      return Json.Json_Value'Class
   is

      use Harriet.Calendar;
      use Harriet.Db;

      Result : Json.Json_Object;

      procedure Set (Property_Name, Property_Value : String);
      procedure Set (Property_Name  : String;
                     Property_Value : Real);
      procedure Set (Property_Name  : String;
                     Property_Value : Json.Json_Value'Class);

      function Orbiting_Objects
        (Primary : Harriet.Db.Massive_Object_Reference)
         return Json.Json_Array;

      ----------------------
      -- Orbiting_Objects --
      ----------------------

      function Orbiting_Objects
        (Primary : Harriet.Db.Massive_Object_Reference)
         return Json.Json_Array
      is
      begin
         return Arr : Json.Json_Array do
            for Item of
              Harriet.Db.Star_System_Object.Select_By_Primary
                (Primary)
            loop
               Arr.Append (Serialize (Item));
            end loop;
         end return;
      end Orbiting_Objects;

      ---------
      -- Set --
      ---------

      procedure Set (Property_Name, Property_Value : String) is
      begin
         Result.Set_Property (Property_Name, Property_Value);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Property_Name  : String;
                     Property_Value : Real)
      is
      begin
         Result.Set_Property (Property_Name, Float (Property_Value));
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Property_Name  : String;
                     Property_Value : Json.Json_Value'Class)
      is
      begin
         Result.Set_Property (Property_Name, Property_Value);
      end Set;

   begin

      if Star_Spectrum_Palette.Is_Empty then
         Load_Spectrum_Palette;
      end if;

      Set ("title", Object.Name);
      Set ("name", Object.Name);
      Set ("orbit",
           Object.Semimajor_Axis / Harriet.Solar_System.Earth_Orbit);
      Set ("longitude",
           (if Object.Primary_Massive = Null_Massive_Object_Reference
            then 0.0
            else Harriet.Orbits.Calculate_Longitude
              (Massive_Object.Get (Object.Primary_Massive).Mass,
               Object.Semimajor_Axis,
               Harriet.Calendar.Clock - Object.Epoch))
           * Ada.Numerics.Pi / 180.0);
      Set ("year", Object.Period);

      if Object.Top_Record in R_Star | R_World then
         Set ("axisTilt",
              Harriet.Db.Star_System_Object.Get_Star_System_Object
                (Object.Get_Orbiting_Object_Reference)
              .Tilt);
      end if;

      Set ("dependents",
           Orbiting_Objects (Object.Get_Massive_Object_Reference));

      if Object.Top_Record = R_Star then
         declare
            Star  : constant Harriet.Stars.Star_Type'Class :=
              Harriet.Stars.Get
                (Harriet.Db.Star.Get_Star
                   (Object.Get_Orbiting_Object_Reference));
            Color : constant Harriet.Color.Harriet_Color :=
              Star_Spectrum_Palette.Element
                (Natural'Min
                   (Natural (Real'Max (Star.Temperature - 800.0, 0.0)
                    / 29200.0),
                    Star_Spectrum_Palette.Last_Index));
         begin
            Set ("type", "STAR");
            Set ("mass", Object.Mass / Harriet.Solar_System.Solar_Mass);
            Set ("radius",
                 Star.Radius / Harriet.Solar_System.Solar_Radius);
            Set ("temperature", Star.Temperature);
            Set ("red", Color.Red);
            Set ("green", Color.Green);
            Set ("blue", Color.Blue);
         end;
      elsif Object.Top_Record = R_World then
         declare
            World  : constant Harriet.Db.World.World_Type :=
              Harriet.Db.World.Get_World
                (Object.Get_Orbiting_Object_Reference);
         begin
            Set ("type", "WORLD");
            Set ("mass", Object.Mass / Harriet.Solar_System.Earth_Mass);
            Set ("radius",
                 World.Radius / Harriet.Solar_System.Earth_Radius);
            Set ("temperature", World.Average_Temperature);
            Set ("composition",
                 Harriet.Db.World_Composition'Image
                   (World.Composition));
            Set ("climate",
                 Harriet.Db.World_Climate'Image
                   (World.Climate));
            Set ("day", World.Rotation_Period / 3600.0);
         end;

      end if;

      return Result;

   exception
      when E : others =>
         raise Constraint_Error with
           "exception while serializing "
           & Record_Type'Image (Object.Top_Record)
           & " "
           & Object.Name
           & ": "
           & Ada.Exceptions.Exception_Message (E);

   end Serialize;

end Harriet.UI.Models;
