with Harriet.Game;
with Harriet.Json;

package Harriet.UI.Web_UI.Requests is

   function Game_State
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Current_Phase
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Faction_Names
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Faction_State
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Fleet_State
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Legion_State
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Republic_State
     (Game : Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

   function Continue
     (Game : in out Harriet.Game.Game_Type)
      return Harriet.Json.Json_Value'Class;

end Harriet.UI.Web_UI.Requests;
