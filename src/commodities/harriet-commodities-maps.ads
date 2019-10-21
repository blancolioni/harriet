private with WL.String_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Harriet.Commodities.Maps is

   type Map is tagged private;

   function Contains
     (Container : Map;
      Commodity : Commodity_Reference)
      return Boolean;

   procedure Insert
     (Container : in out Map;
      Commodity : Commodity_Reference;
      Value     : Element_Type);

   procedure Update
     (Container : in out Map;
      Commodity : Commodity_Reference;
      Process   : not null access
        procedure (Value : in out Element_Type));

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Commodity : Commodity_Reference;
                   Element : Element_Type));

private

   package Tag_Maps is
     new WL.String_Maps (Element_Type, "=");

   type Map is new Tag_Maps.Map with null record;

end Harriet.Commodities.Maps;
