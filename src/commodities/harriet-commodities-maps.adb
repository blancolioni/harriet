with Harriet.Db.Commodity;

package body Harriet.Commodities.Maps is

   function Key (Commodity : Commodity_Reference) return String
   is (Harriet.Db.Commodity.Get (Commodity).Tag);

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Map;
      Commodity : Commodity_Reference)
      return Boolean
   is
   begin
      return Container.Contains (Key (Commodity));
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Commodity : Commodity_Reference;
      Value     :        Element_Type)
   is
   begin
      Container.Insert (Key (Commodity), Value);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure
        (Commodity : Commodity_Reference; Element : Element_Type))
   is
   begin
      for Position in Container.Iterate loop
         Process (Get (Tag_Maps.Key (Position)),
                  Tag_Maps.Element (Position));
      end loop;
   end Iterate;

   ------------
   -- Update --
   ------------

   procedure Update
     (Container : in out Map; Commodity : Commodity_Reference;
      Process   :    not null access procedure (Value : in out Element_Type))
   is
   begin
      Process (Container (Key (Commodity)));
   end Update;

end Harriet.Commodities.Maps;
