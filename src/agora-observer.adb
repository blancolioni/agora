package body Agora.Observer is

   -------------------
   -- Observe_Trade --
   -------------------

   procedure Observe_Trade
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type)
   is
   begin
      if not This.Commodities.Contains (Commodity.Tag) then
         This.Commodities.Insert
           (Commodity.Tag, Observation_Vectors.Empty_Vector);
      end if;

      declare
         Vector : Observation_Vectors.Vector renames
                    This.Commodities (Commodity.Tag);
      begin
         Vector.Append (Observation_Record'
                          (Price    => Price,
                           Quantity => Quantity));
      end;
   end Observe_Trade;

   -------------------
   -- Trading_Range --
   -------------------

   function Trading_Range
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Price_Model_Type
   is
   begin
      if not This.Commodities.Contains (Commodity.Tag) then
         return (Price_Type'First, Price_Type'Last);
      end if;

      declare
         Vector : Observation_Vectors.Vector renames
                    This.Commodities (Commodity.Tag);
         Count  : Natural := Look_Back;
      begin
         return Model : Price_Model_Type :=
           (Price_Type'Last, Price_Type'First)
         do
            for Element of reverse Vector loop
               if Element.Price < Model.Low then
                  Model.Low := Element.Price;
               end if;
               if Element.Price > Model.High then
                  Model.High := Element.Price;
               end if;
               Count := Count - 1;
               exit when Count = 0;
            end loop;
         end return;
      end;
   end Trading_Range;

end Agora.Observer;
