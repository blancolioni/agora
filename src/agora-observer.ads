private with Ada.Containers.Vectors;
private with WL.String_Maps;

with Agora.Commodity;

package Agora.Observer is

   type Instance is tagged private;
   subtype Any_Instance is Instance'Class;

   type Reference is access all Any_Instance;

   procedure Observe_Trade
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type);

   function Trading_Range
     (This : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Price_Model_Type;

private

   type Observation_Record is
      record
         Price    : Price_Type;
         Quantity : Quantity_Type;
      end record;

   package Observation_Vectors is
     new Ada.Containers.Vectors (Positive, Observation_Record);

   package Commodity_Maps is
     new WL.String_Maps (Observation_Vectors.Vector, Observation_Vectors."=");

   type Instance is tagged
      record
         Commodities : Commodity_Maps.Map;
      end record;

end Agora.Observer;
