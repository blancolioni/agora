with Agora.Commodity;
with Agora.Exchange;
with Agora.Inventory;
with Agora.Observer;
with Agora.Offer;
with Agora.Profession;
with Agora.Trader;

package Agora.Behaviour is

   type Instance is interface;
   subtype Any_Instance is Instance'Class;

   type Reference is access constant Any_Instance;

   function Tag
     (This : Instance)
      return String
      is abstract;

   function Create_Bid
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type
      is abstract;

   function Create_Ask
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type
      is abstract;

   procedure Update_Price_Model
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Action     : Action_Type;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Success    : Boolean;
      Unit_Price : Price_Type;
      Model      : in out Price_Model_Type)
   is abstract;

   function Choose_Profession
     (This       : Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance)
      return Agora.Profession.Reference
      is abstract;

end Agora.Behaviour;
