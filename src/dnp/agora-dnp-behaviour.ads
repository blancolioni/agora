with Agora.Behaviour;
with Agora.Commodity;
with Agora.Exchange;
with Agora.Inventory;
with Agora.Observer;
with Agora.Offer;
with Agora.Profession;
with Agora.Trader;

package Agora.Dnp.Behaviour is

   subtype Parent is Agora.Behaviour.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;

   type Reference is access constant Any_Instance;

   overriding function Tag
     (This : Instance)
      return String;

   overriding function Create_Bid
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type;

   overriding function Create_Ask
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type;

   overriding procedure Update_Price_Model
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Action     : Action_Type;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Success    : Boolean;
      Unit_Price : Price_Type;
      Model      : in out Price_Model_Type);

   overriding function Choose_Profession
     (This       : Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance)
      return Agora.Profession.Reference;

   function Singleton return Reference;

private

   type Instance is new Parent with
      record
         Look_Back : Positive := 10;
      end record;

   overriding function Tag
     (This : Instance)
      return String
   is ("dnp");

end Agora.Dnp.Behaviour;
