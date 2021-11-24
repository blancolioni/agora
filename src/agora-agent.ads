private with WL.String_Maps;

with Agora.Behaviour;
with Agora.Commodity;
with Agora.Exchange;
with Agora.Inventory;
with Agora.Observer;
with Agora.Offer;
with Agora.Profession;
with Agora.Trader;

package Agora.Agent is

   subtype Parent is Agora.Trader.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;

   type Reference is access all Any_Instance;

   function Id (This : Any_Instance) return Agent_Id;
   function Log_Id (This : Any_Instance) return String;
   function Current_Cash (This : Any_Instance) return Money_Type;
   function Last_Cash (This : Any_Instance) return Money_Type;
   function Last_Profit (This : Any_Instance) return Money_Type;
   function Profession (This : Any_Instance) return Agora.Profession.Reference;

   procedure Start_Round
     (This : in out Any_Instance);

   procedure Execute_Profession
     (This : in out Any_Instance);

   procedure Generate_Offers
     (This      : not null access Any_Instance;
      Exchange  : not null access Agora.Exchange.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Create    : not null access
        procedure (Action : Action_Type;
                   Offer  : Agora.Offer.Offer_Type));

   procedure Transfer
     (From, To  : Reference;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Cost      : Money_Type);

   procedure Update_Price_Model
     (This       : not null access Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Action     : Action_Type;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Success    : Boolean;
      Unit_Price : Price_Type);

   procedure Log
     (This    : Any_Instance;
      Message : String);

   function Create
     (Id         : Agent_Id;
      Behaviour  : not null access constant Agora.Behaviour.Any_Instance;
      Inventory  : not null access Agora.Inventory.Any_Instance;
      Profession : not null access constant Agora.Profession.Any_Instance)
      return Reference;

   procedure Reset
     (Agent      : in out Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance);

   procedure Destroy (This : in out Reference);

private

   package Price_Model_Maps is
     new WL.String_Maps (Price_Model_Type);

   type Offer_Record is
      record
         Quantity        : Quantity_Type := 0.0;
         Price           : Price_Type    := 1.0;
         Limit           : Price_Type    := 1.0;
         Closed_Value    : Money_Type    := 0.0;
         Closed_Quantity : Quantity_Type := 0.0;
      end record;

   package Offer_Maps is new WL.String_Maps (Offer_Record);

   type Instance is new Parent with
      record
         Id           : Agent_Id;
         Last_Cash    : Money_Type;
         Last_Offers  : Offer_Maps.Map;
         Bid_Space    : Quantity_Type;
         Behaviour    : Agora.Behaviour.Reference;
         Inventory    : Agora.Inventory.Reference;
         Observer     : Agora.Observer.Reference;
         Profession   : Agora.Profession.Reference;
         Price_Model  : Price_Model_Maps.Map;
         Confidence   : Unit_Real;
      end record;

   overriding procedure On_Bought
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type);

   overriding procedure On_Sold
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type);

   overriding procedure On_Failed_Offer
     (This       : in out Instance;
      Offer      : Agora.Offer.Offer_Type;
      Remaining  : Quantity_Type);

   overriding procedure Get_Last_Offer
     (This            : Instance;
      Commodity       : not null access constant Agora.Commodity.Any_Instance;
      Quantity        : out Quantity_Type;
      Price           : out Price_Type;
      Limit           : out Price_Type;
      Closed_Value    : out Money_Type;
      Closed_Quantity : out Quantity_Type);

   function Id (This : Any_Instance) return Agent_Id
   is (This.Id);

   function Current_Cash (This : Any_Instance) return Money_Type
   is (This.Inventory.Cash);

   function Last_Cash (This : Any_Instance) return Money_Type
   is (This.Last_Cash);

   function Last_Profit (This : Any_Instance) return Money_Type
   is (This.Current_Cash - This.Last_Cash);

   function Profession (This : Any_Instance) return Agora.Profession.Reference
   is (This.Profession);

   function Log_Id (This : Any_Instance) return String
   is ("Agent" & This.Id'Image & " " & This.Profession.Tag);

end Agora.Agent;
