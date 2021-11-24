private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

with Agora.Agent;
with Agora.Commodity;
with Agora.Exchange;
with Agora.Ledger;
with Agora.Offer;
with Agora.Profession;
with Agora.Trader;

package Agora.Market is

   subtype Parent is Agora.Exchange.Instance;

   type Instance is new Parent and Agora.Ledger.Instance with private;

   subtype Any_Instance is Instance'Class;

   type Reference is access all Any_Instance;

   function Name (This : Any_Instance) return String;

   procedure Create_Offer
     (This      : in out Any_Instance;
      Agent     : not null access Agora.Agent.Any_Instance;
      Action    : Action_Type;
      Offer     : Agora.Offer.Offer_Type);

   overriding function Average_Historical_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type;

   overriding function Average_Historical_Asks
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Quantity_Type;

   overriding function Average_Historical_Bids
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Quantity_Type;

   overriding function Highest_Historical_Demand
     (This      : Instance;
      Minimum   : Real)
      return Agora.Commodity.Reference;

   overriding function Most_Profitable_Profession
     (This   : Instance;
      Rounds : Positive)
      return Agora.Profession.Reference;

   overriding function Lowest_Historical_Price
     (This      : Instance;
      Rounds    : Positive;
      Exclude   : Agora.Commodity.Reference_Array :=
        Agora.Commodity.Empty)
      return Agora.Commodity.Reference;

   overriding function Highest_Historical_Price
     (This      : Instance;
      Rounds    : Positive;
      Exclude   : Agora.Commodity.Reference_Array :=
        Agora.Commodity.Empty)
      return Agora.Commodity.Reference;

   procedure Execute (This : not null access Any_Instance);

private

   subtype Dispatch is Parent'Class;

   package Agent_Vectors is
     new Ada.Containers.Vectors
       (Agent_Id, Agora.Agent.Reference, Agora.Agent."=");

   type Offer_Record is
      record
         Agent     : Agora.Agent.Reference;
         Offer     : Agora.Offer.Offer_Type;
      end record;

   function "<" (Left, Right : Offer_Record) return Boolean
   is (Left.Offer.Price < Right.Offer.Price);

   function ">" (Left, Right : Offer_Record) return Boolean
   is (Left.Offer.Price > Right.Offer.Price);

   package Offer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   function Total (List : Offer_Lists.List) return Quantity_Type;
   function Value (List : Offer_Lists.List) return Money_Type;
   function Average_Price (List : Offer_Lists.List) return Price_Type;

   package Ask_Offer_Sorting is
     new Offer_Lists.Generic_Sorting ("<");

   package Bid_Offer_Sorting is
     new Offer_Lists.Generic_Sorting (">");

   type Commodity_Market is array (Action_Type) of Offer_Lists.List;

   package Commodity_Market_Maps is
     new WL.String_Maps (Commodity_Market);

   type History_Record is
      record
         Askers      : Natural;
         Bidders     : Natural;
         Asks        : Quantity_Type;
         Bids        : Quantity_Type;
         Traded      : Quantity_Type;
         Ask_Value   : Money_Type;
         Bid_Value   : Money_Type;
         Trade_Value : Money_Type;
      end record;

   package History_Lists is
     new Ada.Containers.Doubly_Linked_Lists (History_Record);

   package Commodity_History_Maps is
     new WL.String_Maps (History_Lists.List, History_Lists."=");

   type Profession_Record is
      record
         Profit : Money_Type;
      end record;

   package Profession_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Profession_Record);

   package Profession_Profit_Maps is
     new WL.String_Maps (Profession_Lists.List, Profession_Lists."=");

   type Instance is new Parent and Agora.Ledger.Instance with
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Agents      : Agent_Vectors.Vector;
         Offers      : Commodity_Market_Maps.Map;
         History     : Commodity_History_Maps.Map;
         Professions : Profession_Profit_Maps.Map;
      end record;

   overriding procedure Book_Sale
     (This      : in out Instance;
      Seller    : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type);

   overriding procedure Book_Purchase
     (This      : in out Instance;
      Buyer     : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type);

   overriding function Average_Historical_Ask_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type;

   overriding function Average_Historical_Bid_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type;

end Agora.Market;
