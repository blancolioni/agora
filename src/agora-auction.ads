private with Ada.Containers.Doubly_Linked_Lists;

with Agora.Ledger;
with Agora.Offer;
with Agora.Trader;

package Agora.Auction is

   type Instance is abstract tagged private;
   subtype Any_Instance is Instance'Class;

   procedure Add_Ask
     (This   : in out Any_Instance;
      Trader : not null access Agora.Trader.Any_Instance;
      Offer  : Agora.Offer.Offer_Type);

   procedure Add_Bid
     (This   : in out Any_Instance;
      Trader : not null access Agora.Trader.Any_Instance;
      Offer  : Agora.Offer.Offer_Type);

   procedure Resolve_Auction
     (This      : in out Instance;
      Ledger    : not null access Agora.Ledger.Any_Instance)
   is abstract;

private

   type Offer_Record is
      record
         Trader    : Agora.Trader.Reference;
         Remaining : Quantity_Type;
         Original  : Agora.Offer.Offer_Type;
      end record;

   function "<" (Left, Right : Offer_Record) return Boolean
   is (Left.Original.Price < Right.Original.Price);

   function ">" (Left, Right : Offer_Record) return Boolean
   is (Left.Original.Price > Right.Original.Price);

   package Offer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Offer_Record);

   package Ask_Offer_Sorting is
     new Offer_Lists.Generic_Sorting ("<");

   package Bid_Offer_Sorting is
     new Offer_Lists.Generic_Sorting (">");

   procedure Shuffle (List : in out Offer_Lists.List);

   type Instance is abstract tagged
      record
         Asks : Offer_Lists.List;
         Bids : Offer_Lists.List;
      end record;

end Agora.Auction;
