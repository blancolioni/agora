with Agora.Images;
with Agora.Logging;

package body Agora.Auction.Asymmetric is

   subtype Parent is Agora.Auction.Instance;

   type Instance is new Parent with
      record
         null;
      end record;

   overriding procedure Resolve_Auction
     (This   : in out Instance;
      Ledger : not null access Agora.Ledger.Any_Instance);

   procedure Resolve_At_Price
     (All_Asks : in out Offer_Lists.List;
      All_Bids : in out Offer_Lists.List;
      Ledger   : not null access Agora.Ledger.Any_Instance;
      Finished :    out Boolean);

   function Calculate_Bid_Quantity
     (Base_Price  : Price_Type;
      Limit_Price : Price_Type;
      Ask_Price   : Price_Type;
      Original    : Quantity_Type;
      Remaining   : Quantity_Type)
      return Quantity_Type;

   ----------------------------
   -- Calculate_Bid_Quantity --
   ----------------------------

   function Calculate_Bid_Quantity
     (Base_Price  : Price_Type;
      Limit_Price : Price_Type;
      Ask_Price   : Price_Type;
      Original    : Quantity_Type;
      Remaining   : Quantity_Type)
      return Quantity_Type
   is
      Quantity : constant Quantity_Type :=
                   (if Ask_Price <= Base_Price
                    then Original
                    elsif Ask_Price >= Limit_Price
                    then 0.0
                    else Quantity_Type
                      (1.0 - (Real (Ask_Price) - Real (Base_Price))
                       / (Real (Limit_Price) - Real (Base_Price)))
                    * Original);
   begin
      return Quantity_Type'Min (Quantity, Remaining);
   end Calculate_Bid_Quantity;

   -----------------
   -- Get_Auction --
   -----------------

   function Get_Auction return Any_Instance is
   begin
      return This : Instance;
   end Get_Auction;

   ----------------------
   -- Resolve_At_Price --
   ----------------------

   procedure Resolve_At_Price
     (All_Asks : in out Offer_Lists.List;
      All_Bids : in out Offer_Lists.List;
      Ledger   : not null access Agora.Ledger.Any_Instance;
      Finished :    out Boolean)
   is
      type Limit_Bid_Record is
         record
            Auction_Bid     : Offer_Record;
            Limit_Quantity  : Quantity_Type;
            Bought_Quantity : Quantity_Type;
         end record;

      package Limit_Bid_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Limit_Bid_Record);

      Price        : constant Price_Type :=
                       All_Asks.First_Element.Original.Price;
      Asks         : Offer_Lists.List;
      Limit_Bids   : Limit_Bid_Lists.List;
      Delayed_Bids : Offer_Lists.List;
      Ask_Quantity : Quantity_Type := 0.0;
      Bid_Quantity : Quantity_Type := 0.0;

   begin

      Finished := False;

      while not All_Asks.Is_Empty
        and then All_Asks.First_Element.Original.Price = Price
      loop
         Ask_Quantity := Ask_Quantity + All_Asks.First_Element.Remaining;
         Asks.Append (All_Asks.First_Element);
         All_Asks.Delete_First;
      end loop;

      for Bid of All_Bids loop
         if Bid.Original.Limit_Price > Price then
            declare
               Quantity : constant Quantity_Type :=
                            Calculate_Bid_Quantity
                              (Base_Price  => Bid.Original.Price,
                               Limit_Price => Bid.Original.Limit_Price,
                               Ask_Price   => Price,
                               Original    => Bid.Original.Quantity,
                               Remaining   => Bid.Remaining);
            begin
               Limit_Bids.Append
                 (Limit_Bid_Record'
                    (Auction_Bid     => Bid,
                     Limit_Quantity  => Quantity,
                     Bought_Quantity => 0.0));
               Bid_Quantity := Bid_Quantity + Quantity;
            end;
         else
            Delayed_Bids.Append (Bid);
         end if;
      end loop;

      if Limit_Bids.Is_Empty or else Bid_Quantity = 0.0 then
         Agora.Logging.Log
           ("no bidders at " & Agora.Images.Image (Price));
         Finished := True;
         return;
      end if;

      declare
         Traded_Quantity : constant Quantity_Type :=
                             Quantity_Type'Min (Ask_Quantity, Bid_Quantity);
         Ask_Factor      : constant Quantity_Type :=
                             (if Traded_Quantity < Ask_Quantity
                              then Traded_Quantity / Ask_Quantity
                              else 1.0);
         Bid_Factor      : constant Quantity_Type :=
                             (if Traded_Quantity < Bid_Quantity
                              then Traded_Quantity / Bid_Quantity
                              else 1.0);
      begin
         for Ask of Asks loop
            declare
               Sold : constant Quantity_Type :=
                        Round (Ask.Original.Quantity * Ask_Factor);
            begin
               Ask.Remaining := Ask.Remaining - Sold;
               Ledger.Book_Sale
                 (Seller    => Ask.Trader,
                  Commodity => Ask.Original.Commodity,
                  Quantity  => Sold,
                  Price     => Price);
               Ask.Trader.On_Sold
                 (Ask.Original.Commodity, Sold, Price);
               if Ask.Remaining > 0.0 then
                  Ask.Trader.On_Failed_Offer
                    (Ask.Original, Ask.Remaining);
               end if;
            end;
         end loop;

         for Limit_Bid of Limit_Bids loop
            declare
               Bought : constant Quantity_Type :=
                          Round (Limit_Bid.Limit_Quantity * Bid_Factor);
               Bid    : Offer_Record renames Limit_Bid.Auction_Bid;
               Offer  : Agora.Offer.Offer_Type renames
                          Bid.Original;
            begin
               Ledger.Book_Purchase
                 (Buyer     => Bid.Trader,
                  Commodity => Bid.Original.Commodity,
                  Quantity  => Bought,
                  Price     => Price);
               Bid.Remaining := Bid.Remaining - Bought;
               Bid.Trader.On_Bought
                 (Offer.Commodity, Bought, Price);
            end;
         end loop;
      end;

      All_Bids.Clear;

      for Bid of Limit_Bids loop
         if Bid.Bought_Quantity < Bid.Limit_Quantity then
            All_Bids.Append (Bid.Auction_Bid);
         end if;
      end loop;

      for Bid of Delayed_Bids loop
         All_Bids.Append (Bid);
      end loop;

      Finished := All_Bids.Is_Empty;

   end Resolve_At_Price;

   ---------------------
   -- Resolve_Auction --
   ---------------------

   overriding procedure Resolve_Auction
     (This   : in out Instance;
      Ledger : not null access Agora.Ledger.Any_Instance)
   is
      Finished : Boolean := False;
   begin

      Shuffle (This.Bids);

      Ask_Offer_Sorting.Sort (This.Asks);
      Bid_Offer_Sorting.Sort (This.Bids);
      while not This.Asks.Is_Empty
        and then not This.Bids.Is_Empty
        and then not Finished
      loop
         Resolve_At_Price (This.Asks, This.Bids, Ledger, Finished);
      end loop;

   end Resolve_Auction;

end Agora.Auction.Asymmetric;
