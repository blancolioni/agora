with WL.Random;

package body Agora.Auction is

   -------------
   -- Add_Ask --
   -------------

   procedure Add_Ask
     (This   : in out Any_Instance;
      Trader : not null access Agora.Trader.Any_Instance;
      Offer  : Agora.Offer.Offer_Type)
   is
   begin
      This.Asks.Append
        (Offer_Record'
           (Trader    => Agora.Trader.Reference (Trader),
            Remaining => Offer.Quantity,
            Original  => Offer));
   end Add_Ask;

   -------------
   -- Add_Bid --
   -------------

   procedure Add_Bid
     (This   : in out Any_Instance;
      Trader : not null access Agora.Trader.Any_Instance;
      Offer  : Agora.Offer.Offer_Type)
   is
   begin
      This.Bids.Append
        (Offer_Record'
           (Trader    => Agora.Trader.Reference (Trader),
            Remaining => Offer.Quantity,
            Original  => Offer));
   end Add_Bid;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle (List : in out Offer_Lists.List) is
      Length    : constant Natural := Natural (List.Length);
      Available : array (1 .. Length) of Positive;
      Items     : array (1 .. Length) of Offer_Record;
      Last      : Natural := Available'Last;
   begin
      for I in Available'Range loop
         Available (I) := I;
      end loop;

      for Item of List loop
         declare
            Index : constant Positive :=
                      WL.Random.Random_Number (1, Last);
         begin
            Items (Available (Index)) := Item;
            Available (Index) := Available (Last);
            Last := Last - 1;
         end;
      end loop;

      List.Clear;

      for Item of Items loop
         List.Append (Item);
      end loop;

   end Shuffle;

end Agora.Auction;
