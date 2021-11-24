with Agora.Commodity;
with Agora.Trader;

package Agora.Ledger is

   type Instance is interface;
   subtype Any_Instance is Instance'Class;

   procedure Book_Sale
     (This      : in out Instance;
      Seller    : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type)
   is abstract;

   procedure Book_Purchase
     (This      : in out Instance;
      Buyer     : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type)
   is abstract;

end Agora.Ledger;
