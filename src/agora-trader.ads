with Agora.Commodity;
with Agora.Offer;

package Agora.Trader is

   type Instance is interface;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Any_Instance;

   procedure On_Bought
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type)
   is abstract;

   procedure On_Sold
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type)
   is abstract;

   procedure On_Failed_Offer
     (This       : in out Instance;
      Offer      : Agora.Offer.Offer_Type;
      Remaining  : Quantity_Type)
   is abstract;

   procedure Get_Last_Offer
     (This            : Instance;
      Commodity       : not null access constant Agora.Commodity.Any_Instance;
      Quantity        : out Quantity_Type;
      Price           : out Price_Type;
      Limit           : out Price_Type;
      Closed_Value    : out Money_Type;
      Closed_Quantity : out Quantity_Type)
   is abstract;

   function Last_Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type;

   function Last_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type;

   function Last_Close_Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type;

   function Last_Close_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type;

end Agora.Trader;
