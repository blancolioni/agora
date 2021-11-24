with Agora.Commodity;
with Agora.Profession;

package Agora.Exchange is

   type Instance is interface;
   subtype Any_Instance is Instance'Class;

   type Reference is access all Any_Instance;

   function Average_Historical_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
   is abstract;

   function Average_Historical_Ask_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
      is abstract;

   function Average_Historical_Bid_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
      is abstract;

   function Average_Historical_Asks
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Quantity_Type
      is abstract;

   function Average_Historical_Bids
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Quantity_Type
      is abstract;

   function Highest_Historical_Demand
     (This      : Instance;
      Minimum   : Real)
      return Agora.Commodity.Reference
      is abstract;

   function Most_Profitable_Profession
     (This   : Instance;
      Rounds : Positive)
      return Agora.Profession.Reference
      is abstract;

   function Lowest_Historical_Price
     (This      : Instance;
      Rounds    : Positive;
      Exclude   : Agora.Commodity.Reference_Array :=
        Agora.Commodity.Empty)
      return Agora.Commodity.Reference
      is abstract;

   function Highest_Historical_Price
     (This      : Instance;
      Rounds    : Positive;
      Exclude   : Agora.Commodity.Reference_Array :=
        Agora.Commodity.Empty)
      return Agora.Commodity.Reference
      is abstract;

end Agora.Exchange;
