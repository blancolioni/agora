with Agora.Commodity;

package Agora.Offer is

   type Offer_Type is
      record
         Commodity    : Agora.Commodity.Reference;
         Quantity     : Quantity_Type;
         Price        : Price_Type;
         Limit_Price  : Price_Type;
      end record;

   function Image (Offer : Offer_Type) return String;

end Agora.Offer;
