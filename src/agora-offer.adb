with Agora.Images;

package body Agora.Offer is

   -----------
   -- Image --
   -----------

   function Image (Offer : Offer_Type) return String is
      use Agora.Images;
      Total_Price : constant Money_Type :=
                      Offer.Quantity * Offer.Price;
      Total_Limit : constant Money_Type :=
                      Offer.Quantity * Offer.Limit_Price;
   begin
      return Image (Offer.Quantity) & " " & Offer.Commodity.Tag
        & " @ "
        & Image (Offer.Price)
        & "/"
        & Image (Offer.Limit_Price)
        & " each; total "
        & Image (Total_Price)
        & "/"
        & Image (Total_Limit);
   end Image;

end Agora.Offer;
