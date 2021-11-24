package body Agora.Trader is

   ----------------------
   -- Last_Close_Price --
   ----------------------

   function Last_Close_Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type
   is
      Quantity        : Quantity_Type;
      Price           : Price_Type;
      Limit           : Price_Type;
      Closed_Value    : Money_Type;
      Closed_Quantity : Quantity_Type;
   begin
      This.Get_Last_Offer (Commodity, Quantity, Price, Limit,
                           Closed_Value, Closed_Quantity);
      if Closed_Quantity > 0.0 then
         return Price_Delta_Type'Max
           (Closed_Value / Closed_Quantity, Minimum_Price);
      else
         return 1.0;
      end if;
   end Last_Close_Price;

   -------------------------
   -- Last_Close_Quantity --
   -------------------------

   function Last_Close_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
      Quantity        : Quantity_Type;
      Price           : Price_Type;
      Limit           : Price_Type;
      Closed_Value    : Money_Type;
      Closed_Quantity : Quantity_Type;
   begin
      This.Get_Last_Offer (Commodity, Quantity, Price, Limit,
                           Closed_Value, Closed_Quantity);
      return Closed_Quantity;
   end Last_Close_Quantity;

   ----------------
   -- Last_Price --
   ----------------

   function Last_Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type
   is
      Quantity        : Quantity_Type;
      Price           : Price_Type;
      Limit           : Price_Type;
      Closed_Value    : Money_Type;
      Closed_Quantity : Quantity_Type;
   begin
      This.Get_Last_Offer (Commodity, Quantity, Price, Limit,
                           Closed_Value, Closed_Quantity);
      return Price;
   end Last_Price;

   -------------------
   -- Last_Quantity --
   -------------------

   function Last_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
      Quantity        : Quantity_Type;
      Price           : Price_Type;
      Limit           : Price_Type;
      Closed_Value    : Money_Type;
      Closed_Quantity : Quantity_Type;
   begin
      This.Get_Last_Offer (Commodity, Quantity, Price, Limit,
                           Closed_Value, Closed_Quantity);
      return Quantity;
   end Last_Quantity;

end Agora.Trader;
