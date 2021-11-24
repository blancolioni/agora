with Agora.Images;
with Agora.Logging;
with Agora.Random;

package body Agora.Dnp.Behaviour is

   Rec : aliased constant Instance := Instance'
     (Parent with Look_Back => <>);

   Significant_Price_Delta : constant := 0.25;
   Significant_Imbalance   : constant := 0.33;
   Low_Inventory           : constant Real := 0.1;
   High_Inventory          : constant Real := 2.0;

   function Create_Price
     (Model : Price_Model_Type)
      return Price_Type
     with Unreferenced;

   function Determine_Purchase_Quantity
     (Exchange  : not null access constant Agora.Exchange.Any_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Observer  : not null access constant Agora.Observer.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Quantity_Type
     with Unreferenced;

   function Determine_Sale_Quantity
     (Exchange  : not null access constant Agora.Exchange.Any_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Observer  : not null access constant Agora.Observer.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Quantity_Type;

   function Position_In_Range
     (Value, Low, High : Price_Type)
      return Real
   is (if Value <= Low
       then 0.0
       elsif Value >= High
       then 1.0
       else Real (Value - Low) / Real (High - Low));

   -----------------------
   -- Choose_Profession --
   -----------------------

   overriding function Choose_Profession
     (This       : Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance)
      return Agora.Profession.Reference
   is
      use type Agora.Commodity.Reference;
      Most_Profitable : constant Agora.Profession.Reference :=
                          Exchange.Most_Profitable_Profession (10);
      Best_Commodity  : constant Agora.Commodity.Reference :=
                          Exchange.Highest_Historical_Demand (1.5);
   begin
      if Best_Commodity /= null then
         for Profession of Agora.Profession.All_Professions loop
            if Profession.Production = Best_Commodity then
               return Profession;
            end if;
         end loop;
      end if;
      return Most_Profitable;
   end Choose_Profession;

   ----------------
   -- Create_Ask --
   ----------------

   overriding function Create_Ask
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type
   is
      Asks             : constant Quantity_Type :=
                           Quantity_Type'Max
                             (Exchange.Average_Historical_Asks (Commodity, 10),
                              0.5);
      Bids             : constant Quantity_Type :=
                           Quantity_Type'Max
                             (Exchange.Average_Historical_Bids (Commodity, 10),
                              0.5);
      Historical_Price : constant Price_Type :=
                           Exchange.Average_Historical_Price
                             (Commodity, 10);
      Ratio            : constant Real :=
                           Real (Bids - Asks) / Real (Bids + Asks);
      Markup           : constant Real :=
                           (1.0 + Ratio) / 2.0 * Confidence + 1.0;
      Historical_Markup : constant Real :=
                            Ratio / 4.0 * Confidence + 1.0;
      Min_Ask_Price     : constant Price_Type :=
                            Inventory.Price (Commodity);
      Cost_With_Profit  : constant Price_Type :=
                            Min_Ask_Price
                              * Price_Type (Markup);
      Min_Hist_Price    : constant Price_Type :=
                            Price_Delta_Type'Max
                              (Price_Delta_Type
                                 (Real (Historical_Price)
                                  * Historical_Markup),
                               Price_Type'First);
      Last_Quantity       : constant Quantity_Type :=
                              Trader.Last_Quantity (Commodity);
      Last_Price          : constant Price_Type :=
                              Trader.Last_Price (Commodity);
      Last_Close_Quantity : constant Quantity_Type :=
                              Trader.Last_Close_Quantity (Commodity);
      Last_Close_Price    : constant Price_Type :=
                              Trader.Last_Close_Price (Commodity)
                              with Unreferenced;

      Price             : constant Price_Type :=
      --  Inventory.Price (Commodity) * Price_Type'(1.02);
                              (if Last_Close_Quantity = Last_Quantity
                               and then Last_Price > Min_Ask_Price
                               then Last_Price
                               * Price_Delta_Type
                                 (1.0 + Agora.Random.Unit_Random / 5.0)
                               elsif Min_Ask_Price >= Min_Hist_Price
                               then Min_Ask_Price * 1.02
                               elsif Cost_With_Profit > Min_Hist_Price
                               and then Last_Close_Quantity < Last_Quantity
                               then Min_Hist_Price
                               elsif Cost_With_Profit >= Min_Hist_Price
                               then Price_Type
                                 ((Real (Cost_With_Profit)
                                  - Real (Min_Hist_Price))
                                  * Agora.Random.Unit_Random
                                  + Real (Min_Hist_Price))
                               else Price_Type
                                 ((Real (Min_Hist_Price)
                                  - Real (Min_Ask_Price))
                                  * Agora.Random.Unit_Random
                                  + Real (Min_Ask_Price)));
      --  Price_Type'Max
      --    (Min_Ask_Price, Min_Hist_Price);
                         --  Price_Type'Max
                         --    (Create_Price (Model),
                         --     Inventory.Price (Commodity) * 1.02);
      Ideal_Quantity : constant Quantity_Type :=
                         Determine_Sale_Quantity
                           (Exchange  => Exchange,
                            Inventory => Inventory,
                            Observer  => Observer,
                            Commodity => Commodity,
                            Look_Back => This.Look_Back);
      Have              : constant Quantity_Type :=
                            Inventory.Quantity (Commodity);
      Quantity          : constant Quantity_Type :=
                            Quantity_Type'Min
                              (Quantity_Type'Max (Ideal_Quantity, Limit),
                               Have);
      Last_Ask_Quantity : Quantity_Type;
      Last_Ask_Price    : Price_Type;
      Last_Limit_Price  : Price_Type;
      Sold_Quantity     : Quantity_Type;
      Sold_Value        : Money_Type;
   begin
      Trader.Get_Last_Offer
        (Commodity       => Commodity,
         Quantity        => Last_Ask_Quantity,
         Price           => Last_Ask_Price,
         Limit           => Last_Limit_Price,
         Closed_Value    => Sold_Value,
         Closed_Quantity => Sold_Quantity);

      if Last_Ask_Quantity > 0.0 then
         Agora.Logging.Log
           ("last time: ask = "
            & Agora.Images.Image (Last_Ask_Quantity)
            & " @ " & Agora.Images.Image (Last_Ask_Price)
            & " total "
            & Agora.Images.Image
              (Money_Type'(Last_Ask_Quantity * Last_Ask_Price))
            & "; sold " & Agora.Images.Image (Sold_Quantity)
            & " for " & Agora.Images.Image (Sold_Value));
      end if;

      Agora.Logging.Log
        ("ask price: asks=" & Agora.Images.Image (Asks)
         & "; bids=" & Agora.Images.Image (Bids)
         & "; ratio=" & Agora.Images.Image (Ratio)
         & "; confidence=" & Agora.Images.Image (Confidence)
         & "; markup=" & Agora.Images.Image (Markup)
         & "; min ask price (cost)=" & Agora.Images.Image (Min_Ask_Price)
         & " with markup=" & Agora.Images.Image (Cost_With_Profit)
         & "; historical price=" & Agora.Images.Image (Historical_Price)
         & "; historical markup=" & Agora.Images.Image (Historical_Markup)
         & "; max ask price (history)=" & Agora.Images.Image (Min_Hist_Price)
         & (if Last_Quantity > 0.0
           then "; last ask price=" & Agora.Images.Image (Last_Price)
           & "; closed offers="
           & Agora.Images.Image
             (Real'(Real (Last_Close_Quantity
              / Last_Quantity) * 100.0))
           & "%"
           else "")
         & "; final price=" & Agora.Images.Image (Price));
      return Agora.Offer.Offer_Type'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    => Quantity,
         Price       => Price,
         Limit_Price => Price);
   end Create_Ask;

   ----------------
   -- Create_Bid --
   ----------------

   overriding function Create_Bid
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Observer   : not null access constant Agora.Observer.Any_Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Confidence : Unit_Real;
      Model      : Price_Model_Type;
      Limit      : Quantity_Type)
      return Agora.Offer.Offer_Type
   is
      Historical_Price : constant Price_Type :=
                           Exchange.Average_Historical_Price
                             (Commodity, This.Look_Back);
      Last_Quantity       : constant Quantity_Type :=
                              Trader.Last_Quantity (Commodity);
      Last_Price          : constant Price_Type :=
                              Trader.Last_Price (Commodity);
      Last_Close_Quantity : constant Quantity_Type :=
                              Trader.Last_Close_Quantity (Commodity);
      Price               : constant Price_Type :=
                              (if Last_Close_Quantity = Last_Quantity
                               then Price_Type'Min
                                 (Last_Price, Historical_Price)
                               elsif Last_Close_Quantity = 0.0
                               then Historical_Price * Price_Type'(1.05)
                               else Last_Price * Price_Type
                                 (1.0 + Real (Last_Close_Quantity)
                                    / Real (Last_Quantity) / 2.0));
      Ideal_Limit      : constant Price_Type :=
                           Price * Price_Type (3.0 - Confidence);
      Historical_Limit : constant Price_Type :=
                           Historical_Price * Price_Type (2.0 - Confidence);
      Limit_Price    : constant Price_Type :=
                           Price_Type'Max (Ideal_Limit, Historical_Limit);
      Quantity       : constant Quantity_Type := Limit;
      Last_Bid_Quantity : Quantity_Type;
      Last_Bid_Price    : Price_Type;
      Last_Limit_Price  : Price_Type;
      Bought_Quantity   : Quantity_Type;
      Bought_Value      : Money_Type;
   begin
      Trader.Get_Last_Offer
        (Commodity       => Commodity,
         Quantity        => Last_Bid_Quantity,
         Price           => Last_Bid_Price,
         Limit           => Last_Limit_Price,
         Closed_Value    => Bought_Value,
         Closed_Quantity => Bought_Quantity);

      if Last_Bid_Quantity > 0.0 then
         Agora.Logging.Log
           ("last time: bid = "
            & Agora.Images.Image (Last_Bid_Quantity)
            & " @ " & Agora.Images.Image (Last_Bid_Price)
            & "/" & Agora.Images.Image (Last_Limit_Price)
            & " total "
            & Agora.Images.Image
              (Money_Type'(Last_Bid_Quantity * Last_Bid_Price))
            & "/"
            & Agora.Images.Image
              (Money_Type'(Last_Bid_Quantity * Last_Limit_Price))
            & "; bought " & Agora.Images.Image (Bought_Quantity)
            & " for " & Agora.Images.Image (Bought_Value));
      end if;

      Agora.Logging.Log
        ("bid price: historical=" & Agora.Images.Image (Historical_Price)
         & "; ideal price=" & Agora.Images.Image (Price)
         & "; ideal limit price=" & Agora.Images.Image (Ideal_Limit)
         & "; historical limit=" & Agora.Images.Image (Historical_Limit)
         & "; limit price=" & Agora.Images.Image (Limit_Price)
         & "; ideal quantity=" & Agora.Images.Image (Quantity));
      return Agora.Offer.Offer_Type'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    => Quantity,
         Price       => Price,
         Limit_Price => Limit_Price);
   end Create_Bid;

   ------------------
   -- Create_Price --
   ------------------

   function Create_Price
     (Model : Price_Model_Type)
      return Price_Type
   is
   begin
      return Agora.Random.Unit_Random * (Model.High - Model.Low)
        + Model.Low;
   end Create_Price;

   ---------------------------------
   -- Determine_Purchase_Quantity --
   ---------------------------------

   function Determine_Purchase_Quantity
     (Exchange  : not null access constant Agora.Exchange.Any_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Observer  : not null access constant Agora.Observer.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Quantity_Type
   is
      Public_Mean   : constant Price_Type :=
                        Exchange.Average_Historical_Price
                          (Commodity, Look_Back);
      Price_Range   : constant Price_Model_Type :=
                        Observer.Trading_Range (Commodity, Look_Back);
      Favourability : constant Real :=
                        1.0 -
                          Position_In_Range
                            (Public_Mean, Price_Range.Low, Price_Range.High);
      Quantity      : constant Quantity_Type :=
                        Favourability * Inventory.Shortage (Commodity);
   begin
      return Quantity_Type'Max
        (Quantity_Type'Truncation (Quantity), 1.0);
   end Determine_Purchase_Quantity;

   -----------------------------
   -- Determine_Sale_Quantity --
   -----------------------------

   function Determine_Sale_Quantity
     (Exchange  : not null access constant Agora.Exchange.Any_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Observer  : not null access constant Agora.Observer.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Look_Back : Positive)
      return Quantity_Type
   is
      Public_Mean   : constant Price_Type :=
                        Exchange.Average_Historical_Price
                          (Commodity, Look_Back);
      Price_Range   : constant Price_Model_Type :=
                        Observer.Trading_Range (Commodity, Look_Back);
      Favourability : constant Real :=
                        Position_In_Range
                          (Public_Mean, Price_Range.Low, Price_Range.High);
      Quantity      : constant Quantity_Type :=
                        Favourability * Inventory.Surplus (Commodity);
   begin
      return Quantity_Type'Max
        (Quantity_Type'Truncation (Quantity), 1.0);
   end Determine_Sale_Quantity;

   ---------------
   -- Singleton --
   ---------------

   function Singleton return Reference is
   begin
      return Rec'Access;
   end Singleton;

   ------------------------
   -- Update_Price_Model --
   ------------------------

   overriding procedure Update_Price_Model
     (This       : Instance;
      Trader     : not null access constant Agora.Trader.Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Inventory  : not null access constant Agora.Inventory.Any_Instance;
      Action     : Action_Type;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Success    : Boolean;
      Unit_Price : Price_Type;
      Model      : in out Price_Model_Type)
   is
      Model_Low     : Price_Delta_Type := Model.Low;
      Model_High    : Price_Delta_Type := Model.High;
      Model_Mean    : constant Price_Type := (Model_Low + Model_High) / 2.0;
      Public_Mean   : constant Price_Type :=
                        Exchange.Average_Historical_Price (Commodity, 1);
      Delta_To_Mean : constant Price_Delta_Type :=
                        abs (Model_Mean - Public_Mean);
      Wobble        : Price_Delta_Type := 0.05;
   begin
      if Success then
         case Action is
            when Bid =>
               if Model_Mean > Public_Mean + Significant_Price_Delta then
                  --  overpaid
                  Model_Low := Model_Low - Delta_To_Mean / 2.0;
                  Model_High := Model_High - Delta_To_Mean / 2.0;
               end if;
            when Ask =>
               if Public_Mean > Model_Mean + Significant_Price_Delta then
                  --  priced too low
                  Model_Low := Model_Low + Delta_To_Mean / 2.0;
                  Model_High := Model_High + Delta_To_Mean / 2.0;
               end if;
         end case;

         --  increase certainty of belief
         Model_Low := Model_Low + Wobble * Model_Mean;
         Model_High := Model_High - Wobble * Model_Mean;

      else

         --  shift towards the mean
         if Model_Mean > Public_Mean then
            Model_High := Model_High - Delta_To_Mean / 2.0;
            Model_Low  := Model_Low - Delta_To_Mean / 2.0;
         else
            Model_High := Model_High + Delta_To_Mean / 2.0;
            Model_Low  := Model_Low + Delta_To_Mean / 2.0;
         end if;

         declare
            Special_Case : Boolean := False;
            Have         : constant Quantity_Type :=
                             Inventory.Quantity (Commodity);
            Ideal        : constant Quantity_Type :=
                             Inventory.Ideal (Commodity);
         begin
            case Action is
               when Bid =>
                  if Have < Low_Inventory * Ideal then
                     Wobble := Wobble * 2.0;
                     Special_Case := True;
                  end if;
               when Ask =>
                  if Have > High_Inventory * Ideal then
                     Wobble := Wobble * 2.0;
                     Special_Case := True;
                  end if;
            end case;

            if not Special_Case then
               declare
                  Asks          : constant Quantity_Type :=
                                    Exchange.Average_Historical_Asks
                                      (Commodity, 1);
                  Bids          : constant Quantity_Type :=
                                    Exchange.Average_Historical_Bids
                                      (Commodity, 1);
                  Supply_Demand : constant Real :=
                                    Real ((Asks - Bids) / (Asks + Bids));
               begin
                  if abs Supply_Demand > Significant_Imbalance then
                     declare
                        New_Mean : constant Price_Type :=
                                     (1.0 - Supply_Demand) * Public_Mean;
                        Mean_Delta : constant Price_Delta_Type :=
                                       Model_Mean - New_Mean;
                     begin
                        Model_Low := Model_Low - Mean_Delta / 2.0;
                        Model_High := Model_High - Mean_Delta / 2.0;
                     end;
                  end if;
               end;
            end if;
         end;

         --  Increase uncertainty

         Model_Low := Model_Low - Wobble * Model_Mean;
         Model_High := Model_High + Wobble * Model_Mean;
      end if;

      Model.Low := Price_Delta_Type'Max (Model_Low, Minimum_Price);
      Model.High := Price_Delta_Type'Max (Model_High, Minimum_Price);

   end Update_Price_Model;

end Agora.Dnp.Behaviour;
