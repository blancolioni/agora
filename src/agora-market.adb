with WL.Random;

with Agora.Images;
with Agora.Logging;

with Agora.Auction;
with Agora.Configure;

package body Agora.Market is

   procedure Scan_History
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive;
      Process   : not null access
        procedure (Item : History_Record));

   procedure Resolve_Offers
     (This      : not null access Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance);

   procedure Save_Profits
     (This : in out Any_Instance);

   procedure Shuffle (List : in out Offer_Lists.List) with Unreferenced;

   ----------------------------------
   -- Average_Historical_Ask_Price --
   ----------------------------------

   overriding function Average_Historical_Ask_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
   is
      Total    : Money_Type := 0.0;
      Quantity : Quantity_Type := 0.0;

      procedure Add (Item : History_Record);

      ---------
      -- Add --
      ---------

      procedure Add (Item : History_Record) is
      begin
         if Item.Traded > 0.0 then
            Total := Total + Item.Ask_Value;
            Quantity := Quantity + Item.Asks;
         end if;
      end Add;

   begin
      This.Scan_History (Commodity, Rounds, Add'Access);
      if Quantity > 0.0 then
         return Total / Quantity;
      else
         return Commodity.Initial_Price;
      end if;
   end Average_Historical_Ask_Price;

   -----------------------------
   -- Average_Historical_Asks --
   -----------------------------

   overriding function Average_Historical_Asks
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Quantity_Type
   is
      Total : Quantity_Type := 0.0;

      procedure Add (Item : History_Record);

      ---------
      -- Add --
      ---------

      procedure Add (Item : History_Record) is
      begin
         Total := Total + Item.Asks;
      end Add;

   begin
      This.Scan_History (Commodity, Rounds, Add'Access);
      return Total / Quantity_Type (Rounds);
   end Average_Historical_Asks;

   ----------------------------------
   -- Average_Historical_Bid_Price --
   ----------------------------------

   overriding function Average_Historical_Bid_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
   is
      Total    : Money_Type := 0.0;
      Quantity : Quantity_Type := 0.0;

      procedure Add (Item : History_Record);

      ---------
      -- Add --
      ---------

      procedure Add (Item : History_Record) is
      begin
         if Item.Traded > 0.0 then
            Total := Total + Item.Bid_Value;
            Quantity := Quantity + Item.Bids;
         end if;
      end Add;

   begin
      This.Scan_History (Commodity, Rounds, Add'Access);
      if Quantity > 0.0 then
         return Total / Quantity;
      else
         return Commodity.Initial_Price;
      end if;
   end Average_Historical_Bid_Price;

   -----------------------------
   -- Average_Historical_Bids --
   -----------------------------

   overriding function Average_Historical_Bids
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive) return Quantity_Type
   is
      Total : Quantity_Type := 0.0;

      procedure Add (Item : History_Record);

      ---------
      -- Add --
      ---------

      procedure Add (Item : History_Record) is
      begin
         Total := Total + Item.Bids;
      end Add;

   begin
      This.Scan_History (Commodity, Rounds, Add'Access);
      return Total / Quantity_Type (Rounds);
   end Average_Historical_Bids;

   overriding function Average_Historical_Price
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive)
      return Price_Type
   is
      Total : Price_Delta_Type := 0.0;
      Count : Natural := 0;

      procedure Add (Item : History_Record);

      ---------
      -- Add --
      ---------

      procedure Add (Item : History_Record) is
      begin
         if Item.Traded > 0.0 then
            Total := Total + Item.Trade_Value / Item.Traded;
            Count := Count + 1;
         end if;
      end Add;

   begin
      This.Scan_History (Commodity, Rounds, Add'Access);
      if Count > 0 then
         return Total / Price_Type (Count);
      else
         return Commodity.Initial_Price;
      end if;
   end Average_Historical_Price;

   -------------------
   -- Average_Price --
   -------------------

   function Average_Price (List : Offer_Lists.List) return Price_Type is
      Value    : Money_Type := 0.0;
      Quantity : Quantity_Type := 0.0;
   begin
      for Item of List loop
         Quantity := Quantity + Item.Offer.Quantity;
         Value := Value + Item.Offer.Quantity * Item.Offer.Price;
      end loop;

      if Quantity > 0.0 then
         return Value / Quantity;
      else
         return Price_Type'First;
      end if;
   end Average_Price;

   -------------------
   -- Book_Purchase --
   -------------------

   overriding procedure Book_Purchase
     (This      : in out Instance;
      Buyer     : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type)
   is null;

   ---------------
   -- Book_Sale --
   ---------------

   overriding procedure Book_Sale
     (This      : in out Instance;
      Seller    : not null access Agora.Trader.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Price     : Price_Type)
   is
      List : History_Lists.List renames This.History (Commodity.Tag);
      Rec  : History_Record renames List (List.Last);
   begin
      Rec.Traded := Rec.Traded + Quantity;
      Rec.Trade_Value := Rec.Trade_Value + Quantity * Price;
   end Book_Sale;

   ------------------
   -- Create_Offer --
   ------------------

   procedure Create_Offer
     (This   : in out Any_Instance;
      Agent  : not null access Agora.Agent.Any_Instance;
      Action : Action_Type;
      Offer  : Agora.Offer.Offer_Type)
   is
      Rec : constant Offer_Record := Offer_Record'
        (Agent => Agora.Agent.Reference (Agent),
         Offer => Offer);
   begin
      if not This.Offers.Contains (Offer.Commodity.Tag) then
         This.Offers.Insert (Offer.Commodity.Tag,
                             Commodity_Market'
                               (others => <>));
      end if;

      Agent.Log
        (Action'Image
         & " "
         & Agora.Offer.Image (Offer));

      This.Offers (Offer.Commodity.Tag) (Action).Append (Rec);

   end Create_Offer;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : not null access Any_Instance) is
   begin

      This.Save_Profits;

      for Agent of This.Agents loop
         Agent.Start_Round;
         Agent.Execute_Profession;

         for Commodity of
           Agora.Commodity.All_Commodities
         loop
            declare
               procedure Create_Offer
                 (Action : Action_Type;
                  Offer  : Agora.Offer.Offer_Type);

               ------------------
               -- Create_Offer --
               ------------------

               procedure Create_Offer
                 (Action : Action_Type;
                  Offer  : Agora.Offer.Offer_Type)
               is
               begin
                  This.Create_Offer (Agent, Action, Offer);
               end Create_Offer;

            begin
               Agent.Generate_Offers (This, Commodity, Create_Offer'Access);
            end;
         end loop;
      end loop;

      for Commodity of Agora.Commodity.All_Commodities loop
         if This.Offers.Contains (Commodity.Tag) then
            Agora.Logging.Log ("resolving offers: " & Commodity.Tag);
            This.Resolve_Offers (Commodity);
         end if;
      end loop;

      for Agent of This.Agents loop
         if Agent.Current_Cash <= 0.0 then
            Agent.Reset (This);
         end if;
      end loop;

   end Execute;

   -------------------------------
   -- Highest_Historical_Demand --
   -------------------------------

   overriding function Highest_Historical_Demand
     (This      : Instance;
      Minimum   : Real)
      return Agora.Commodity.Reference
   is
      Best_Commodity : Agora.Commodity.Reference := null;
      Best_Ratio     : Real := -1.0;
   begin
      for Commodity of Agora.Commodity.All_Commodities loop
         declare
            Asks : constant Quantity_Type :=
                     Quantity_Type'Max
                       (This.Average_Historical_Asks (Commodity, 10),
                        0.5);
            Bids : constant Quantity_Type :=
                     This.Average_Historical_Bids (Commodity, 10);
            Ratio : constant Real := Real (Bids) / Real (Asks);
         begin
            Agora.Logging.Log
              (Commodity.Tag
               & ": bids " & Agora.Images.Image (Bids)
               & "; asks " & Agora.Images.Image (Asks)
               & "; ratio " & Agora.Images.Image (Ratio));
            if Ratio >= Best_Ratio then
               Best_Ratio := Ratio;
               Best_Commodity := Commodity;
            end if;
         end;
      end loop;

      if Best_Ratio >= Minimum then
         return Best_Commodity;
      else
         return null;
      end if;
   end Highest_Historical_Demand;

   ------------------------------
   -- Highest_Historical_Price --
   ------------------------------

   overriding function Highest_Historical_Price
     (This    : Instance; Rounds : Positive;
      Exclude : Agora.Commodity.Reference_Array := Agora.Commodity.Empty)
      return Agora.Commodity.Reference
   is
      Best_Price : Price_Delta_Type := 0.0;
      Best_Item  : Agora.Commodity.Reference := null;
   begin
      for Commodity of
        Agora.Commodity.All_Commodities_Excluding
          (Excluding => Exclude)
      loop
         declare
            Price : constant Price_Type :=
                      This.Average_Historical_Price (Commodity, Rounds);
         begin
            if Price > Best_Price then
               Best_Price := Price;
               Best_Item := Commodity;
            end if;
         end;
      end loop;
      return Best_Item;
   end Highest_Historical_Price;

   -----------------------------
   -- Lowest_Historical_Price --
   -----------------------------

   overriding function Lowest_Historical_Price
     (This    : Instance;
      Rounds  : Positive;
      Exclude : Agora.Commodity.Reference_Array := Agora.Commodity.Empty)
      return Agora.Commodity.Reference
   is
      Best_Price : Price_Type := Price_Type'Last;
      Best_Item  : Agora.Commodity.Reference := null;
   begin
      for Commodity of
        Agora.Commodity.All_Commodities_Excluding
          (Excluding => Exclude)
      loop
         declare
            Price : constant Price_Type :=
                      This.Average_Historical_Price (Commodity, Rounds);
         begin
            if Price < Best_Price then
               Best_Price := Price;
               Best_Item := Commodity;
            end if;
         end;
      end loop;
      return Best_Item;
   end Lowest_Historical_Price;

   --------------------------------
   -- Most_Profitable_Profession --
   --------------------------------

   overriding function Most_Profitable_Profession
     (This   : Instance;
      Rounds : Positive)
      return Agora.Profession.Reference
   is
      Best_Profession : Agora.Profession.Reference;
      Best_Profit     : Money_Type := Money_Type'First;

      function Average_Profit
        (List : Profession_Lists.List)
         return Money_Type;

      --------------------
      -- Average_Profit --
      --------------------

      function Average_Profit
        (List : Profession_Lists.List)
         return Money_Type
      is
         Count : Natural := 0;
         Total : Money_Type := 0.0;
      begin
         for Element of reverse List loop
            Total := Total + Element.Profit;
            Count := Count + 1;
            exit when Count = Rounds;
         end loop;

         if Count = 0 then
            return 0.0;
         else
            return Total / Money_Type (Count);
         end if;
      end Average_Profit;

   begin
      for Profession of Agora.Profession.All_Professions loop
         if This.Professions.Contains (Profession.Tag) then
            declare
               This_Profit : constant Money_Type :=
                               Average_Profit
                                 (This.Professions (Profession.Tag));
            begin
               Agora.Logging.Log
                 (Profession.Tag & ": average profit "
                  & Agora.Images.Image (This_Profit));
               if This_Profit > Best_Profit then
                  Best_Profit := This_Profit;
                  Best_Profession := Profession;
               end if;
            end;
         end if;
      end loop;

      return Best_Profession;
   end Most_Profitable_Profession;

   ----------
   -- Name --
   ----------

   function Name (This : Any_Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (This.Name);
   end Name;

   --------------------
   -- Resolve_Offers --
   --------------------

   procedure Resolve_Offers
     (This      : not null access Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
   is
      Offers  : Commodity_Market renames This.Offers (Commodity.Tag);
      Asks    : Offer_Lists.List renames Offers (Ask);
      Bids    : Offer_Lists.List renames Offers (Bid);
      Auction : Agora.Auction.Any_Instance :=
                  Agora.Configure.Standard_Auction;
      History : History_Record :=
                  History_Record'
                    (Askers  => Natural (Asks.Length),
                     Bidders => Natural (Bids.Length),
                     Asks    => 0.0,
                     Bids    => 0.0,
                     Traded  => 0.0,
                     Ask_Value => 0.0,
                     Bid_Value => 0.0,
                     Trade_Value => 0.0);

   begin

      for Item of Asks loop
         Auction.Add_Ask (Item.Agent, Item.Offer);
         History.Asks := History.Asks + Item.Offer.Quantity;
         History.Ask_Value := History.Ask_Value
           + Item.Offer.Quantity * Item.Offer.Price;
      end loop;

      for Item of Bids loop
         Auction.Add_Bid (Item.Agent, Item.Offer);
         History.Bids := History.Bids + Item.Offer.Quantity;
         History.Bid_Value := History.Bid_Value
           + Item.Offer.Quantity * Item.Offer.Price;
      end loop;

      This.History (Commodity.Tag).Append (History);

      Auction.Resolve_Auction (This);

      Asks.Clear;
      Bids.Clear;

      if not This.History.Contains (Commodity.Tag) then
         This.History.Insert
           (Commodity.Tag, History_Lists.Empty_List);
      end if;

      --  declare
      --     List : History_Lists.List renames
      --              This.History (Commodity.Tag);
      --  begin
      --     List.Append
      --       (History_Record'
      --          (Askers  => Askers,
      --           Bidders => Bidders,
      --           Asks    => Ask_Quantity,
      --           Bids    => Bid_Quantity,
      --           Traded  => Traded,
      --           Total   => Total));
      --  end;

   end Resolve_Offers;

   ------------------
   -- Save_Profits --
   ------------------

   procedure Save_Profits
     (This : in out Any_Instance)
   is
      type Profit_Record is
         record
            Change : Money_Type := 0.0;
            Count  : Natural    := 0;
         end record;

      package Profit_Maps is new WL.String_Maps (Profit_Record);
      Profit_Map : Profit_Maps.Map;
   begin
      for Profession of Agora.Profession.All_Professions loop
         Profit_Map.Insert (Profession.Tag, (others => <>));
      end loop;

      for Agent of This.Agents loop
         declare
            This_Record : Profit_Record renames
                            Profit_Map (Agent.Profession.Tag);
            This_Profit : constant Money_Type :=
                            Agent.Current_Cash - Agent.Last_Cash;
         begin
            This_Record.Change := This_Record.Change + This_Profit;
            This_Record.Count := This_Record.Count + 1;
         end;
      end loop;

      for Profession of Agora.Profession.All_Professions loop
         declare
            This_Record : Profit_Record renames
                            Profit_Map (Profession.Tag);
            This_Profit : constant Money_Type :=
                            (if This_Record.Count = 0
                             then 0.0
                             else This_Record.Change
                             / Money_Type (This_Record.Count));
         begin
            if not This.Professions.Contains (Profession.Tag) then
               This.Professions.Insert
                 (Profession.Tag,
                  Profession_Lists.Empty_List);
            end if;

            Agora.Logging.Log
              (Category => Profession.Tag,
               Message  =>
                 "agents:" & This_Record.Count'Image
               & "; profit total: "
               & Agora.Images.Image (This_Record.Change)
               & "; per agent: "
               & Agora.Images.Image (This_Profit));

            This.Professions (Profession.Tag).Append
              ((Profit => This_Profit));
         end;
      end loop;
   end Save_Profits;

   ------------------
   -- Scan_History --
   ------------------

   procedure Scan_History
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Rounds    : Positive;
      Process   : not null access
        procedure (Item : History_Record))
   is
      Count : Natural := 0;
   begin
      if This.History.Contains (Commodity.Tag) then
         for History of reverse This.History (Commodity.Tag) loop
            Count := Count + 1;
            Process (History);
            exit when Count = Rounds;
         end loop;
      end if;
   end Scan_History;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle (List : in out Offer_Lists.List) is
      Length : constant Natural := Natural (List.Length);
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

   -----------
   -- Total --
   -----------

   function Total (List : Offer_Lists.List) return Quantity_Type is
   begin
      return Quantity : Quantity_Type := 0.0 do
         for Item of List loop
            Quantity := Quantity + Item.Offer.Quantity;
         end loop;
      end return;
   end Total;

   -----------
   -- Value --
   -----------

   function Value (List : Offer_Lists.List) return Money_Type is
   begin
      return Value : Money_Type := 0.0 do
         for Item of List loop
            Value := Value + Item.Offer.Quantity * Item.Offer.Price;
         end loop;
      end return;
   end Value;

end Agora.Market;
