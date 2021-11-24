with WL.Random;

with Agora.Images;
with Agora.Logging;

package body Agora.Market_Hardcoded is

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

   procedure Shuffle (List : in out Offer_Lists.List);

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

   ------------------------------
   -- Average_Historical_Price --
   ------------------------------

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
            Total := Total + Item.Total / Item.Traded;
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
      Offers       : Commodity_Market renames This.Offers (Commodity.Tag);
      Asks         : Offer_Lists.List := Offers (Ask);
      Bids         : Offer_Lists.List := Offers (Bid);
      Traded       : Quantity_Type := 0.0;
      Total        : Money_Type    := 0.0;
      Askers       : constant Natural := Natural (Asks.Length);
      Bidders      : constant Natural := Natural (Bids.Length);
      Ask_Quantity : Quantity_Type := 0.0;
      Ask_Value    : Money_Type := 0.0;
      Bid_Quantity : Quantity_Type := 0.0;
      Bid_Value    : Money_Type := 0.0;

      procedure Resolve_At_Ask_Price;

      procedure Resolve_At_Ask_And_Bid_Price;

      ----------------------------------
      -- Resolve_At_Ask_And_Bid_Price --
      ----------------------------------

      procedure Resolve_At_Ask_And_Bid_Price is
      begin
         Ask_Offer_Sorting.Sort (Asks);
         Bid_Offer_Sorting.Sort (Bids);

         while not Asks.Is_Empty
           and then not Bids.Is_Empty
           and then Asks.First_Element.Offer.Price
             <= Bids.First_Element.Offer.Price
         loop
            declare
               Ask      : Offer_Record renames Asks (Asks.First);
               Bid      : Offer_Record renames Bids (Bids.First);
               Price    : constant Price_Type :=
                            (Ask.Offer.Price + Bid.Offer.Price) / 2.0;
               Quantity : constant Quantity_Type :=
                            Quantity_Type'Min (Ask.Offer.Quantity,
                                               Bid.Offer.Quantity);
            begin
               if Quantity > 0.0 then
                  Ask.Offer.Quantity := Ask.Offer.Quantity - Quantity;
                  Bid.Offer.Quantity := Bid.Offer.Quantity - Quantity;

                  Agora.Logging.Log
                    (Ask.Agent.Log_Id
                     & " sells "
                     & Agora.Images.Image (Quantity)
                     & " "
                     & Commodity.Tag
                     & " to "
                     & Bid.Agent.Log_Id
                     & " for "
                     & Agora.Images.Image (Price)
                     & " each; total "
                     & Agora.Images.Image (Quantity * Price));

                  Agora.Agent.Transfer
                    (From      => Ask.Agent,
                     To        => Bid.Agent,
                     Commodity => Commodity,
                     Quantity  => Quantity,
                     Cost      => Quantity * Price);

                  Ask.Agent.Update_Price_Model
                    (Exchange   => This,
                     Action     => Agora.Ask,
                     Commodity  => Commodity,
                     Success    => True,
                     Unit_Price => Price);

                  Bid.Agent.Update_Price_Model
                    (Exchange   => This,
                     Action     => Agora.Bid,
                     Commodity  => Commodity,
                     Success    => True,
                     Unit_Price => Price);

                  Traded := Traded + Quantity;
                  Total  := Total + Quantity * Price;
               end if;
            end;

            if Asks.First_Element.Offer.Quantity = 0.0 then
               Asks.Delete_First;
            end if;

            if Bids.First_Element.Offer.Quantity = 0.0 then
               Bids.Delete_First;
            end if;

         end loop;

         while not Bids.Is_Empty loop
            Bids.First_Element.Agent.Update_Price_Model
              (Exchange   => This,
               Action     => Agora.Bid,
               Commodity  => Commodity,
               Success    => False,
               Unit_Price => Bids.First_Element.Offer.Price);
            Bids.Delete_First;
         end loop;

         while not Asks.Is_Empty loop
            Asks.First_Element.Agent.Update_Price_Model
              (Exchange   => This,
               Action     => Agora.Ask,
               Commodity  => Commodity,
               Success    => False,
               Unit_Price => Asks.First_Element.Offer.Price);
            Asks.Delete_First;
         end loop;

      end Resolve_At_Ask_And_Bid_Price;

      --------------------------
      -- Resolve_At_Ask_Price --
      --------------------------

      procedure Resolve_At_Ask_Price is

         function Position_In_Range
           (Value, Low, High : Price_Type)
            return Real
         is (if Value <= Low
             then 0.0
             elsif Value >= High
             then 1.0
             else Real (Value - Low) / Real (High - Low));
      begin
         Shuffle (Asks);
         Shuffle (Bids);

         Ask_Offer_Sorting.Sort (Asks);

         while not Asks.Is_Empty
           and then not Bids.Is_Empty
         loop
            declare
               Price : constant Price_Type := Asks.First_Element.Offer.Price;
               List  : Offer_Lists.List;
               Ask_Quantity : Quantity_Type := 0.0;
               Remaining_Ask : Quantity_Type;
            begin
               while not Asks.Is_Empty
                 and then Asks.First_Element.Offer.Price = Price
               loop
                  List.Append (Asks.First_Element);
                  Ask_Quantity := Ask_Quantity
                    + Asks.First_Element.Offer.Quantity;
                  Asks.Delete_First;
               end loop;

               Remaining_Ask := Ask_Quantity;

               while Remaining_Ask > 0.0
                 and then not Bids.Is_Empty
               loop

                  declare
                     Bid      : Offer_Record renames Bids (Bids.First);
                     Pos      : constant Real :=
                                  Position_In_Range
                                    (Price, Bid.Price, Bid.Limit_Price);
                     Buy      : constant Quantity_Type :=
                                  Bid.Offer.Quantity
                                    * Quantity_Type (1.0 - Pos);
                     Quantity : constant Quantity_Type :=
                                  Quantity_Type'Min
                                    (Remaining_Ask,
                                     Quantity_Type'Min (Buy, Bid.Remaining));
                  begin
                     if Quantity > 0.0 then
                        Remaining_Ask := Remaining_Ask - Quantity;
                        Bid.Remaining := Bid.Remaining - Quantity;

                        Agora.Logging.Log
                          (Ask.Agent.Log_Id
                           & " sells "
                           & Agora.Images.Image (Quantity)
                           & " "
                              & Commodity.Tag
                           & " to "
                           & Bid.Agent.Log_Id
                           & " for "
                           & Agora.Images.Image (Price)
                           & " each; total "
                           & Agora.Images.Image (Quantity * Price));

                        Agora.Agent.Transfer
                          (From      => Ask.Agent,
                           To        => Bid.Agent,
                           Commodity => Commodity,
                           Quantity  => Quantity,
                           Cost      => Quantity * Price);

                        Traded := Traded + Quantity;
                        Total  := Total + Quantity * Price;
                     end if;
                  end;

                  if Bids.First_Element.Remaining = 0.0 then
                     Bids.Delete_First;
                     exit when Bids.Is_Empty;
                  end if;
               end loop;

               declare
                  New_List : Offer_Lists.List;
               begin
                  for Ask of List loop
                     if Ask.Offer.Quantity > 0.0 then
                        New_List.Append (Ask);
                     end if;
                     end loop;
                     Shuffle (New_List);
                     List := New_List;
                  end;
               end loop;
            end;
         end loop;
      end Resolve_At_Ask_Price;

   begin

      for Ask of Asks loop
         Ask_Quantity := Ask_Quantity + Ask.Offer.Quantity;
         Ask_Value := Ask_Value + Ask.Offer.Quantity * Ask.Offer.Price;
      end loop;

      for Bid of Bids loop
         Bid_Quantity := Bid_Quantity + Bid.Offer.Quantity;
         Bid_Value := Bid_Value + Bid.Offer.Quantity * Bid.Offer.Price;
      end loop;

      if True then
         Resolve_At_Ask_Price;
      else
         Resolve_At_Ask_And_Bid_Price;
      end if;

      for List of Offers loop
         List.Clear;
      end loop;

      if not This.History.Contains (Commodity.Tag) then
         This.History.Insert
           (Commodity.Tag, History_Lists.Empty_List);
      end if;

      declare
         List : History_Lists.List renames
                  This.History (Commodity.Tag);
      begin
         List.Append
           (History_Record'
              (Askers  => Askers,
               Bidders => Bidders,
               Asks    => Ask_Quantity,
               Bids    => Bid_Quantity,
               Traded  => Traded,
               Total   => Total));
      end;

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

end Agora.Market_Harcoded;
