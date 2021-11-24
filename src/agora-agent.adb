with Agora.Images;
with Agora.Logging;

package body Agora.Agent is

   function Initial_Price_Belief
     (Exchange  : not null access Agora.Exchange.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Model_Type;

   ------------
   -- Create --
   ------------

   function Create
     (Id         : Agent_Id;
      Behaviour  : not null access constant Agora.Behaviour.Any_Instance;
      Inventory  : not null access Agora.Inventory.Any_Instance;
      Profession : not null access constant Agora.Profession.Any_Instance)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Id          => Id,
         Last_Cash   => Inventory.Cash,
         Bid_Space   => 0.0,
         Behaviour   => Agora.Behaviour.Reference (Behaviour),
         Inventory   => Agora.Inventory.Reference (Inventory),
         Observer    => new Agora.Observer.Instance,
         Profession  => Agora.Profession.Reference (Profession),
         Confidence  => 0.0,
         Price_Model => <>,
         Last_Offers => <>)
      do
         Profession.Set_Ideal_Quantities (This.Inventory);
         for Commodity of Agora.Commodity.All_Commodities loop
            This.Observer.Observe_Trade (Commodity, 0.5, 1.0);
            This.Observer.Observe_Trade (Commodity, 1.5, 1.0);
         end loop;
      end return;

   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (This : in out Reference) is
   begin
      null;
   end Destroy;

   ------------------------
   -- Execute_Profession --
   ------------------------

   procedure Execute_Profession (This : in out Any_Instance) is
   begin
      This.Log ("start production: " & This.Inventory.Image);
      This.Profession.Execute
        (Inventory => This.Inventory);
      This.Log ("end production: " & This.Inventory.Image);
   end Execute_Profession;

   ---------------------
   -- Generate_Offers --
   ---------------------

   procedure Generate_Offers
     (This      : not null access Any_Instance;
      Exchange  : not null access Agora.Exchange.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Create    : not null access
        procedure (Action : Action_Type;
                   Offer  : Agora.Offer.Offer_Type))
   is
      Surplus : constant Quantity_Type := This.Inventory.Surplus (Commodity);
      Shortage : constant Quantity_Type := This.Inventory.Shortage (Commodity);
      Space    : constant Quantity_Type :=
                   This.Inventory.Available_Space - This.Bid_Space;
      Rec      : Offer_Record;
   begin
      if not This.Price_Model.Contains (Commodity.Tag) then
         This.Price_Model.Insert
           (Commodity.Tag, Initial_Price_Belief (Exchange, Commodity));
      end if;

      if Surplus > 0.0 then
         declare
            Offer : constant Agora.Offer.Offer_Type :=
                      This.Behaviour.Create_Ask
                        (Trader     => This,
                         Exchange   => Exchange,
                         Inventory  => This.Inventory,
                         Observer   => This.Observer,
                         Commodity  => Commodity,
                         Confidence => This.Confidence,
                         Model      => This.Price_Model (Commodity.Tag),
                         Limit      => Surplus);
         begin
            if Offer.Quantity > 0.0 then
               Rec.Quantity := Offer.Quantity;
               Rec.Price := Offer.Price;
               Rec.Limit := Offer.Limit_Price;
               Create (Ask, Offer);
            end if;
         end;
      elsif Shortage > 0.0 and then Space >= Commodity.Size then
         declare
            Limit : Quantity_Type;
         begin
            if Shortage * Commodity.Size <= Space then
               Limit := Shortage;
            else
               Limit :=
                 Quantity_Type'Truncation (Space / Shortage / Commodity.Size);
            end if;
            if Limit > 0.0 then
               declare
                  Offer : constant Agora.Offer.Offer_Type :=
                            This.Behaviour.Create_Bid
                              (Trader     => This,
                               Exchange   => Exchange,
                               Inventory  => This.Inventory,
                               Observer   => This.Observer,
                               Commodity  => Commodity,
                               Confidence => This.Confidence,
                               Model      => This.Price_Model (Commodity.Tag),
                               Limit      => Limit);
               begin
                  if Offer.Quantity > 0.0 then
                     Rec.Quantity := Offer.Quantity;
                     Rec.Price := Offer.Price;
                     Rec.Limit := Offer.Limit_Price;
                     Create (Bid, Offer);
                     This.Bid_Space :=
                       This.Bid_Space + Offer.Quantity * Commodity.Size;
                  end if;
               end;
            end if;
         end;
      end if;

      if This.Last_Offers.Contains (Commodity.Tag) then
         This.Last_Offers.Delete (Commodity.Tag);
      end if;

      if Rec.Quantity > 0.0 then
         This.Last_Offers.Insert (Commodity.Tag, Rec);
      end if;

   end Generate_Offers;

   --------------------
   -- Get_Last_Offer --
   --------------------

   overriding procedure Get_Last_Offer
     (This            : Instance;
      Commodity       : not null access constant Agora.Commodity.Any_Instance;
      Quantity        : out Quantity_Type;
      Price           : out Price_Type;
      Limit           : out Price_Type;
      Closed_Value    : out Money_Type;
      Closed_Quantity : out Quantity_Type)
   is
   begin
      if not This.Last_Offers.Contains (Commodity.Tag) then
         Quantity := 0.0;
         Price := 1.0;
         Limit := 1.0;
         Closed_Value := 0.0;
         Closed_Quantity := 0.0;
      else
         declare
            Last : Offer_Record renames This.Last_Offers (Commodity.Tag);
         begin
            Quantity := Last.Quantity;
            Price := Last.Price;
            Limit := Last.Limit;
            Closed_Value := Last.Closed_Value;
            Closed_Quantity := Last.Closed_Quantity;
         end;
      end if;
   end Get_Last_Offer;

   --------------------------
   -- Initial_Price_Belief --
   --------------------------

   function Initial_Price_Belief
     (Exchange  : not null access Agora.Exchange.Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Model_Type
   is
      Average : constant Price_Type :=
                  Exchange.Average_Historical_Price (Commodity, 10);
   begin
      return Price_Model_Type'
        (Low  => Average * 0.5,
         High => Average * 1.5);
   end Initial_Price_Belief;

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : Any_Instance;
      Message : String)
   is
   begin
      Agora.Logging.Log
        (Category => This.Log_Id,
         Message  => Message);
   end Log;

   ---------------
   -- On_Bought --
   ---------------

   overriding procedure On_Bought
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type)
   is
      Value : constant Money_Type := Quantity * Price;
      Rec   : Offer_Record renames This.Last_Offers (Commodity.Tag);
   begin
      This.Log ("buys " & Agora.Images.Image (Quantity)
                & " " & Commodity.Tag
                & " @ " & Agora.Images.Image (Price)
                & "; total " & Agora.Images.Image (Value));

      This.Inventory.Spend (Value);
      This.Inventory.Add (Commodity, Round (Quantity), Value);
      Rec.Closed_Quantity := Rec.Closed_Quantity + Quantity;
      Rec.Closed_Value := Rec.Closed_Value + Value;
   end On_Bought;

   overriding procedure On_Failed_Offer
     (This       : in out Instance;
      Offer      : Agora.Offer.Offer_Type;
      Remaining  : Quantity_Type)
   is null;

   -------------
   -- On_Sold --
   -------------

   overriding procedure On_Sold
     (This       : in out Instance;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Quantity   : Quantity_Type;
      Price      : Price_Type)
   is
      Value : constant Money_Type := Quantity * Price;
      Rec   : Offer_Record renames This.Last_Offers (Commodity.Tag);
   begin
      This.Log ("sells " & Agora.Images.Image (Quantity)
                & " " & Commodity.Tag
                & " @ " & Agora.Images.Image (Price)
                & "; total " & Agora.Images.Image (Value));
      This.Inventory.Earn (Value);
      This.Inventory.Remove (Commodity, Round (Quantity));
      Rec.Closed_Quantity := Rec.Closed_Quantity + Quantity;
      Rec.Closed_Value := Rec.Closed_Value + Value;
   end On_Sold;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Agent      : in out Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance)
   is
      New_Profession : constant Agora.Profession.Reference :=
                         Agent.Behaviour.Choose_Profession
                           (Exchange);
   begin
      Agent.Log ("reset");
      Agent.Profession := New_Profession;
      Agent.Inventory.Clear;
      Agent.Profession.Set_Ideal_Quantities (Agent.Inventory);
      Agent.Inventory.Reset_Cash (100.0);
      Agent.Log ("resume");
   end Reset;

   -----------------
   -- Start_Round --
   -----------------

   procedure Start_Round (This : in out Any_Instance) is
   begin
      This.Log ("starting round: cash = "
                & Agora.Images.Image (This.Current_Cash)
                & "; previous cash = "
                & Agora.Images.Image (This.Last_Cash)
                & "; profit = "
                & Agora.Images.Image (This.Current_Cash - This.Last_Cash));

      This.Last_Cash := This.Current_Cash;
      This.Bid_Space := 0.0;
      This.Confidence := Real'Min (This.Confidence + 1.0 / 16.0, 1.0);
   end Start_Round;

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (From, To  : Reference;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type; Cost : Money_Type)
   is
      Value : Money_Type;
   begin
      From.Inventory.Remove (Commodity, Quantity, Value);
      To.Inventory.Add (Commodity, Quantity, Cost);
      To.Inventory.Spend (Cost);
      From.Inventory.Earn (Cost);
   end Transfer;

   ------------------------
   -- Update_Price_Model --
   ------------------------

   procedure Update_Price_Model
     (This       : not null access Any_Instance;
      Exchange   : not null access constant Agora.Exchange.Any_Instance;
      Action     : Action_Type;
      Commodity  : not null access constant Agora.Commodity.Any_Instance;
      Success    : Boolean;
      Unit_Price : Price_Type)
   is
      Model : Price_Model_Type renames This.Price_Model (Commodity.Tag);
   begin
      This.Log ("updating price model: "
                & Commodity.Tag
                & ": "
                & Action'Image
                & ": "
                & (if Success
                  then "success at " & Agora.Images.Image (Unit_Price)
                  else "failure")
                & ": current price model "
                & Agora.Images.Image (Model.Low)
                & "/"
                & Agora.Images.Image ((Model.Low + Model.High) / 2.0)
                & "/"
                & Agora.Images.Image (Model.High));

      This.Behaviour.Update_Price_Model
        (Trader     => This,
         Exchange   => Exchange,
         Inventory  => This.Inventory,
         Action     => Action,
         Commodity  => Commodity,
         Success    => Success,
         Unit_Price => Unit_Price,
         Model      => Model);

      This.Log ("new price model: "
                & Agora.Images.Image (Model.Low)
                & "/"
                & Agora.Images.Image ((Model.Low + Model.High) / 2.0)
                & "/"
                & Agora.Images.Image (Model.High));
   end Update_Price_Model;

end Agora.Agent;
