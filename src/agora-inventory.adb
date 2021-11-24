with Agora.Images;
with Agora.Logging;

package body Agora.Inventory is

   Unit_Inventory_Singleton : aliased constant Instance :=
                                Instance'
                                  (Is_Constant    => True,
                                   Constant_Value => 1.0,
                                   Cash           => 0.0,
                                   Costs          => 0.0,
                                   Space          => 0.0,
                                   Map            => <>);

   Zero_Inventory_Singleton : aliased constant Instance :=
                                Instance'
                                  (Is_Constant    => True,
                                   Constant_Value => 0.0,
                                   Cash           => 0.0,
                                   Costs          => 0.0,
                                   Space          => 0.0,
                                   Map            => <>);

   function Unit_Inventory return Constant_Reference
   is (Unit_Inventory_Singleton'Access);

   function Zero_Inventory return Constant_Reference
   is (Zero_Inventory_Singleton'Access);

   function Get
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Record;

   ---------
   -- Add --
   ---------

   procedure Add
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : Money_Type)
   is
   begin
      if This.Map.Contains (Commodity.Tag) then
         declare
            Rec : Commodity_Record renames This.Map (Commodity.Tag);
         begin
            Rec.Quantity := Rec.Quantity + Quantity;
            Rec.Value := Rec.Value + Value;
         end;
      else
         This.Map.Insert
           (Commodity.Tag,
            Commodity_Record'
              (Commodity => Agora.Commodity.Reference (Commodity),
               Quantity  => Quantity,
               Ideal     => 0.0,
               Value     => Value));
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type;
      Value     : Money_Type)
   is
   begin
      This.Add (Agora.Commodity.Get (Commodity), Quantity, Value);
   end Add;

   ---------------------
   -- Available_Space --
   ---------------------

   function Available_Space (This : Any_Instance) return Quantity_Type is
   begin
      return This.Max_Quantity - This.Used_Space;
   end Available_Space;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Any_Instance) is
   begin
      This.Map.Clear;
      This.Clear_Recorded_Costs;
   end Clear;

   --------------------------
   -- Clear_Recorded_Costs --
   --------------------------

   procedure Clear_Recorded_Costs
     (This : in out Any_Instance)
   is
   begin
      This.Costs := 0.0;
   end Clear_Recorded_Costs;

   ----------------------
   -- Create_Inventory --
   ----------------------

   function Create_Inventory
     (Config     : Tropos.Configuration;
      Start_Cash : Money_Type)
      return Reference
   is
      Start : constant Tropos.Configuration := Config.Child ("start");
      Ideal : constant Tropos.Configuration := Config.Child ("ideal");
   begin
      return This : constant Reference := new Instance'
        (Is_Constant    => False,
         Constant_Value => 0.0,
         Space          => Quantity_Type (Float'(Config.Get ("max_size"))),
         Map            => <>,
         Cash           => Start_Cash,
         Costs          => 0.0)
      do
         for Commodity of Agora.Commodity.All_Commodities loop
            declare
               Start_Quantity : constant Float :=
                                  Start.Get (Commodity.Tag, 0.0);
               Ideal_Quantity : constant Float :=
                                  Ideal.Get (Commodity.Tag, 0.0);
            begin
               if Start_Quantity > 0.0
                 or else Ideal_Quantity > 0.0
               then
                  This.Map.Insert
                    (Commodity.Tag,
                     Commodity_Record'
                       (Commodity => Commodity,
                        Quantity  => Quantity_Type (Start_Quantity),
                        Ideal     => Quantity_Type (Ideal_Quantity),
                        Value     =>
                          Quantity_Type (Start_Quantity)
                        * Commodity.Initial_Price / 2.0));
               end if;
            end;
         end loop;
      end return;
   end Create_Inventory;

   ----------
   -- Earn --
   ----------

   procedure Earn (This   : in out Any_Instance;
                   Amount : Money_Type)
   is
   begin
      This.Cash := This.Cash + Amount;
   end Earn;

   ---------
   -- Get --
   ---------

   function Get
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Record
   is
   begin
      if This.Is_Constant then
         return Commodity_Record'
           (Commodity => Agora.Commodity.Reference (Commodity),
            Quantity  => This.Constant_Value,
            Ideal     => This.Constant_Value,
            Value     => This.Constant_Value * Commodity.Initial_Price);
      elsif This.Map.Contains (Commodity.Tag) then
         return This.Map (Commodity.Tag);
      else
         return Commodity_Record'
           (Commodity => Agora.Commodity.Reference (Commodity),
            Quantity  => 0.0,
            Ideal     => 0.0,
            Value     => 0.0);
      end if;
   end Get;

   -----------
   -- Ideal --
   -----------

   function Ideal
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
   begin
      return This.Get (Commodity).Ideal;
   end Ideal;

   -----------
   -- Image --
   -----------

   function Image
     (This : Any_Instance)
      return String
   is
      use Commodity_Maps;

      function Stock_Image
        (Current : Cursor;
         First   : Boolean)
         return String;

      -----------------
      -- Stock_Image --
      -----------------

      function Stock_Image
        (Current : Cursor;
         First   : Boolean)
         return String
      is
      begin
         if not Has_Element (Current) then
            return "";
         elsif This.Map (Current).Quantity > 0.0 then
            declare
               Img : constant String :=
                       This.Map (Current).Commodity.Tag
                     & ":"
                       & Agora.Images.Image (This.Map (Current).Quantity)
                       & "/"
                       & Agora.Images.Image (This.Map (Current).Value);
            begin
               return (if First then "" else ";")
                 & Img & Stock_Image (Next (Current), False);
            end;
         else
            return Stock_Image (Next (Current), First);
         end if;
      end Stock_Image;

   begin
      return "cash:" & Agora.Images.Image (This.Cash)
        & "; investment "
        & Agora.Images.Image (This.Costs)
        & "; space "
        & Agora.Images.Image (This.Space)
        & "/"
        & Agora.Images.Image (This.Used_Space)
        & "/"
        & Agora.Images.Image (This.Available_Space)
        & (if This.Is_Empty then ""
           else "; stock " & Stock_Image (This.Map.First, True));
   end Image;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Any_Instance) return Boolean is
   begin
      return This.Used_Space = 0.0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (This : Any_Instance) return Boolean is
   begin
      return This.Available_Space = 0.0;
   end Is_Full;

   -------------------
   -- Make_Room_For --
   -------------------

   procedure Make_Room_For
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
   is
      Required_Space : constant Quantity_Type :=
                         Commodity.Size * Quantity;
      Checks         : constant Agora.Commodity.Reference_Array :=
                         Agora.Commodity.All_Commodities_Excluding
                           (Excluding =>
                              (1 => Agora.Commodity.Reference (Commodity)));

   begin
      while This.Available_Space < Required_Space loop
         declare
            use type Agora.Commodity.Reference;
            Cheapest_Item : Agora.Commodity.Reference := null;
            Lowest_Price  : Price_Type := Price_Type'Last;
         begin
            for Check of Checks loop
               if This.Quantity (Check) > 0.0
                 and then This.Price (Check) < Lowest_Price
               then
                  Cheapest_Item := Check;
                  Lowest_Price  := This.Price (Check);
               end if;
            end loop;

            pragma Assert (Cheapest_Item /= null);

            declare
               Value : Money_Type;
               Quantity : constant Quantity_Type :=
                            Quantity_Type'Min
                              (This.Quantity (Cheapest_Item),
                               Required_Space - This.Available_Space);
            begin
               This.Remove
                 (Cheapest_Item, Quantity, Value);
               Agora.Logging.Log
                 ("dumped " & Agora.Images.Image (Quantity)
                  & " " & Cheapest_Item.Tag
                  & " worth " & Agora.Images.Image (Value));
               --  pragma Unreferenced (Value);
            end;
         end;
      end loop;
   end Make_Room_For;

   -------------------
   -- Make_Room_For --
   -------------------

   procedure Make_Room_For
     (This : in out Any_Instance; Commodity : String; Quantity : Quantity_Type)
   is
   begin
      This.Make_Room_For (Agora.Commodity.Get (Commodity), Quantity);
   end Make_Room_For;

   ------------------
   -- Max_Quantity --
   ------------------

   function Max_Quantity (This : Any_Instance) return Quantity_Type is
   begin
      return This.Space;
   end Max_Quantity;

   -----------
   -- Price --
   -----------

   function Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type
   is
      Rec : constant Commodity_Record := This.Get (Commodity);
   begin
      if Rec.Quantity > 0.0 then
         return Price_Delta_Type'Max
           (Rec.Value / Rec.Quantity, Price_Type'First);
      else
         return Price_Type'First;
      end if;
   end Price;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
   begin
      return This.Get (Commodity).Quantity;
   end Quantity;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (This : Any_Instance;
      Tag  : String)
      return Quantity_Type
   is
   begin
      return This.Quantity (Agora.Commodity.Get (Tag));
   end Quantity;

   -----------------
   -- Record_Cost --
   -----------------

   procedure Record_Cost
     (This   : in out Any_Instance;
      Amount : Money_Type)
   is
   begin
      This.Costs := This.Costs + Amount;
   end Record_Cost;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : out Money_Type)
   is
   begin
      if This.Map.Contains (Commodity.Tag) then
         declare
            Rec : Commodity_Record renames This.Map (Commodity.Tag);
            Price : constant Price_Delta_Type :=
                      Rec.Value / Rec.Quantity;
            New_Value : constant Money_Type :=
                          (Rec.Quantity - Quantity) * Price;
         begin
            Value := Rec.Value - New_Value;
            Rec.Value := New_Value;
            Rec.Quantity := Rec.Quantity - Quantity;
         end;
      elsif Quantity > 0.0 then
         raise Constraint_Error with
           "cannot remove "
           & Agora.Images.Image (Quantity)
           & " "
           & Commodity.Tag
           & " from inventory because there are none";
      end if;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type;
      Value     : out Money_Type)
   is
   begin
      This.Remove (Agora.Commodity.Get (Commodity), Quantity, Value);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
   is
      Value : Money_Type with Unreferenced;
   begin
      This.Remove (Commodity, Quantity, Value);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type)
   is
   begin
      This.Remove (Agora.Commodity.Get (Commodity), Quantity);
   end Remove;

   ----------------
   -- Reset_Cash --
   ----------------

   procedure Reset_Cash
     (This : in out Any_Instance;
      Cash : Money_Type)
   is
   begin
      This.Cash := Cash;
   end Reset_Cash;

   ------------------------
   -- Set_Ideal_Quantity --
   ------------------------

   procedure Set_Ideal_Quantity
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
   is
   begin
      if This.Map.Contains (Commodity.Tag) then
         This.Map (Commodity.Tag).Ideal := Quantity;
      else
         This.Map.Insert
           (Commodity.Tag,
            Commodity_Record'
              (Commodity => Agora.Commodity.Reference (Commodity),
               Quantity  => 0.0,
               Ideal     => Quantity,
               Value     => 0.0));
      end if;
   end Set_Ideal_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : Money_Type)
   is
   begin
      if This.Map.Contains (Commodity.Tag) then
         declare
            Rec : Commodity_Record renames This.Map (Commodity.Tag);
         begin
            Rec.Quantity := Quantity;
            Rec.Value := Value;
         end;
      else
         This.Map.Insert
           (Commodity.Tag,
            Commodity_Record'
              (Commodity => Agora.Commodity.Reference (Commodity),
               Quantity  => Quantity,
               Ideal     => 0.0,
               Value     => Value));
      end if;
   end Set_Quantity;

   --------------
   -- Shortage --
   --------------

   function Shortage
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
      Rec : constant Commodity_Record := This.Get (Commodity);
   begin
      if Rec.Quantity < Rec.Ideal then
         return Rec.Ideal - Rec.Quantity;
      else
         return 0.0;
      end if;
   end Shortage;

   -----------
   -- Spend --
   -----------

   procedure Spend (This   : in out Any_Instance;
                    Amount : Money_Type)
   is
   begin
      This.Cash := This.Cash - Amount;
   end Spend;

   -------------
   -- Surplus --
   -------------

   function Surplus
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
      Rec : constant Commodity_Record := This.Get (Commodity);
   begin
      if Rec.Quantity > Rec.Ideal then
         return Rec.Quantity - Rec.Ideal;
      else
         return 0.0;
      end if;
   end Surplus;

   ----------------
   -- Used_Space --
   ----------------

   function Used_Space
     (This      : Any_Instance)
      return Quantity_Type
   is
   begin
      return Quantity : Quantity_Type := 0.0 do
         for Rec of This.Map loop
            Quantity := Quantity + Rec.Quantity * Rec.Commodity.Size;
         end loop;
      end return;
   end Used_Space;

   -----------
   -- Value --
   -----------

   function Value
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Money_Type
   is
   begin
      return This.Get (Commodity).Value;
   end Value;

end Agora.Inventory;
