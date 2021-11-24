with Agora.Inventory;
with Agora.Random;

package body Agora.Dnp.Profession is

   Profession_Config : Tropos.Configuration;

   function Added (Base : Quantity_Type;
                   Space : Quantity_Type)
                   return Quantity_Type
   is (Quantity_Type'Truncation (Quantity_Type'Min (Base, Space)));

   type Blacksmith_Instance is new Instance with null record;

   overriding function Tag
     (This : Blacksmith_Instance)
      return String
   is ("blacksmith");

   overriding function Production
     (This : Blacksmith_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("tools"));

   overriding procedure Execute
     (This      : Blacksmith_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Blacksmith_Singleton : aliased constant Blacksmith_Instance :=
                            (null record);

   type Farmer_Instance is new Instance with null record;

   overriding function Tag
     (This : Farmer_Instance)
      return String
   is ("farmer");

   overriding function Production
     (This : Farmer_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("food"));

   overriding procedure Execute
     (This      : Farmer_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Farmer_Singleton : aliased constant Farmer_Instance :=
                        (null record);

   type Lumberjack_Instance is new Instance with null record;

   overriding function Tag
     (This : Lumberjack_Instance)
      return String
   is ("woodcutter");

   overriding function Production
     (This : Lumberjack_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("wood"));

   overriding procedure Execute
     (This      : Lumberjack_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Lumberjack_Singleton : aliased constant Lumberjack_Instance :=
                        (null record);

   type Miner_Instance is new Instance with null record;

   overriding function Tag
     (This : Miner_Instance)
      return String
   is ("miner");

   overriding function Production
     (This : Miner_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("ore"));

   overriding procedure Execute
     (This      : Miner_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Miner_Singleton : aliased constant Miner_Instance :=
                        (null record);

   type Refiner_Instance is new Instance with null record;

   overriding function Tag
     (This : Refiner_Instance)
      return String
   is ("refiner");

   overriding function Production
     (This : Refiner_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("metal"));

   overriding procedure Execute
     (This      : Refiner_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Refiner_Singleton : aliased constant Refiner_Instance :=
                        (null record);

   type Worker_Instance is new Instance with null record;

   overriding function Tag
     (This : Worker_Instance)
      return String
   is ("worker");

   overriding function Production
     (This : Worker_Instance)
      return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("work"));

   overriding procedure Execute
     (This      : Worker_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   Worker_Singleton : aliased constant Worker_Instance :=
                         (null record);

   ----------------
   -- Blacksmith --
   ----------------

   function Blacksmith return Agora.Profession.Reference is
   begin
      return Blacksmith_Singleton'Access;
   end Blacksmith;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Blacksmith_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Food : constant Quantity_Type :=
               Inventory.Quantity ("food");
      Metal : constant Quantity_Type :=
                Inventory.Quantity ("metal");
      Has_Food : constant Boolean := Food >= 1.0;
      Has_Metal : constant Boolean := Metal >= 1.0;
   begin
      if Has_Food and then Has_Metal then
         --  convert all metal into tools
         declare
            Metal_Value : Money_Type;
            Food_Value  : Money_Type;
         begin
            Inventory.Remove ("metal", Metal, Metal_Value);
            Inventory.Remove ("food", 1.0, Food_Value);
            Inventory.Add ("tools", Metal, Metal_Value + Food_Value);
         end;
      else
         Inventory.Spend (2.0);
         if Inventory.Is_Full then
            Inventory.Make_Room_For ("food", 2.0);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Farmer_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Wood      : constant Quantity_Type :=
                    Inventory.Quantity ("wood");
      Tools     : constant Quantity_Type :=
                    Inventory.Quantity ("tools");
      Has_Wood  : constant Boolean := Wood >= 1.0;
      Has_Tools : constant Boolean := Tools >= 1.0;
      Space     : constant Quantity_Type := Inventory.Available_Space;
   begin
      if Has_Wood and then Has_Tools then
         --  produce 4 food, consume 1 wood, break tools with 10% chance

         declare
            Wood_Value  : Money_Type;
            Tools_Value : Money_Type := 0.0;
         begin
            Inventory.Remove ("wood", 1.0, Wood_Value);
            if Agora.Random.Unit_Random <= 0.1 then
               Inventory.Remove ("tools", 1.0, Tools_Value);
            end if;
            Inventory.Add ("food", Added (4.0, Space),
                           Wood_Value + Tools_Value);
         end;
      elsif Has_Wood then
         declare
            Wood_Value : Money_Type;
         begin
            Inventory.Remove ("wood", 1.0, Wood_Value);
            Inventory.Add ("food", Added (2.0, Space),
                           Wood_Value);
         end;
      else
         Inventory.Spend (2.0);
         if not Has_Wood and then Inventory.Is_Full then
            Inventory.Make_Room_For ("wood", 2.0);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Lumberjack_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Food      : constant Quantity_Type :=
                    Inventory.Quantity ("food");
      Tools     : constant Quantity_Type :=
                    Inventory.Quantity ("tools");
      Has_Food  : constant Boolean := Food >= 1.0;
      Has_Tools : constant Boolean := Tools >= 1.0;
      Space     : constant Quantity_Type := Inventory.Available_Space;
   begin
      if Has_Food and then Has_Tools then
         --  produce 2 wood, consume 1 food, break tools with 10% chance

         declare
            Food_Value  : Money_Type;
            Tools_Value : Money_Type := 0.0;
         begin
            Inventory.Remove ("food", 1.0, Food_Value);
            if Agora.Random.Unit_Random <= 0.1 then
               Inventory.Remove ("tools", 1.0, Tools_Value);
            end if;
            Inventory.Add ("wood", Added (2.0, Space),
                           Food_Value + Tools_Value);
         end;
      elsif Has_Food then
         declare
            Food_Value : Money_Type;
         begin
            Inventory.Remove ("food", 1.0, Food_Value);
            Inventory.Add ("wood", Added (1.0, Space), Food_Value);
         end;
      else
         Inventory.Spend (2.0);
         if not Has_Food and then Inventory.Is_Full then
            Inventory.Make_Room_For ("food", 2.0);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Miner_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Food      : constant Quantity_Type :=
                    Inventory.Quantity ("food");
      Tools     : constant Quantity_Type :=
                    Inventory.Quantity ("tools");
      Has_Food  : constant Boolean := Food >= 1.0;
      Has_Tools : constant Boolean := Tools >= 1.0;
      Space     : constant Quantity_Type := Inventory.Available_Space;
   begin
      if Has_Food and then Has_Tools then
         --  produce 4 ore, consume 1 food, break tools with 10% chance

         declare
            Food_Value  : Money_Type;
            Tools_Value : Money_Type := 0.0;
         begin
            Inventory.Remove ("food", 1.0, Food_Value);
            if Agora.Random.Unit_Random <= 0.1 then
               Inventory.Remove ("tools", 1.0, Tools_Value);
            end if;
            Inventory.Add ("ore", Added (4.0, Space),
                           Food_Value + Tools_Value);
         end;
      elsif Has_Food then
         declare
            Food_Value : Money_Type;
         begin
            Inventory.Remove ("food", 1.0, Food_Value);
            Inventory.Add ("ore", Added (2.0, Space), Food_Value);
         end;
      else
         Inventory.Spend (2.0);
         if not Has_Food and then Inventory.Is_Full then
            Inventory.Make_Room_For ("food", 2.0);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Refiner_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Food      : constant Quantity_Type :=
                    Inventory.Quantity ("food");
      Tools     : constant Quantity_Type :=
                    Inventory.Quantity ("tools");
      Ore       : constant Quantity_Type :=
                    Inventory.Quantity ("ore");
      Has_Food  : constant Boolean := Food >= 1.0;
      Has_Tools : constant Boolean := Tools >= 1.0;
      Has_Ore   : constant Boolean := Ore >= 1.0;
   begin
      if Has_Food and then Has_Ore then

         if Has_Tools then
            --  convert all ore into metal, break tools with 10% chance

            declare
               Food_Value  : Money_Type;
               Ore_Value   : Money_Type;
               Tools_Value : Money_Type := 0.0;
            begin
               Inventory.Remove ("food", 1.0, Food_Value);
               Inventory.Remove ("ore", Ore, Ore_Value);
               if Agora.Random.Unit_Random <= 0.1 then
                  Inventory.Remove ("tools", 1.0, Tools_Value);
               end if;
               Inventory.Add ("metal", Ore,
                              Food_Value + Ore_Value + Tools_Value);
            end;
         else
            --  convert up to 2 ore into metal, consume 1 food
            declare
               Food_Value  : Money_Type;
               Ore_Value   : Money_Type;
               Convert     : constant Quantity_Type :=
                               Quantity_Type'Min (Ore, 2.0);
            begin
               Inventory.Remove ("food", 1.0, Food_Value);
               Inventory.Remove ("ore", Convert, Ore_Value);
               Inventory.Add ("metal", Convert, Food_Value + Ore_Value);
            end;
         end if;
      else
         Inventory.Spend (2.0);
         if not Has_Food and then Inventory.Is_Full then
            Inventory.Make_Room_For ("food", 2.0);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Worker_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Food      : constant Quantity_Type :=
                    Inventory.Quantity ("food");
      Has_Food  : constant Boolean := Food >= 1.0;
      Work      : constant Quantity_Type :=
                    Inventory.Quantity ("work");
      Need_Work : constant Boolean := Work < 1.0;
      Space     : constant Quantity_Type := Inventory.Available_Space;
      Food_Value : Money_Type;
   begin

      if Has_Food then
         Inventory.Remove ("food", 1.0, Food_Value);
         if Need_Work then
            Inventory.Add ("work", Added (1.0, Space), Food_Value);
         end if;
      end if;
   end Execute;

   ------------
   -- Farmer --
   ------------

   function Farmer return Agora.Profession.Reference is
   begin
      return Farmer_Singleton'Access;
   end Farmer;

   --------------------
   -- Ideal_Quantity --
   --------------------

   overriding function Ideal_Quantity
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
   begin
      for Config of Profession_Config loop
         if Config.Get ("id") = Dispatch (This).Tag then
            declare
               Inv_Config : constant Tropos.Configuration :=
                              Config.Child ("inventory");
               Ideal_Config : constant Tropos.Configuration :=
                                Inv_Config.Child ("ideal");
               Quantity     : constant Natural :=
                                Ideal_Config.Get (Commodity.Tag, 0);
            begin
               return Quantity_Type (Quantity);
            end;
         end if;
      end loop;
      raise Constraint_Error with
        "cannot find configuration for profession '"
        & Dispatch (This).Tag
        & "'";
   end Ideal_Quantity;

   ----------------
   -- Lumberjack --
   ----------------

   function Lumberjack return Agora.Profession.Reference is
   begin
      return Lumberjack_Singleton'Access;
   end Lumberjack;

   -----------
   -- Miner --
   -----------

   function Miner return Agora.Profession.Reference is
   begin
      return Miner_Singleton'Access;
   end Miner;

   -------------
   -- Refiner --
   -------------

   function Refiner return Agora.Profession.Reference is
   begin
      return Refiner_Singleton'Access;
   end Refiner;

   ----------------------------
   -- Save_Profession_Config --
   ----------------------------

   procedure Save_Profession_Config
     (Config : Tropos.Configuration)
   is
   begin
      Profession_Config := Config;
   end Save_Profession_Config;

   ------------
   -- Worker --
   ------------

   function Worker return Agora.Profession.Reference is
   begin
      return Worker_Singleton'Access;
   end Worker;

end Agora.Dnp.Profession;
