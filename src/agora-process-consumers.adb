with Agora.Logging;
with Agora.Random;

package body Agora.Process.Consumers is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
      Inv : constant Quantity_Type := Inventory.Quantity (This.Commodity);
      Quantity : constant Quantity_Type :=
                   (if This.Consume_All
                    then Inv
                    else Quantity_Type'Min
                      (Inv,
                       This.Quantity.Evaluate
                         (Inventory)));

      Full_Value : constant Money_Type :=
                     (if Quantity = 0.0
                      then 0.0
                      else Quantity * Inventory.Price (This.Commodity));

      procedure Consume (Quantity : Quantity_Type;
                         Value    : Money_Type);

      -------------
      -- Consume --
      -------------

      procedure Consume (Quantity : Quantity_Type;
                         Value    : Money_Type)
      is
      begin

         Context.This_Quantity := Quantity;

         if not Context.Consumption.Contains (This.Commodity.Tag) then
            Context.Consumption.Insert
              (Key      => This.Commodity.Tag,
               New_Item => Commodity_Record'
                 (Quantity => 0.0,
                  Value    => 0.0));
         end if;

         declare
            Rec : Commodity_Record renames
                    Context.Consumption (This.Commodity.Tag);
         begin
            Rec.Quantity := Rec.Quantity + Quantity;
            Rec.Value := Rec.Value + Value;
         end;

         Agora.Logging.Log
           ("consume " & Agora.Images.Image (Quantity)
            & " " & This.Commodity.Tag
            & " valued at " & Agora.Images.Image (Value));
      end Consume;

   begin
      if Quantity > 0.0 then
         Context.Current_Cost := Context.Current_Cost
           + Money_Type (Real (Full_Value) * This.Chance);
         if This.Chance >= 1.0
           or else Agora.Random.Unit_Random < This.Chance
         then
            Consume (Quantity, Full_Value);
         end if;
      end if;
   end Execute;

end Agora.Process.Consumers;
