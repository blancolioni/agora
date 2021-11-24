with Agora.Images;
with Agora.Expression;

private package Agora.Process.Consumers is

   subtype Parent is Agora.Process.Instance;

   type Instance is new Parent with
      record
         Commodity   : Agora.Commodity.Reference;
         Quantity    : Agora.Expression.Reference;
         Chance      : Real;
         Consume_All : Boolean;
      end record;

   overriding function Image
     (This : Instance)
      return String
   is ("consume "
       & (if This.Consume_All then "all"
         else This.Quantity.Image)
       & " " & This.Commodity.Tag
       & (if This.Chance < 1.0
         then " with chance "
         & Agora.Images.Image (This.Chance * Real'(100.0)) & "%"
         else ""));

   overriding function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (if This.Commodity.Tag /= Commodity.Tag
       then (Not_Used, 0.0, 1.0)
       elsif This.Consume_All
       then Commodity_Usage'(Class    => Use_All,
                             Quantity => 0.0,
                             Chance   => 1.0)
       elsif This.Chance < 1.0 then
          Commodity_Usage'(Class    => Chance_Of_Use,
                           Quantity =>
                              This.Quantity.Evaluate
                             (Agora.Inventory.Unit_Inventory),
                           Chance   => This.Chance)
       else Commodity_Usage'(Class    => Use_Quantity,
                             Quantity =>
                                This.Quantity.Evaluate
                                  (Agora.Inventory.Unit_Inventory),
                             Chance   => 1.0));

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (Commodity_Usage'(Class    => Not_Used,
                        Quantity => 0.0,
                        Chance   => 1.0));

   overriding procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

end Agora.Process.Consumers;
