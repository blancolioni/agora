with Agora.Expression;

private package Agora.Process.Producers is

   subtype Parent is Agora.Process.Instance;

   type Instance is new Parent with
      record
         Commodity   : Agora.Commodity.Reference;
         Quantity    : Agora.Expression.Reference;
         Chance      : Real;
         Produce_All : Boolean;
      end record;

   overriding function Image
     (This : Instance)
      return String
   is ("produce"
       & (if This.Produce_All then ""
         else " " & This.Quantity.Image)
       & " " & This.Commodity.Tag);

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (if This.Commodity.Tag /= Commodity.Tag
       then (Not_Used, 0.0, 1.0)
       elsif This.Produce_All
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

   overriding function Consumption
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

end Agora.Process.Producers;
