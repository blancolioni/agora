with Agora.Images;

private package Agora.Process.Queries is

   subtype Parent is Agora.Process.Instance;

   type Instance is abstract new Parent with
      record
         Commodity : Agora.Commodity.Reference;
         Quantity  : Quantity_Type;
      end record;

   overriding function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (Commodity_Usage'(Class    => Not_Used,
                        Quantity => 0.0,
                        Chance   => 1.0));

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (Commodity_Usage'(Class    => Not_Used,
                        Quantity => 0.0,
                        Chance   => 1.0));

   type Quantity_At_Least is new Instance with null record;

   overriding function Image
     (This : Quantity_At_Least)
      return String
   is (This.Commodity.Tag & " >= " & Agora.Images.Image (This.Quantity));

   overriding procedure Execute
     (This      : Quantity_At_Least;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

   type Quantity_Greater_Than is new Instance with null record;

   overriding function Image
     (This : Quantity_Greater_Than)
      return String
   is (This.Commodity.Tag & " > " & Agora.Images.Image (This.Quantity));

   overriding procedure Execute
     (This      : Quantity_Greater_Than;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

   type Quantity_At_Most is new Instance with null record;

   overriding function Image
     (This : Quantity_At_Most)
      return String
   is (This.Commodity.Tag & " <= " & Agora.Images.Image (This.Quantity));

   overriding procedure Execute
     (This      : Quantity_At_Most;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

end Agora.Process.Queries;
