with Ada.Containers.Doubly_Linked_Lists;

private package Agora.Process.Structure is

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   subtype Parent is Agora.Process.Instance;

   type Instance is new Parent with
      record
         Condition : Reference_Lists.List;
         Success   : Reference_Lists.List;
         Failure   : Reference_Lists.List;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage;

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage;

   overriding procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

   function Create
     (Condition, Success, Failure : Reference_Array)
      return Reference;

end Agora.Process.Structure;
