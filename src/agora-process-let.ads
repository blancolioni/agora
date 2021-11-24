private with Ada.Strings.Unbounded;

with Agora.Process.Expression;

package Agora.Process.Let is

   subtype Parent is Agora.Process.Instance;

   type Instance is abstract new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access constant Any_Instance;

   overriding function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (No_Usage);

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is (No_Usage);

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type);

   function Create
     (Name  : String;
      Value : Agora.Process.Expression.Reference)
      return Reference;

private

   type Instance is abstract new Parent with
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Agora.Process.Expression.Reference;
      end record;

end Agora.Process.Let;
