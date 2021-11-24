with Agora.Commodity;
with Agora.Inventory;

package Agora.Expression is

   type Instance is abstract tagged private;
   subtype Any_Instance is Instance'Class;
   type Reference is access constant Any_Instance;

   function Evaluate
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance)
      return Quantity_Type
      is abstract;

   function Image
     (This      : Instance)
      return String
      is abstract;

   function Constant_Expression
     (Value : Quantity_Type)
      return Reference;

   function Inventory_Expression
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference;

   type Reference_Array is
     array (Positive range <>) of Reference;

   function Function_Expression
     (Name      : String;
      Arguments : Reference_Array)
      return Reference;

   function Clamp (Left, Right : Reference) return Reference;
   function Min (Left, Right : Reference) return Reference;

   function "*" (Left, Right : Reference) return Reference;
   function "*" (Left : Reference; Right : Quantity_Type) return Reference;
   function "*" (Left : Quantity_Type; Right : Reference) return Reference;

private

   type Instance is abstract tagged
      record
         null;
      end record;

end Agora.Expression;
