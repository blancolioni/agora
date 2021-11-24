private with Ada.Strings.Unbounded;

with Agora.Commodity;
with Agora.Inventory;
with Agora.Process;

package Agora.Profession is

   type Instance is tagged private;
   subtype Any_Instance is Instance'Class;
   type Reference is access constant Any_Instance;

   function Tag
     (This : Any_Instance)
      return String;

   procedure Execute
     (This : Any_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   function Production
     (This : Any_Instance)
      return Agora.Commodity.Reference;

   function Ideal_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type;

   procedure Set_Ideal_Quantities
     (This      : Any_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance);

   function Create
     (Tag        : String;
      Production : not null access constant Agora.Commodity.Any_Instance;
      Process    : not null access constant Agora.Process.Any_Instance)
      return Reference;

   type Reference_Array is array (Positive range <>) of Reference;
   Empty : Reference_Array (1 .. 0);

   function Get (Tag : String) return Reference;
   function All_Professions return Reference_Array;

   procedure Save_Profession
     (Profession : not null access constant Any_Instance);

private

   type Instance is tagged
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Production : Agora.Commodity.Reference;
         Process    : Agora.Process.Reference;
      end record;

   function Tag
     (This : Any_Instance)
      return String
   is (Ada.Strings.Unbounded.To_String (This.Tag));

   function Production
     (This : Any_Instance)
      return Agora.Commodity.Reference
   is (This.Production);

end Agora.Profession;
