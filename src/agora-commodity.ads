private with Ada.Strings.Unbounded;

package Agora.Commodity is

   type Instance is tagged private;
   subtype Any_Instance is Instance'Class;

   type Reference is access constant Any_Instance;

   function Tag (This : Any_Instance) return String;
   function Size (This : Any_Instance) return Quantity_Type;
   function Initial_Price (This : Any_Instance) return Price_Type;

   type Reference_Array is array (Positive range <>) of Reference;
   Empty : Reference_Array (1 .. 0);

   function All_Commodities return Reference_Array;

   function All_Commodities_Excluding
     (Excluding : Reference_Array)
      return Reference_Array;

   function Get (Tag : String) return Reference;

private

   type Instance is tagged
      record
         Tag           : Ada.Strings.Unbounded.Unbounded_String;
         Size          : Quantity_Type;
         Initial_Price : Price_Type;
      end record;

   procedure New_Commodity
     (Tag           : String;
      Size          : Quantity_Type;
      Initial_Price : Price_Type);

   function Tag (This : Any_Instance) return String
   is (Ada.Strings.Unbounded.To_String (This.Tag));

   function Size (This : Any_Instance) return Quantity_Type
   is (This.Size);

   function Initial_Price (This : Any_Instance) return Price_Type
   is (This.Initial_Price);

end Agora.Commodity;
