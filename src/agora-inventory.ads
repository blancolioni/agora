private with WL.String_Maps;

with Tropos;

with Agora.Commodity;

package Agora.Inventory is

   type Instance is tagged private;
   subtype Any_Instance is Instance'Class;

   type Reference is access all Any_Instance;
   type Constant_Reference is access constant Any_Instance;

   function Cash (This : Any_Instance) return Money_Type;

   procedure Earn (This : in out Any_Instance;
                   Amount : Money_Type);

   procedure Spend (This   : in out Any_Instance;
                    Amount : Money_Type);

   procedure Record_Cost
     (This : in out Any_Instance;
      Amount : Money_Type);

   function Recorded_Costs
     (This : Any_Instance)
      return Money_Type;

   procedure Clear_Recorded_Costs
     (This : in out Any_Instance);

   function Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type;

   function Quantity
     (This : Any_Instance;
      Tag  : String)
      return Quantity_Type;

   function Value
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Money_Type;

   function Price
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Price_Type;

   function Max_Quantity
     (This      : Any_Instance)
      return Quantity_Type;

   function Used_Space
     (This      : Any_Instance)
      return Quantity_Type;

   function Available_Space
     (This      : Any_Instance)
      return Quantity_Type
     with Post =>
       This.Max_Quantity - This.Used_Space = Available_Space'Result;

   function Is_Empty
     (This : Any_Instance)
      return Boolean
     with Post =>
       Is_Empty'Result = (Available_Space (This) = Max_Quantity (This));

   function Is_Full
     (This : Any_Instance)
      return Boolean
     with Post =>
       Is_Full'Result = (Available_Space (This) = 0.0);

   procedure Set_Quantity
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : Money_Type)
     with Pre => Quantity
       <= This.Available_Space / Commodity.Size + This.Quantity (Commodity),
       Post => This.Quantity (Commodity) = Quantity;

   procedure Set_Ideal_Quantity
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
     with Post => This.Ideal (Commodity) = Quantity;

   procedure Add
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : Money_Type)
     with Pre => Quantity * Commodity.Size <= This.Available_Space,
     Post => This.Available_Space = This.Available_Space'Old
       - Quantity * Commodity.Size
     and then This.Quantity (Commodity) >= Quantity;

   procedure Add
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type;
      Value     : Money_Type);

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Value     : out Money_Type)
     with Pre => Quantity <= This.Quantity (Commodity),
     Post => This.Available_Space = This.Available_Space'Old
       + Quantity * Commodity.Size;

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
     with Pre => Quantity <= This.Quantity (Commodity),
     Post => This.Available_Space = This.Available_Space'Old
       + Quantity * Commodity.Size;

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type;
      Value     : out Money_Type);

   procedure Remove
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type);

   procedure Make_Room_For
     (This      : in out Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
     with Pre => Commodity.Size * Quantity <= This.Max_Quantity,
     Post => This.Available_Space >= Commodity.Size * Quantity;

   procedure Make_Room_For
     (This      : in out Any_Instance;
      Commodity : String;
      Quantity  : Quantity_Type);

   function Ideal
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type;

   function Shortage
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
     with Post => Shortage'Result =
       (if This.Quantity (Commodity) >= This.Ideal (Commodity)
          then 0.0 else This.Ideal (Commodity) - This.Quantity (Commodity));

   function Surplus
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
     with Post => Surplus'Result =
       (if This.Quantity (Commodity) <= This.Ideal (Commodity)
          then 0.0 else This.Quantity (Commodity) - This.Ideal (Commodity));

   function Image
     (This : Any_Instance)
      return String;

   function Create_Inventory
     (Config     : Tropos.Configuration;
      Start_Cash : Money_Type)
      return Reference;

   function Unit_Inventory return Constant_Reference;
   function Zero_Inventory return Constant_Reference;

   procedure Clear (This : in out Any_Instance);

   procedure Reset_Cash
     (This : in out Any_Instance;
      Cash : Money_Type);

private

   type Commodity_Record is
      record
         Commodity : Agora.Commodity.Reference;
         Quantity  : Quantity_Type;
         Ideal     : Quantity_Type;
         Value     : Money_Type;
      end record;

   package Commodity_Maps is
     new WL.String_Maps (Commodity_Record);

   type Instance is tagged
      record
         Is_Constant    : Boolean;
         Constant_Value : Quantity_Type;
         Cash           : Money_Type;
         Costs          : Money_Type;
         Space          : Quantity_Type;
         Map            : Commodity_Maps.Map;
      end record;

   function Cash (This : Any_Instance) return Money_Type
   is (This.Cash);

   function Recorded_Costs
     (This : Any_Instance)
      return Money_Type
   is (This.Costs);

end Agora.Inventory;
