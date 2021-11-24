private with WL.String_Maps;

with Agora.Commodity;
with Agora.Expression;
with Agora.Inventory;

package Agora.Process is

   type Commodity_Usage_Class is
     (Not_Used, Chance_Of_Use, Use_Quantity, Use_All);

   type Commodity_Usage is
      record
         Class    : Commodity_Usage_Class := Not_Used;
         Quantity : Quantity_Type         := 0.0;
         Chance   : Real                 := 1.0;
      end record;

   No_Usage : constant Commodity_Usage := (Not_Used, 0.0, 1.0);

   function Join (Left, Right : Commodity_Usage) return Commodity_Usage;

   type Instance is interface;
   subtype Any_Instance is Instance'Class;
   type Reference is access constant Any_Instance;

   function Image
     (This : Instance)
      return String
      is abstract;

   function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
      is abstract;

   function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
      is abstract;

   type Context_Type is private;

   procedure Apply
     (Context   : Context_Type;
      Inventory : not null access Agora.Inventory.Any_Instance);

   procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is abstract;

   function Check_Quantity_At_Least
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference;

   function Check_Quantity_More_Than
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference;

   function Check_Quantity_At_Most
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference;

   function Consume
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Chance    : Real := 1.0)
      return Reference;

   function Consume_All
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference;

   function Produce
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference;

   function Produce
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Agora.Expression.Reference)
      return Reference;

   function Produce_All
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference;

   type Reference_Array is array (Positive range <>) of Reference;
   Empty : Reference_Array (1 .. 0);

   function Sequence
     (Steps : Reference_Array)
      return Reference;

   function If_Then_Else
     (Condition     : Reference_Array;
      True_Process  : Reference_Array;
      False_Process : Reference_Array)
      return Reference;

   function If_Then_Else
     (Condition     : Reference;
      True_Process  : Reference_Array;
      False_Process : Reference_Array)
      return Reference;

   function If_Then
     (Condition     : Reference_Array;
      True_Process  : Reference_Array)
      return Reference;

   function If_Then
     (Condition     : Reference;
      True_Process  : Reference_Array)
      return Reference;

private

   type Commodity_Record is
      record
         Quantity : Quantity_Type := 0.0;
         Value    : Money_Type    := 0.0;
      end record;

   package Commodity_Maps is new WL.String_Maps (Commodity_Record);

   package Environment_Maps is
     new WL.String_Maps (Real);

   type Context_Type is
      record
         Current_Cost  : Money_Type := 0.0;
         This_Quantity : Quantity_Type := 0.0;
         This_Flag     : Boolean       := False;
         Environment   : Environment_Maps.Map;
         Consumption   : Commodity_Maps.Map;
         Production    : Agora.Commodity.Reference;
         Produced      : Quantity_Type := 0.0;
      end record;

   function If_Then_Else
     (Condition     : Reference;
      True_Process  : Reference_Array;
      False_Process : Reference_Array)
      return Reference
   is (If_Then_Else ((1 => Condition), True_Process, False_Process));

   function If_Then
     (Condition     : Reference;
      True_Process  : Reference_Array)
      return Reference
     is (If_Then ((1 => Condition), True_Process));

end Agora.Process;
