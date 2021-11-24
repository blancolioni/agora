with Agora.Process.Consumers;
with Agora.Process.Producers;
with Agora.Process.Queries;
with Agora.Process.Structure;

with Agora.Images;
with Agora.Logging;

package body Agora.Process is

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Context   : Context_Type;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
   begin
      for Position in Context.Consumption.Iterate loop
         Inventory.Remove (Commodity_Maps.Key (Position),
                           Commodity_Maps.Element (Position).Quantity);
      end loop;
      if Context.Produced > 0.0 then
         declare
            Value : constant Money_Type :=
                      (if Context.Current_Cost = 0.0 then 1.0
                       else Context.Current_Cost)
                      + Inventory.Recorded_Costs;
         begin
            Inventory.Add (Context.Production, Context.Produced, Value);
            Inventory.Clear_Recorded_Costs;
            Agora.Logging.Log
              ("produced " & Agora.Images.Image (Context.Produced)
               & " " & Context.Production.Tag
               & " for " & Agora.Images.Image (Value));
         end;
      else
         Inventory.Record_Cost (Context.Current_Cost);
      end if;
   end Apply;

   -----------------------------
   -- Check_Quantity_At_Least --
   -----------------------------

   function Check_Quantity_At_Least
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference
   is
   begin
      return new Queries.Quantity_At_Least'
        (Commodity => Agora.Commodity.Reference (Commodity),
         Quantity  => Quantity);
   end Check_Quantity_At_Least;

   ----------------------------
   -- Check_Quantity_At_Most --
   ----------------------------

   function Check_Quantity_At_Most
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type) return Reference
   is
   begin
      return new Queries.Quantity_At_Most'
        (Commodity => Agora.Commodity.Reference (Commodity),
         Quantity  => Quantity);
   end Check_Quantity_At_Most;

   ------------------------------
   -- Check_Quantity_More_Than --
   ------------------------------

   function Check_Quantity_More_Than
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference
   is
   begin
      return new Queries.Quantity_Greater_Than'
        (Commodity => Agora.Commodity.Reference (Commodity),
         Quantity  => Quantity);
   end Check_Quantity_More_Than;

   -------------
   -- Consume --
   -------------

   function Consume
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type;
      Chance    : Real := 1.0)
      return Reference
   is
   begin
      return new Consumers.Instance'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    =>
           Agora.Expression.Constant_Expression (Quantity),
         Chance      => Chance,
         Consume_All => False);
   end Consume;

   -----------------
   -- Consume_All --
   -----------------

   function Consume_All
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference
   is
   begin
      return new Consumers.Instance'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    => null,
         Chance      => 1.0,
         Consume_All => True);
   end Consume_All;

   -------------
   -- If_Then --
   -------------

   function If_Then
     (Condition    : Reference_Array;
      True_Process : Reference_Array)
      return Reference
   is
   begin
      return Structure.Create
        (Condition => Condition,
         Success   => True_Process,
         Failure   => Empty);
   end If_Then;

   ------------------
   -- If_Then_Else --
   ------------------

   function If_Then_Else
     (Condition     : Reference_Array;
      True_Process  : Reference_Array;
      False_Process : Reference_Array)
      return Reference
   is
   begin
      return Structure.Create
        (Condition => Condition,
         Success   => True_Process,
         Failure   => False_Process);
   end If_Then_Else;

   function Join (Left, Right : Commodity_Usage) return Commodity_Usage is
   begin
      case Left.Class is
         when Not_Used =>
            return Right;
         when Chance_Of_Use =>
            case Right.Class is
               when Not_Used =>
                  return Left;
               when Chance_Of_Use =>
                  return (Chance_Of_Use,
                          Quantity_Type'Max (Left.Quantity, Right.Quantity),
                          Real'Max (Left.Chance, Right.Chance));
               when Use_Quantity =>
                  return Right;
               when Use_All =>
                  return Right;
            end case;
         when Use_Quantity =>
            case Right.Class is
               when Not_Used =>
                  return Left;
               when Chance_Of_Use =>
                  return Left;
               when Use_Quantity =>
                  return (Use_Quantity,
                          Quantity_Type'Max (Left.Quantity, Right.Quantity),
                          1.0);
               when Use_All =>
                  return Right;
            end case;
         when Use_All =>
            return Left;
      end case;
   end Join;

   -------------
   -- Produce --
   -------------

   function Produce
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Quantity_Type)
      return Reference
   is (Produce
       (Commodity, Agora.Expression.Constant_Expression (Quantity)));

   -------------
   -- Produce --
   -------------

   function Produce
     (Commodity : not null access constant Agora.Commodity.Any_Instance;
      Quantity  : Agora.Expression.Reference)
      return Reference
   is
   begin
      return new Producers.Instance'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    => Quantity,
         Chance      => 1.0,
         Produce_All => False);
   end Produce;

   -----------------
   -- Produce_All --
   -----------------

   function Produce_All
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference
   is
   begin
      return new Producers.Instance'
        (Commodity   => Agora.Commodity.Reference (Commodity),
         Quantity    => null,
         Chance      => 1.0,
         Produce_All => True);
   end Produce_All;

   --------------
   -- Sequence --
   --------------

   function Sequence (Steps : Reference_Array) return Reference is
   begin
      return Structure.Create
        (Condition => Empty,
         Success   => Steps,
         Failure   => Empty);
   end Sequence;

end Agora.Process;
