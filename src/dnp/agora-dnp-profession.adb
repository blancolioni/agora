with Agora.Commodity;
with Agora.Process;

with Agora.Expression;

package body Agora.Dnp.Profession is

   function Food return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("food"));

   function Metal return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("metal"));

   function Ore return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("ore"));

   function Tools return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("tools"));

   function Wood return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("wood"));

   function Work return Agora.Commodity.Reference
   is (Agora.Commodity.Get ("work"));

   ----------------
   -- Blacksmith --
   ----------------

   function Blacksmith return Agora.Profession.Reference is
      use Agora.Process;
      Query : constant Agora.Process.Reference_Array :=
                (1 => Check_Quantity_At_Least (Food, 1.0),
                 2 => Check_Quantity_At_Least (Metal, 1.0),
                 3 => Check_Quantity_At_Most (Tools, 3.0));
      Perform : constant Agora.Process.Reference_Array :=
                  (1 => Consume_All (Metal),
                   2 => Produce_All (Tools));

      Process : constant Agora.Process.Reference :=
                  Sequence ((Consume (Food, 1.0),
                            If_Then (Query, Perform)));
   begin
      return Agora.Profession.Create
        ("blacksmith", Tools, Process);
   end Blacksmith;

   ------------
   -- Farmer --
   ------------

   function Farmer return Agora.Profession.Reference is
      use Agora.Expression;
      use Agora.Process;
      Need_Food_Query : constant Agora.Process.Reference_Array :=
                          (1 => Check_Quantity_At_Most (Food, 9.0));
      Have_Tools      : constant Agora.Process.Reference_Array :=
                          (1 => Check_Quantity_More_Than (Wood, 0.0),
                           2 => Check_Quantity_At_Least (Tools, 1.0),
                           3 => Check_Quantity_More_Than (Work, 0.0));
      No_Tools        : constant Agora.Process.Reference_Array :=
                          (1 => Check_Quantity_More_Than (Wood, 0.0),
                           2 => Check_Quantity_At_Most (Tools, 0.0),
                           3 => Check_Quantity_More_Than (Work, 0.0));
      Wood_Factor     : constant Agora.Expression.Reference :=
                          Clamp
                            (Inventory_Expression (Wood),
                             Constant_Expression (1.0));
      Work_Factor     : constant Agora.Expression.Reference :=
                            Clamp
                              (Inventory_Expression (Work),
                               Constant_Expression (1.0));
      Production_Factor : constant Agora.Expression.Reference :=
                            Min (Wood_Factor, Work_Factor);
      Perform_With_Tools : constant Agora.Process.Reference_Array :=
                             (1 => Consume (Wood, 1.0),
                              2 => Consume (Tools, 1.0, 0.1),
                              3 => Consume (Work, 1.0),
                              4 => Produce (Food, Production_Factor * 6.0));
      Perform_No_Tools   : constant Agora.Process.Reference_Array :=
                             (1 => Consume (Wood, 1.0),
                              2 => Consume (Work, 1.0),
                              3 => Produce (Food, Production_Factor * 3.0));
      Perform_No_Inputs  : constant Agora.Process.Reference_Array :=
                             (1 => Produce (Food, 1.0));
      Process : constant Agora.Process.Reference :=
                             If_Then
                               (Condition    => Need_Food_Query,
                                True_Process =>
                                  (1 => If_Then_Else
                                     (Have_Tools,
                                      Perform_With_Tools,
                                      (1 => If_Then_Else
                                         (No_Tools,
                                          Perform_No_Tools,
                                          Perform_No_Inputs)))));
   begin
      return Agora.Profession.Create
        ("farmer", Food, Process);
   end Farmer;

   ----------------
   -- Lumberjack --
   ----------------

   function Lumberjack return Agora.Profession.Reference is
      use Agora.Expression;
      use Agora.Process;
      Can_Work_Query    : constant Agora.Process.Reference_Array :=
                            (Check_Quantity_At_Most (Wood, 3.0),
                             Check_Quantity_More_Than (Food, 0.0),
                             Check_Quantity_More_Than (Work, 0.0));
      Have_Tools         : constant Agora.Process.Reference_Array :=
                             (1 => Check_Quantity_At_Least (Tools, 1.0));
      Food_Factor        : constant Agora.Expression.Reference :=
                             Clamp
                               (Inventory_Expression (Food),
                                Constant_Expression (1.0));
      Work_Factor        : constant Agora.Expression.Reference :=
                             Clamp
                               (Inventory_Expression (Work),
                                Constant_Expression (1.0));
      Production_Factor  : constant Agora.Expression.Reference :=
                             Min (Food_Factor, Work_Factor);
      Perform_With_Tools : constant Agora.Process.Reference_Array :=
                             (1 => Consume (Food, 1.0),
                              2 => Consume (Tools, 1.0, 0.1),
                              3 => Consume (Work, 1.0),
                              4 => Produce (Wood, Production_Factor * 2.0));
      Perform_No_Tools   : constant Agora.Process.Reference_Array :=
                             (1 => Consume (Food, 1.0),
                              2 => Produce (Wood, Production_Factor));
      Process            : constant Agora.Process.Reference :=
                             If_Then
                               (Condition    => Can_Work_Query,
                                True_Process =>
                                  (1 => If_Then_Else
                                     (Have_Tools,
                                      Perform_With_Tools,
                                      Perform_No_Tools)));
   begin
      return Agora.Profession.Create
        ("woodcutter", Wood, Process);
   end Lumberjack;

   -----------
   -- Miner --
   -----------

   function Miner return Agora.Profession.Reference is
      use Agora.Process;
      Can_Work_Query     : constant Agora.Process.Reference_Array :=
                             (Check_Quantity_At_Most (Ore, 3.0),
                              Check_Quantity_At_Least (Food, 1.0));
      Have_Tools         : constant Agora.Process.Reference_Array :=
                             (1 => Check_Quantity_At_Least (Tools, 1.0));
      Perform_With_Tools : constant Agora.Process.Reference_Array :=
                             (Consume (Tools, 1.0, 0.1),
                              Produce (Ore, 4.0));
      Perform_No_Tools   : constant Agora.Process.Reference_Array :=
                             (1 => Produce (Ore, 2.0));
      Process            : constant Agora.Process.Reference :=
                             If_Then
                               (Condition    => Can_Work_Query,
                                True_Process =>
                                  (1 => If_Then_Else
                                     (Have_Tools,
                                      Perform_With_Tools,
                                      Perform_No_Tools)));
   begin
      return Agora.Profession.Create
        ("miner", Ore, Sequence ((Consume (Food, 1.0), Process)));
   end Miner;

   -------------
   -- Refiner --
   -------------

   function Refiner return Agora.Profession.Reference is
      use Agora.Process;
      Can_Work   : constant Agora.Process.Reference_Array :=
                     (1 => Check_Quantity_At_Least (Food, 1.0),
                      2 => Check_Quantity_At_Least (Ore, 1.0),
                      3 => Check_Quantity_At_Most (Metal, 3.0));
      Perform_With_Tools : constant Agora.Process.Reference_Array :=
                             (Consume (Tools, 1.0, 0.1),
                              Consume_All (Ore),
                              Produce_All (Metal));
      Perform_No_Tools   : constant Agora.Process.Reference_Array :=
                             (Consume (Ore, 2.0),
                              Produce_All (Metal));
      Process : constant Agora.Process.Reference :=
                  Sequence ((Consume (Food, 1.0),
                            If_Then
                              (Can_Work,
                               (1 =>
                                  If_Then_Else
                                    (Check_Quantity_At_Least (Tools, 1.0),
                                     Perform_With_Tools,
                                     Perform_No_Tools)))));
   begin
      return Agora.Profession.Create
        ("refiner", Metal, Process);
   end Refiner;

   ------------
   -- Worker --
   ------------

   function Worker return Agora.Profession.Reference is
      use Agora.Expression;
      use Agora.Process;
      Query : constant Agora.Process.Reference_Array :=
                (Check_Quantity_At_Most (Work, 0.0),
                 Check_Quantity_More_Than (Food, 0.0));
   begin
      return Agora.Profession.Create
        (Tag        => "worker",
         Production => Work,
         Process    =>
           Sequence ((Consume (Food, 1.0),
             If_Then
               (Query,
                (1 => Produce
                     (Work,
                      Min (Inventory_Expression (Food),
                        Constant_Expression (1.0)) * 2.0))))));
   end Worker;

end Agora.Dnp.Profession;
