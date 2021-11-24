package body Agora.Process.Producers is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
      Quantity : constant Quantity_Type :=
                   (if This.Produce_All
                    then Context.This_Quantity
                    else This.Quantity.Evaluate (Inventory));
   begin
      pragma Assert (Context.Produced = 0.0);
      Context.Production := This.Commodity;
      Context.Produced := Quantity;
   end Execute;

end Agora.Process.Producers;
