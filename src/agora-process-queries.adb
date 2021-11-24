package body Agora.Process.Queries is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Quantity_At_Least;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
   begin
      if Context.This_Flag then
         Context.This_Flag :=
           Inventory.Quantity (This.Commodity) >= This.Quantity;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Quantity_At_Most;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
   begin
      if Context.This_Flag then
         Context.This_Flag :=
           Inventory.Quantity (This.Commodity) <= This.Quantity;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      : Quantity_Greater_Than;
      Inventory : not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
   begin
      if Context.This_Flag then
         Context.This_Flag :=
           Inventory.Quantity (This.Commodity) > This.Quantity;
      end if;
   end Execute;

end Agora.Process.Queries;
