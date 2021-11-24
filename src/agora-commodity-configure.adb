package body Agora.Commodity.Configure is

   ----------------------
   -- Create_Commodity --
   ----------------------

   procedure Create_Commodity (Config : Tropos.Configuration) is
   begin
      New_Commodity
        (Tag  => Config.Get ("id"),
         Size =>
           Quantity_Type (Float'(Config.Get ("size", 1.0))),
         Initial_Price =>
           Price_Type (Float'(Config.Get ("price", 1.0))));
   end Create_Commodity;

end Agora.Commodity.Configure;
