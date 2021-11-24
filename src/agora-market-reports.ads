package Agora.Market.Reports is

   procedure Report_Market (This : Reference);

   procedure Export_CSV
     (This      : Reference;
      Commodity : Agora.Commodity.Reference;
      Path      : String);

end Agora.Market.Reports;
