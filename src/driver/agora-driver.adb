with Tropos.Reader;

with Agora.Market.Configure;
with Agora.Market.Reports;

with Agora.Commodity;
with Agora.Logging;
with Agora.Paths;

procedure Agora.Driver is
begin

   Agora.Logging.Start_Logging;

   declare
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Json_Config
                   (Agora.Paths.Config_File ("dnp.json"));
      Market : constant Agora.Market.Reference :=
                 Agora.Market.Configure.Create_Market (Config);
   begin
      for I in 1 .. 100 loop
         Agora.Logging.Log ("--- START ROUND" & I'Img);
         Market.Execute;
         Agora.Logging.Log ("--- END ROUND" & I'Img);
      end loop;

      Agora.Market.Reports.Report_Market (Market);

      for Commodity of Agora.Commodity.All_Commodities loop
         Agora.Market.Reports.Export_CSV
           (This      => Market,
            Commodity => Commodity,
            Path      => Commodity.Tag & ".csv");
      end loop;
   end;

   Agora.Logging.Stop_Logging;

exception
   when others =>
      Agora.Logging.Stop_Logging;
      raise;
end Agora.Driver;
