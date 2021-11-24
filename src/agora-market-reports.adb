with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.Reports.Tables;

with Agora.Images;

package body Agora.Market.Reports is

   ----------------
   -- Export_CSV --
   ----------------

   procedure Export_CSV
     (This      : Reference;
      Commodity : Agora.Commodity.Reference;
      Path      : String)
   is
      use Ada.Text_IO;
      use Agora.Images;
      Round : Natural := 0;
      File : File_Type;
      function Image (X : Integer) return String
      is (Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Left));

   begin
      Create (File, Out_File, Path);
      Set_Output (File);
      Put_Line
        ("round,sellers,buyers,asks,bids,avg-ask-price,avg-bid-price,"
         & "traded, value, price");
      for Element of This.History (Commodity.Tag) loop
         declare
            Avg_Ask_Price : constant Price_Delta_Type :=
                              (if Element.Asks = 0.0
                               then 0.0
                               else Element.Ask_Value / Element.Asks);
            Avg_Bid_Price : constant Price_Delta_Type :=
                              (if Element.Bids = 0.0
                               then 0.0
                               else Element.Bid_Value / Element.Bids);
         begin
            Put_Line
              (Image (Round)
               & ","
               & Image (Element.Askers)
               & ","
               & Image (Element.Bidders)
               & ","
               & Image (Element.Asks)
               & ","
               & Image (Element.Bids)
               & ","
               & Image (Avg_Ask_Price)
               & ","
               & Image (Avg_Bid_Price)
               & ","
               & Image (Element.Traded)
               & ","
               & Image (Element.Trade_Value)
               & ","
               & (if Element.Traded > 0.0
                 then Image (Element.Trade_Value / Element.Traded)
                 else "0"));
         end;
         Round := Round + 1;
      end loop;

      Set_Output (Standard_Output);
      Close (File);
   end Export_CSV;

   -------------------
   -- Report_Market --
   -------------------

   procedure Report_Market (This : Reference) is
      Table : WL.Reports.Tables.Table_Report;
   begin
      Table.Add_Column ("COMMODITY");
      Table.Add_Column ("PRODUCERS");
      Table.Add_Column ("AVE. PRICE");
      Table.Add_Column ("ASKS");
      Table.Add_Column ("BIDS");
      Table.Add_Column ("TRADED");

      for Commodity of Agora.Commodity.All_Commodities loop
         Table.Append_Row;
         Table.Append_Cell (Commodity.Tag);

         declare
            use type Agora.Commodity.Reference;
            Producers : Natural := 0;
         begin
            for Agent of This.Agents loop
               if Agent.Profession.Production = Commodity then
                  Producers := Producers + 1;
               end if;
            end loop;
            Table.Append_Cell (Producers'Image);
         end;
         Table.Append_Cell
           (Agora.Images.Image
              (This.Average_Historical_Price (Commodity, 10)));
         Table.Append_Cell
           (Agora.Images.Image
              (This.Average_Historical_Asks (Commodity, 10)));
         Table.Append_Cell
           (Agora.Images.Image
              (This.Average_Historical_Bids (Commodity, 10)));
         Table.Append_Cell ("");
      end loop;

      WL.Reports.Put (Table);

   end Report_Market;

end Agora.Market.Reports;
