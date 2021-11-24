with Agora.Configure;
with Agora.Dnp;

with Agora.Behaviour;
with Agora.Inventory;
with Agora.Profession;

with Agora.Commodity.Configure;

package body Agora.Market.Configure is

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market (Config : Tropos.Configuration) return Reference is
      Start_Agents : constant Tropos.Configuration :=
                       Config.Child ("start_conditions")
                       .Child ("agents");
      Next_Id      : Agent_Id := Agent_Id'First;
      Behaviour    : Agora.Behaviour.Reference;
      This         : constant Reference := new Instance;
   begin
      for Commodity_Config of Config.Child ("goods") loop
         Agora.Commodity.Configure.Create_Commodity
           (Config => Commodity_Config);
      end loop;

      Agora.Dnp.Load;

      Behaviour :=
        Agora.Configure.Get_Behaviour (Config.Config_Name);

      for Agent_Config of Config.Child ("agents") loop
         declare
            Tag  : constant String := Agent_Config.Get ("id");
            Cash : constant Money_Type :=
                     Money_Type (Float'(Agent_Config.Get ("money")));
            Inv  : constant Tropos.Configuration :=
                     Agent_Config.Child ("inventory");
            Profession : constant Agora.Profession.Reference :=
                           Agora.Profession.Get
                             (Agent_Config.Get ("logic"));
         begin
            for Index in 1 .. Start_Agents.Get (Tag) loop
               declare
                  Agent : constant Agora.Agent.Reference :=
                            Agora.Agent.Create
                              (Id         => Next_Id,
                               Behaviour  => Behaviour,
                               Inventory  =>
                                 Agora.Inventory.Create_Inventory
                                   (Config     => Inv,
                                    Start_Cash => Cash),
                               Profession => Profession);
               begin
                  This.Agents.Append (Agent);
                  Next_Id := Next_Id + 1;
               end;
            end loop;
         end;
      end loop;

      for Commodity of Agora.Commodity.All_Commodities loop
         declare
            List : History_Lists.List;
         begin
            List.Append
              (History_Record'
                 (Askers      => 1,
                  Bidders     => 1,
                  Asks        => 1.0,
                  Bids        => 1.0,
                  Traded      => 1.0,
                  Ask_Value   => Money_Type (Commodity.Initial_Price),
                  Bid_Value   => Money_Type (Commodity.Initial_Price),
                  Trade_Value => Money_Type (Commodity.Initial_Price)));
            This.History.Insert (Commodity.Tag, List);
         end;
      end loop;

      return This;
   end Create_Market;

end Agora.Market.Configure;
