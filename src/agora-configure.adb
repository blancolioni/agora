with WL.String_Maps;

with Agora.Auction.Asymmetric;

package body Agora.Configure is

   package Behaviour_Maps is
     new WL.String_Maps (Agora.Behaviour.Reference, Agora.Behaviour."=");

   Behaviour_Map : Behaviour_Maps.Map;

   -------------------
   -- Get_Behaviour --
   -------------------

   function Get_Behaviour (Tag : String) return Agora.Behaviour.Reference is
   begin
      return Behaviour_Map.Element (Tag);
   end Get_Behaviour;

   --------------------
   -- Save_Behaviour --
   --------------------

   procedure Save_Behaviour
     (This : not null access constant Agora.Behaviour.Any_Instance)
   is
   begin
      Behaviour_Map.Insert (This.Tag, Agora.Behaviour.Reference (This));
   end Save_Behaviour;

   ----------------------
   -- Standard_Auction --
   ----------------------

   function Standard_Auction
     return Agora.Auction.Any_Instance
   is
   begin
      return Agora.Auction.Asymmetric.Get_Auction;
   end Standard_Auction;

end Agora.Configure;
