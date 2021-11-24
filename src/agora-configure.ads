with Agora.Auction;
with Agora.Behaviour;

package Agora.Configure is

   function Get_Behaviour (Tag : String) return Agora.Behaviour.Reference;

   procedure Save_Behaviour
     (This : not null access constant Agora.Behaviour.Any_Instance);

   function Standard_Auction
     return Agora.Auction.Any_Instance;

end Agora.Configure;
