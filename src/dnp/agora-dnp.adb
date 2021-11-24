with Agora.Configure;
with Agora.Profession;

with Agora.Dnp.Behaviour;
with Agora.Dnp.Profession;

package body Agora.Dnp is

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      Agora.Configure.Save_Behaviour (Agora.Dnp.Behaviour.Singleton);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Blacksmith);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Farmer);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Lumberjack);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Miner);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Refiner);
      Agora.Profession.Save_Profession (Agora.Dnp.Profession.Worker);
   end Load;

end Agora.Dnp;
