package Agora.Worker is

   type Instance is interface;
   subtype Any_Instance is Instance'Class;

   type Reference is access constant Any_Instance;

   procedure Produce
     (This :

end Agora.Worker;
