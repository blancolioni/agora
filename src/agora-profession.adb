with Ada.Containers.Vectors;

with Agora.Logging;

package body Agora.Profession is

   package Profession_Vectors is
     new Ada.Containers.Vectors (Positive, Reference);

   Profession_Vector : Profession_Vectors.Vector;

   ---------------------
   -- All_Professions --
   ---------------------

   function All_Professions return Reference_Array is
   begin
      return Arr : Reference_Array (1 .. Profession_Vector.Last_Index) do
         for I in Arr'Range loop
            Arr (I) := Profession_Vector (I);
         end loop;
      end return;
   end All_Professions;

   ------------
   -- Create --
   ------------

   function Create
     (Tag        : String;
      Production : not null access constant Agora.Commodity.Any_Instance;
      Process    : not null access constant Agora.Process.Any_Instance)
      return Reference
   is
   begin
      return new Instance'
        (Tag        => Ada.Strings.Unbounded.To_Unbounded_String (Tag),
         Production => Agora.Commodity.Reference (Production),
         Process    => Agora.Process.Reference (Process));
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This      : Any_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
      Context : Agora.Process.Context_Type;
   begin
      Agora.Logging.Log
        (Tag (This),
         "executing: " & This.Process.Image);
      This.Process.Execute (Inventory, Context);
      Agora.Process.Apply (Context, Inventory);
   end Execute;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Reference is
   begin
      for Profession of Profession_Vector loop
         if Agora.Profession.Tag (Profession.all) = Tag then
            return Profession;
         end if;
      end loop;
      raise Constraint_Error with
        "no such profession: " & Tag;
   end Get;

   --------------------
   -- Ideal_Quantity --
   --------------------

   function Ideal_Quantity
     (This      : Any_Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Quantity_Type
   is
      use all type Agora.Process.Commodity_Usage_Class;
      Consumed : constant Agora.Process.Commodity_Usage :=
                   This.Process.Consumption (Commodity);
   begin
      case Consumed.Class is
         when Not_Used =>
            return 0.0;
         when Chance_Of_Use =>
            return Consumed.Quantity;
         when Use_Quantity =>
            return Consumed.Quantity * 3.0;
         when Use_All =>
            return 5.0;
      end case;
   end Ideal_Quantity;

   ---------------------
   -- Save_Profession --
   ---------------------

   procedure Save_Profession
     (Profession : not null access constant Any_Instance)
   is
   begin
      Profession_Vector.Append (Reference (Profession));
   end Save_Profession;

   --------------------------
   -- Set_Ideal_Quantities --
   --------------------------

   procedure Set_Ideal_Quantities
     (This      : Any_Instance;
      Inventory : not null access Agora.Inventory.Any_Instance)
   is
   begin
      for Commodity of Agora.Commodity.All_Commodities loop
         Inventory.Set_Ideal_Quantity
           (Commodity, This.Ideal_Quantity (Commodity));
      end loop;
   end Set_Ideal_Quantities;

end Agora.Profession;
