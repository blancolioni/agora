with Ada.Containers.Vectors;
with WL.String_Maps;

package body Agora.Commodity is

   package Commodity_Vectors is
     new Ada.Containers.Vectors (Positive, Reference);

   package Commodity_Maps is
     new WL.String_Maps (Reference);

   Commodity_Vector : Commodity_Vectors.Vector;
   Commodity_Map    : Commodity_Maps.Map;

   ---------------------
   -- All_Commodities --
   ---------------------

   function All_Commodities return Reference_Array is
   begin
      return Result : Reference_Array (1 .. Commodity_Vector.Last_Index) do
         for I in Result'Range loop
            Result (I) := Commodity_Vector (I);
         end loop;
      end return;
   end All_Commodities;

   -------------------------------
   -- All_Commodities_Excluding --
   -------------------------------

   function All_Commodities_Excluding
     (Excluding : Reference_Array)
      return Reference_Array
   is
      Base : Reference_Array := All_Commodities;
      Last : Natural := 0;
   begin
      for Item of Base loop
         declare
            Found : Boolean := False;
         begin
            for Test of Excluding loop
               if Item = Test then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Last := Last + 1;
               Base (Last) := Item;
            end if;
         end;
      end loop;
      return Base (1 .. Last);
   end All_Commodities_Excluding;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Reference is
   begin
      pragma Assert (Commodity_Map.Contains (Tag),
                     "no such commodity: " & Tag);
      return Commodity_Map (Tag);
   end Get;

   -------------------
   -- New_Commodity --
   -------------------

   procedure New_Commodity
     (Tag           : String;
      Size          : Quantity_Type;
      Initial_Price : Price_Type)
   is
      Ref : constant Reference :=
              new Instance'
                (Tag           =>
                           Ada.Strings.Unbounded.To_Unbounded_String (Tag),
                 Size          => Size,
                 Initial_Price => Initial_Price);
   begin
      Commodity_Vector.Append (Ref);
      Commodity_Map.Insert (Tag, Ref);
   end New_Commodity;

end Agora.Commodity;
