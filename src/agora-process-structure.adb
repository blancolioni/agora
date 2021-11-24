with Ada.Strings.Unbounded;

package body Agora.Process.Structure is

   -----------------
   -- Consumption --
   -----------------

   overriding function Consumption
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is
      function Usage (List : Reference_Lists.List) return Commodity_Usage;

      -----------
      -- Usage --
      -----------

      function Usage (List : Reference_Lists.List) return Commodity_Usage is
      begin
         return Result : Commodity_Usage := (Not_Used, 0.0, 1.0) do
            for Item of List loop
               Result := Join (Result, Item.Consumption (Commodity));
            end loop;
         end return;
      end Usage;

   begin
      return Join (Join (Usage (This.Condition), Usage (This.Success)),
                   Usage (This.Failure));
   end Consumption;

   ------------
   -- Create --
   ------------

   function Create
     (Condition, Success, Failure : Reference_Array) return Reference
   is
      function To_List (Arr : Reference_Array) return Reference_Lists.List;

      -------------
      -- To_List --
      -------------

      function To_List (Arr : Reference_Array) return Reference_Lists.List is
      begin
         return Result : Reference_Lists.List do
            for Item of Arr loop
               Result.Append (Item);
            end loop;
         end return;
      end To_List;

   begin
      return new Instance'
        (Condition => To_List (Condition),
         Success   => To_List (Success),
         Failure   => To_List (Failure));
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This      :        Instance;
      Inventory :        not null access constant Agora.Inventory.Any_Instance;
      Context   : in out Context_Type)
   is
      procedure Run (List : Reference_Lists.List);

      ---------
      -- Run --
      ---------

      procedure Run (List : Reference_Lists.List) is
      begin
         for Item of List loop
            Item.Execute (Inventory, Context);
         end loop;
      end Run;

   begin
      Context.This_Flag := True;
      Run (This.Condition);
      if Context.This_Flag then
         Run (This.Success);
      else
         Run (This.Failure);
      end if;
   end Execute;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This : Instance)
      return String
   is
      function List_Image
        (List : Reference_Lists.List;
         Separator : String)
         return String;

      ----------------
      -- List_Image --
      ----------------

      function List_Image
        (List      : Reference_Lists.List;
         Separator : String)
         return String
      is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;

      begin
         for Element of List loop
            if Result /= "" then
               Result := Result & " " & Separator & " ";
            end if;
            Result := Result & Element.Image;
         end loop;
         return To_String (Result);
      end List_Image;

   begin
      if not This.Condition.Is_Empty then
         return "(if " & List_Image (This.Condition, "and") & " then "
           & List_Image (This.Success, ";")
           & (if not This.Failure.Is_Empty
              then " else " & List_Image (This.Failure, ";") else "")
           & ")";
      else
         return "(" & List_Image (This.Success, ";") & ")";
      end if;
   end Image;

   ----------------
   -- Production --
   ----------------

   overriding function Production
     (This      : Instance;
      Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Commodity_Usage
   is
      function Usage (List : Reference_Lists.List) return Commodity_Usage;

      -----------
      -- Usage --
      -----------

      function Usage (List : Reference_Lists.List) return Commodity_Usage is
      begin
         return Result : Commodity_Usage := (Not_Used, 0.0, 1.0) do
            for Item of List loop
               Result := Join (Result, Item.Production (Commodity));
            end loop;
         end return;
      end Usage;

   begin
      return Join (Join (Usage (This.Condition), Usage (This.Success)),
                   Usage (This.Failure));
   end Production;

end Agora.Process.Structure;
