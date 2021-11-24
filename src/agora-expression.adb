with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Agora.Images;

package body Agora.Expression is

   type Constant_Instance is new Instance with
      record
         Quantity : Quantity_Type;
      end record;

   overriding function Evaluate
     (This      : Constant_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance)
      return Quantity_Type
   is (This.Quantity);

   overriding function Image
     (This      : Constant_Instance)
      return String
   is (Agora.Images.Image (This.Quantity));

   type Inventory_Instance is new Instance with
      record
         Commodity : Agora.Commodity.Reference;
      end record;

   overriding function Evaluate
     (This      : Inventory_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance)
      return Quantity_Type
   is (Inventory.Quantity (This.Commodity));

   overriding function Image
     (This      : Inventory_Instance)
      return String
   is (This.Commodity.Tag);

   type Quantity_Array is array (Positive range <>) of Quantity_Type;

   type Function_Evaluator is access
     function (Arguments : Quantity_Array) return Quantity_Type;

   package Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Reference);

   type Function_Instance is new Instance with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Fn        : Function_Evaluator;
         Arguments : Reference_Vectors.Vector;
      end record;

   overriding function Evaluate
     (This      : Function_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance)
      return Quantity_Type;

   overriding function Image
     (This      : Function_Instance)
      return String;

   package Evaluator_Maps is
     new WL.String_Maps (Function_Evaluator);

   Evaluator_Map : Evaluator_Maps.Map;

   function Evaluate_Clamp
     (Arguments : Quantity_Array)
      return Quantity_Type;

   function Evaluate_Min
     (Arguments : Quantity_Array)
      return Quantity_Type;

   function Evaluate_Multiply
     (Arguments : Quantity_Array)
      return Quantity_Type;

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Reference) return Reference is
   begin
      return Function_Expression ("*", (Left, Right));
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Reference; Right : Quantity_Type) return Reference is
   begin
      return Left * Constant_Expression (Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Quantity_Type; Right : Reference) return Reference is
   begin
      return Constant_Expression (Left) * Right;
   end "*";

   -----------
   -- Clamp --
   -----------

   function Clamp (Left, Right : Reference) return Reference is
   begin
      return Function_Expression ("clamp", (Left, Right));
   end Clamp;

   -------------------------
   -- Constant_Expression --
   -------------------------

   function Constant_Expression (Value : Quantity_Type) return Reference is
   begin
      return new Constant_Instance'
        (Quantity => Value);
   end Constant_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Function_Instance;
      Inventory : not null access constant Agora.Inventory.Any_Instance)
      return Quantity_Type
   is
      Arguments : Quantity_Array (1 .. This.Arguments.Last_Index);
   begin
      for I in Arguments'Range loop
         Arguments (I) := This.Arguments.Element (I).Evaluate (Inventory);
      end loop;
      return This.Fn (Arguments);
   end Evaluate;

   --------------------
   -- Evaluate_Clamp --
   --------------------

   function Evaluate_Clamp
     (Arguments : Quantity_Array)
      return Quantity_Type
   is
      pragma Assert (Arguments'Length in 1 .. 3,
                     "clamp must have 1, 2 or 3 arguments");
      Min : constant Quantity_Type :=
              (if Arguments'Length = 3 then Arguments (Arguments'Last)
               else 0.0);
      Max : constant Quantity_Type :=
              (if Arguments'Length > 1
               then Arguments (Arguments'First + 1)
               else 1.0);
      Value : constant Quantity_Type := Arguments (Arguments'First);
   begin
      return (if Value <= Min then Min
              elsif Value >= Max then Max
              else Value);
   end Evaluate_Clamp;

   ------------------
   -- Evaluate_Min --
   ------------------

   function Evaluate_Min
     (Arguments : Quantity_Array)
      return Quantity_Type
   is
      pragma Assert (Arguments'Length >= 1,
                     "min requires at least one argument");
      Result : Quantity_Type := Quantity_Type'Last;
   begin
      for Arg of Arguments loop
         Result := Quantity_Type'Min (Result, Arg);
      end loop;
      return Result;
   end Evaluate_Min;

   -----------------------
   -- Evaluate_Multiply --
   -----------------------

   function Evaluate_Multiply
     (Arguments : Quantity_Array)
      return Quantity_Type
   is
      Result : Quantity_Type := 1.0;
   begin
      for Arg of Arguments loop
         Result := Result * Arg;
      end loop;
      return Result;
   end Evaluate_Multiply;

   -------------------------
   -- Function_Expression --
   -------------------------

   function Function_Expression
     (Name      : String;
      Arguments : Reference_Array)
      return Reference
   is
      Vec : Reference_Vectors.Vector;
   begin
      pragma Assert (Evaluator_Map.Contains (Name),
                     "no such function: " & Name);

      for Arg of Arguments loop
         Vec.Append (Arg);
      end loop;

      return new Function_Instance'
        (Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Fn        => Evaluator_Map (Name),
         Arguments => Vec);
   end Function_Expression;

   -----------
   -- Image --
   -----------

   overriding function Image
     (This      : Function_Instance)
      return String
   is
      function Argument_Image (Start : Positive) return String;

      --------------------
      -- Argument_Image --
      --------------------

      function Argument_Image (Start : Positive) return String is
         Current_Image : constant String :=
                           This.Arguments.Element (Start).Image;
      begin
         return (if Start = 1 then "" else ",")
           & Current_Image
           & (if Start < This.Arguments.Last_Index
              then Argument_Image (Start + 1)
              else "");
      end Argument_Image;

   begin
      return Ada.Strings.Unbounded.To_String (This.Name)
        & (if This.Arguments.Is_Empty
           then ""
           else "(" & Argument_Image (1) & ")");
   end Image;

   --------------------------
   -- Inventory_Expression --
   --------------------------

   function Inventory_Expression
     (Commodity : not null access constant Agora.Commodity.Any_Instance)
      return Reference
   is
   begin
      return new Inventory_Instance'
        (Commodity => Agora.Commodity.Reference (Commodity));
   end Inventory_Expression;

   ---------
   -- Min --
   ---------

   function Min (Left, Right : Reference) return Reference is
   begin
      return Function_Expression ("min", (Left, Right));
   end Min;

begin
   Evaluator_Map.Insert ("*", Evaluate_Multiply'Access);
   Evaluator_Map.Insert ("clamp", Evaluate_Clamp'Access);
   Evaluator_Map.Insert ("min", Evaluate_Min'Access);
end Agora.Expression;
