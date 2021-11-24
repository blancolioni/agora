package Agora is

   Minimum_Price : constant := 0.01;

   type Real is new Float range Float'First .. Float'Last;

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   type Money_Type is new Real range Real'First .. Real'Last;
   type Price_Delta_Type is new Money_Type;
   subtype Price_Type is
     Price_Delta_Type range Minimum_Price .. Price_Delta_Type'Last;

   function "*"
     (Left : Real; Right : Price_Delta_Type)
      return Price_Delta_Type;

   type Price_Model_Type is
      record
         Low, High : Price_Type;
      end record;

   type Quantity_Type is new Real range 0.0 .. Real'Last;

   function Round (Quantity : Quantity_Type) return Quantity_Type
   is (Quantity_Type (Natural (Quantity * Quantity_Type'(1024.0))) / 1024.0);

   function "*"
     (Left : Real; Right : Quantity_Type)
      return Quantity_Type;

   function "*"
     (Left : Quantity_Type; Right : Price_Type)
      return Money_Type;

   function "/"
     (Left : Money_Type; Right : Quantity_Type)
      return Price_Type;

   type Agent_Id is new Positive;

   type Action_Type is (Ask, Bid);

private

   pragma Import (Intrinsic, "*");
   pragma Import (Intrinsic, "/");

end Agora;
