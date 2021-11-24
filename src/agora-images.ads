with WL.Generic_Real_Images;

package Agora.Images is

   function Image (X : Real) return String;
   function Image (X : Money_Type) return String;
   function Image (X : Price_Delta_Type) return String;
   function Image (X : Quantity_Type) return String;

private

   package Img is
     new WL.Generic_Real_Images (Real);

   function Image (X : Real) return String renames Img.Approximate_Image;
   function Image (X : Money_Type) return String
   is ("$" & Image (Real (X)));

   function Image (X : Price_Delta_Type) return String
   is ("$" & Image (Real (X)));

   function Image (X : Quantity_Type) return String
   is (Image (Real (X)));

end Agora.Images;
