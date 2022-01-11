-- =====================================================================================================================
--
--  Package:    BRBON.Portal_Package
--  Project:    BRBON_Ada
--  File:       brbon-portal_package.ads
--
--  Version:    0.1.0
--
--  Author:     Marinus van der Lugt
--  Company:    http://balancingrock.nl
--  Repository: https://github.com/Balancingrock/BRBON_Ada
--
--  Copyright:  (c) 2021 Marinus van der Lugt, All rights reserved.
--
--  License:    MIT, see LICENSE file
--
--  And because I too need to make a living:
--
--   - You can send payment (you choose the amount) via paypal to: sales@balancingrock.nl
--   - Or wire bitcoins to: 1GacSREBxPy1yskLMc9de2nofNv2SNdwqH
--
--  If you prefer to pay in another way, please contact me at rien@balancingrock.nl
--
--  Prices/Quotes for support, modifications or enhancements can also be obtained from: rien@balancingrock.nl
--
-- =====================================================================================================================
-- Purpose
--
-- A portal is an access mechanism for the API user to directly access information stored in an item. A portal is
-- obtained using a look-up or search operation and can thereafter be used as a stand-in for the item.
-- A portal has a few API functions itself which are exposed here.
-- The portal type is defined in the file brbon.ads.
--
-- =====================================================================================================================
-- History
--
-- 0.1.0 - Initial version
--
-- =====================================================================================================================
-- Gnatdoc
--
--  @summary
--  Top level package.
--
--  @description
--  Contains configuration, exceptions and top level type definitions.


with Interfaces; use Interfaces;


with BRBON; use BRBON;


package BRBON.Portal_Package is


   -- This exception is raised when an invalid portal is used.
   --
   Invalid_Portal_Error: exception;


   -- This exception is raised when a null portal is used for anything else but the null-portal test.
   --
   Null_Portal_Error: exception;


   -- A portal can be used to access items in a block without needing the seach/lookup mechanism provided by the block managers
   --
   type Portal is tagged private;


   -- Returns true if the portal is valid. Once invalid, a portal will never become valid again.
   -- Once a portal is invalid, any attempt to access values in the item will raise the excpetion Invalid_Portal_Error.
   -- Note: Only dynamic block functions can invalidate a portal.
   --
   function Is_Valid_Portal (P: Portal) return Boolean;


   -- The null portal may be returned if a (look-up or search) operation does not produce a result.
   -- A null-portal is always invalid and will raise the Null_Portal_Error when tried.
   -- However any portal (including invalid portals) can be tested for beiing a null portal.
   --
   -- If a portal identifies as a null portal, it can be deleted as it is not usable for anything else
   -- but finding out that it is a null portal.
   --
   function Is_Null_Portal (P: Portal) return Boolean;


private

   -- The different types of portal
   --
   type Portal_Type is (Null_Portal, Normal, Element, Field);


   -- The portal type
   --
   type Portal is tagged
      record
         Is_Type: Portal_Type;
         Is_Valid: Boolean := True;
         --
         Item_Ptr: Item_Header_Ptr;
         Element_Index: Unsigned_32 := 0;
         Column_Index: Unsigned_32 := 0;
      end record;


end BRBON.Portal_Package;
