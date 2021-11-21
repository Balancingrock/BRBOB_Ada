-- =====================================================================================================================
--
--  File:       brbon-block-header-single_item_file.ads
--  Project:    BRBON_Ada
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
-- Creates a type 1 BRBON block.
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
--  Creates a BRBON Single Item File block header.
--
--  @description
--  Contains a factory method that can be used to create the header.
--
with Interfaces; use Interfaces;

--with BRBON.Types; use BRBON.Types;


package BRBON.Header.Single_Item_File is


   -- Create a single-file-item block header in the given memory area with the given endianness.
   -- @param Memory_Area_Ptr The memory area where the block header should be constructed & maintained. The least
   -- significant 5 bits of the pointer must be zero's. All the 32 bit words in the memory area will be used as storage
   -- area.
   -- @param Using_Endianness The endianness to be used in the header (and therefore the entire block).
   -- @return A Block_Header of which the storage area is filled with a header of type 1.
   --
   procedure Create
      (
       In_Container: in out BRBON.Container.Instance;
       Header_Byte_Count: Unsigned_16;
       Options: Block_Options;
       Origin: String;
       Identifier: String;
       Extension: String;
       Path_Prefix: String;
       Acquisition_URL: String;
       Target_List: String;
       Public_Key_URL: String;
       Creation_Timestamp: Unsigned_64;
       Expiry_Timestamp: Unsigned_64
      );


end BRBON.Header.Single_Item_File;
