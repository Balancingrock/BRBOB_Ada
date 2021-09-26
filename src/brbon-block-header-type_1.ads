-- =====================================================================================================================
--
--  File:       brbon-block-header-type_1.ads
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
--  Creates a BRBON type 1 block header.
--
--  @description
--  Contains a factory method that can be used to create type 1 BRBON block headers.
--
with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Block.Header.Type_1 is

   Block_Header_Type_1_Byte_Count: Unsigned_32 := 70;


   -- Create a type 1 block header in the given memory area with the given endianness.
   -- @param Memory_Area_Ptr The memory area where the block header should be constructed & maintained. The least
   -- significant 5 bits of the pointer must be zero's. All the 32 bit words in the memory area will be used as storage
   -- area.
   -- @param Using_Endianness The endianness to be used in the header (and therefore the entire block).
   -- @return A Block_Header of which the storage area is filled with a header of type 1.
   --
   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header;

   --
   function Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header renames Block_Header_Type_1_Factory;

end BRBON.Block.Header.Type_1;
