-- =====================================================================================================================
--
--  File:       BRBON.ads
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
-- This is the top level package for an Ada API to a BRBON implementation.
--
-- Change the endianness in the BRBON.Configure to match your system. Then use any of the following APIs:
--
-- Use BRBON.Container for low-level endianess aware storage.
-- Use BRBON.Static_Unprotected for a read-only and/or single-pass write of a BRBON formatted memory image.
-- Use BRBON.Static_Protected for an Ada-like read/write BRBON formatted memory image.
-- Use BRBON.Dynamic for path-based, DB-like, read/write access to a BRBON formatted memory image.
-- Use BRBON.Thread_Safe see Dynamic, but with a thread safe API.
--
-- Note that the above packages are implemented in sequence and only as necessary for our purposes. They represent
-- the implementation traject and not all may be complete (or even started) when you read this.
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
--  Contains exceptions that can be raised to the API user.
--
--  ===============================================================================================
--  Note: BRBON_Ada is developped on an as-needed base, the following API levels are the foreseen
--  implementation path. Missing levels are simply not implemented yet.
--  ===============================================================================================
--
--  To use BRBON, make necessary changes to BRON.Configure and then use any of the following API's:
--
--  - BRBON.Container For low level byte based access to a memory area. A container is endianness
--    (byte-order) aware, thus all multi-byte accesses will return the data in the proper endianess.
--
--    This level may be usefull to anybody who needs to read/write file data. All formatting must
--    be done by the API user.
--
--  - BRBON.Static_Unprotected For fastest possible access using the type structure defined by the
--    BRBON standard. However the structure cannot be modified in size after it was created and the
--    API user must make sure no to exceed the byte-counts used during creation.
--
--    This level may be usefull for read-only access to predefined (received) data sets (files).
--    Note that there are no protections in place to prevent type mismatches.
--
--  - BRBON.Static_Protected This is the most ADA-like interface. The BRBON structure is generated
--    once, and can be updated afterwards, but it is not possible to change the byte-count's of the
--    items that were generated during creation. Exceptions will be raised if the API user violates
--    an earlier creation.
--
--  - BRBON.Dynamic This is akin to a database interface, data can be read and written and byte-counts
--    will be changed on the fly as needed. The disadvantage is that data may need to be moved around
--    which can lead to longer access times.
--
--    Does not support multi-tasking.
--
--  - BRBON.Thread_Safe This adds protective mechanisms to the Dynamic interface to support multi-tasking.
--
-- Note: If a package is absent, it has not been implemented yet ;-)
--
package BRBON is


   -- Raised when a name has an unexpected or illegal value.
   --
   Name_Error: exception;


   -- Raised when an attempt is made to create an illegal block type
   --
   Illegal_Block_Type: exception;


   -- Raised when somthing does not fit in its alotted space.
   --
   Storage_Warning: exception;


   -- Raised when a BRBON structure contains an illegal type pattern.
   --
   Illegal_Item_Type: exception;


   -- Raised when an attempt is made to execute an incompletely coded routine
   --
   Implementation: exception;


   -- Raised when an illegal byte count is specified
   --
   Byte_Count_Error: exception;


   -- Raised when something (see associated message) goes wrong in an array
   --
   Array_Error: exception;

end BRBON;
