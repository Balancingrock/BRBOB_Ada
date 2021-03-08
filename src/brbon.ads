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
--  And because I need to make a living:
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
-- Use BRBON.Thread_Safe for the as Dynamic, but with a thread safe API.
--
-- Note that the above packages are implemented in sequence and only as necessary for our purposes. They represent
-- the implementation traject and not all may be complete when you read this.
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
--    This level may be usefull for read-only access to predefined (received) data sets (files). Or
--    for situations where a data structure is generated in a single pass without the need for
--    changes afterwards.
--
--  - BRBON.Static_Protected This is the most ADA-like interface. The BRBON structure is generated
--    once, and can be updated afterwards, but it is not possible to change the byte-count's of the
--    items that were generated during creation.
--
--  - BRBON.Dynamic This is akin to a database interface, data can be read and written and byte-counts
--    will be changed on the fly as needed. The disadvantage is that data may need to be moved around
--    which can lead to longer access times.
--
--    Does not support multi-tasking.
--
--  - BRBON.Thread_Safe This adds protective mechanisms to the Dynamic interface to support multi-tasking.
--
package BRBON is

   -- Possible exceptions
   -- ===================

   -- Raised when an operation is attempted on the wrong type
   --
   Type_Conflict: exception;


   -- Raised when the storage area runs out of space.
   --
   Storage_Error: exception;

   -- Raised when a BRBON structure contains an illegal type pattern.
   --
   Illegal_Item_Type: exception;

   -- Raised when a string to Item_Name conversion failed.
   --
   Item_Name_Too_Long: exception;

   -- Raised when an attempt is made to execute an incompletely coded routine
   --
   Incomplete_Code: exception;

   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.
   --
   Enum_Mapping_Failed: exception;

   -- Raised when an attempt is made to open a file larger than Unsigned_32'Last bytes.
   --
   File_Too_Large: exception;

end BRBON;
