#summary Aligned/Unaligned data items handling.
#labels Phase-Requirements

# Introduction #

While it is relatively straightforward to map elementary data items from PL/I to COBOL, it is harder to map structures and unions.

One major issue is that PL/I and COBOL compilers do not handle data alignment for structures in the same way.

A PL/I and a COBOL structure are compatible when data created using one structure can be consumed by the other. In other words, each data item in memory must match a compatible PL/I or COBOL item description.

# PL/I and COBOL handling of in-memory alignement #

In both COBOL and PL/I, data items have alignment requirements which vary depending on data type and alignment attributes (ALIGNED/UNALIGNED for PL/I, SYNCHRONIZED for COBOL).

The following table shows that alignment requirements for elementary data items are generally identical between PL/I and COBOL.

| **PL/I data type**                                    | **Storage (Bytes)**      | **ALIGNED**   | **UNALIGNED** | **COBOL data type**      | **SYNCHRONIZED** |
|:------------------------------------------------------|:-------------------------|:--------------|:--------------|:-------------------------|:-----------------|
| BIT(n)                                                | n bits                   | Byte          | Bit           | N/A                      | N/A              |
| CHAR(n)                                               | n                        | Byte          | Byte          | PIC X(n)                 | Byte             |
| CHAR(n) VARZ                                          | n + 1                    | Byte          | Byte          | N/A                      | N/A              |
| G(n)                                                  | 2n                       | Byte          | Byte          | PIC G(n) DISPLAY-1       | Byte             |
| G(n) VARZ                                             | 2n + 2                   | Byte          | Byte          | N/A                      | N/A              |
| WCHAR(n)                                              | 2n                       | Byte          | Byte          | PIC N(n)                 | Byte             |
| WCHAR(n) VARZ                                         | 2n + 2                   | Byte          | Byte          | N/A                      | N/A              |
| PICTURE                                               | Number of PIC chars(**)**| Byte          | Byte          | PIC '...'                | Byte             |
| DEC FIXED(p,q)                                        | CEIL((p+1)/2)            | Byte          | Byte          | PIC S9(p-q)V9(q) COMP-3  | Byte             |
| BIN FIXED(p,q) [&& 1<=p<=7](SIGNED.md) or [1<=p<=8]    | 1                        | Byte          | Byte          | N/A                      | N/A              |
| BIT(n) VAR                                            | 2 bytes + n bits         | Halfword      | Byte          | N/A                      | N/A              |
| CHAR(n) VAR                                           | n + 2                    | Halfword      | Byte          | N/A                      | N/A              |
| G(n) VAR                                              | 2n + 2                   | Halfword      | Byte          | N/A                      | N/A              |
| WCHAR(n) VAR                                          | 2n + 2                   | Halfword      | Byte          | N/A                      | N/A              |
| BIN FIXED(p,q) [&& 8<=p<=15](SIGNED.md) or [9<=p<=16]  | 2                        | Halfword      | Byte          | PIC S9(4) COMP-5         | Halfword         |
| BIN FIXED(p,q) [&& 16<=p<=31](SIGNED.md) or [17<=p<=32]| 4                        | Fullword      | Byte          | PIC S9(9) COMP-5         | Fullword         |
| BIN FLOAT(p) [1<=p<=21]                               | 4                        | Fullword      | Byte          | COMP-1                   | Fullword         |
| DEC FLOAT(p) [1<=p<=6] or [1<=p<=7 && DFP]            | 4                        | Fullword      | Byte          | COMP-1                   | Fullword         |
| BIN FIXED(p,q) [&& 32<=p<=63](SIGNED.md) or [33<=p<=64]| 8                        | Doubleword    | Byte          | PIC S9(18) COMP-5        | Fullword         |
| BIN FLOAT(p) [22<=p<=53]                              | 8                        | Doubleword    | Byte          | COMP-2                   | Doubleword       |
| DEC FLOAT(p) [8<=p<=16] or [7<=p<=16 && DFP]          | 8                        | Doubleword    | Byte          | COMP-2                   | Doubleword       |
| BIN FLOAT(p) [54<=p]                                  | 16                       | Doubleword    | Byte          | N/A                      | N/A              |
| DEC FLOAT(p) [17<=p]                                  | 16                       | Doubleword    | Byte          | N/A                      | N/A              |

(**) Other than V, K and F**

When data is laid out by compilers, the alignment requirements might result in data items being shifted (rather than starting immediatly after the previous item) in order to start on the required boundary (byte, halfword, fullword or doubleword).

This shifting is identical to adding padding bytes, or bits, after the previous data item.

Although elementary data items might have the same alignment requirements, alignment strategies applied by the PL/I and COBOL compilers for structures differ. data laid out by one compiler might not be enterpreted correctly by the other (see z/OS Language Environment Writing Interlanguage Communication Applications http://publibz.boulder.ibm.com/epubs/pdf/ceea4160.pdf for details).

## Default alignment attribute values ##

For PL/I, UNALIGNED is the default for bit data, character data, graphic data, widechar data and numeric character data. ALIGNED is the default for all other types of data. This is important because it means some widely used data types, such as FIXED BINARY, are ALIGNED by default.

For COBOL, all data types are not SYNCHRONIZED by default.

## Alignment attribute value inheritance ##

In PL/I, ALIGNED/UNALIGNED attributes are inherited from a parent in a structure. Childs can override the parent aligment by explicitly setting the attribute.

In COBOL, SYNCHRONIZED can be inherited from a parent but only at the 01 level. Child items have no way to override the SYNCHRONIZED attribute.

## Structures mapping ##

This is probably where the most differences exist between PL/I and COBOL alignment handling.

### PL/I structure mapping strategy ###

PL/I has a sophisticated strategy for structures mapping. The objective is to minimize the number of padding characters.

The algorithm is described in Enterprise PL/I for z/OS Language Reference http://publibfp.boulder.ibm.com/epubs/pdf/ibm3lr70.pdf under **Structure and union mapping**.

The PL/I structure mapping algorithm applied to:

```
dcl 1 A,                        
    3 B char(1),                 
    3 C fixed bin(15),       
    3 D,                       
       4 E char(1),            
       4 F fixed bin(31);     
```

Produces the following memory layout:

```
|               |               | Doubleword boundary
|       |       |       |       | Fullword boundary
|   |   |   |   |   |   |   |   | Halfword boundary
| | | | | | | | | | | | | | | | | Byte boundary
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
 - - -                            <-- The hang
       B                          <-- Byte aligned
         C C                      <-- Halfword aligned
            -                     <-- Padding byte
              E                   <-- Byte aligned
                 F F F F          <-- Fullword aligned
```

The memory occupation is only 9 bytes but the structure does not start on a doubleword boundary. It is actually shifted 3 bytes in order to satisfy the internal alignment requirements. These 3 bytes initial offset are sometimes referred to as "the hang".

### COBOL structure mapping strategy ###

COBOL does not attempt to optimize the padding needed to satisfy the alignment requirements.

All structures start on a doubleword boundary.

Then each item from the structure is taken in turn (in the order they appear in the structure) and the item is shifted, if necessary, by as many bytes as needed to satisfy the alignement requirement.

Applying this strategy to structure:
```
        01 A.                        
            03 B PIC X(1).                
            03 C PIC S9(4) COMP SYNC.          
            03 D.                         
               04 E PIC X(1).             
               04 F PIC S9(9) COMP SYNC.     
```

Produces the following memory layout:

```
|               |               | Doubleword boundary
|       |       |       |       | Fullword boundary
|   |   |   |   |   |   |   |   | Halfword boundary
| | | | | | | | | | | | | | | | | Byte boundary
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
 B                                <-- Byte aligned
   -                              <-- Padding byte
     C C                          <-- Halfword aligned
         E                        <-- Byte aligned
           - - -                  <-- Padding bytes
                 F F F F          <-- Fullword aligned
```

The result is 12 bytes of storage used with 4 padding bytes and 8 useful bytes.

# From PL/I to COBOL #

From the previous paragraphs it should now be clear that a PL/I and COBOL structures, even with the exact same number, order and types of data items, are not necessarily compatible.

If you can afford the luxury of modifying the PL/I structure, then it should be noted that it is relatively straightforward to get it to be compatible. For instance this structure will not have any hidden padding bytes to worry about:

```
dcl 1 A UNALIGNED,                        
    3 B char(1),                
    3 C fixed bin(15)       
    3 D,                      
       4 E char(1),           
       4 F fixed bin(31);     
```

Another popular technique is to start PL/I structures with a dummy doubleword aligned data item. This forces the structure to start on a doublewordword boundary and avoids "the hang".

The rest of this discussion is for those who can't change the PL/I structure.

## The hang issue ##

When a PL/I and a COBOL program share a pointer to a memory location, the presence of the hang is a potential issue. COBOL will assume data starts on the first byte at the memory address while PL/I assumes data starts after the hang.

In such a situation, the only solution is for the COBOL structure to start with a dummy FILLER with the exact same size as the hang.

When PL/I and COBOL communicate indirectly, for instance by sharing a file record, a database segment, or using MQ messages, the hang is not an issue because the various PL/I APIs used to write into files, MQ messages and so forth will not store the hang bytes as part of the data.

## The alignment padding issue ##

The hidden padding bytes that are added to a structure in order to satisfy the alignment requirements are part of the actual data.

This means that even when data is exchanged via APIs, there will be an issue if one of the languages is not aware of the hidden padding bytes presence.

Back to the first PL/I structure shown in this document, a compatible COBOL structure would be:

```
        01 A.                        
            03 B PIC X(1).                
            03 C PIC S9(4) COMP.
            03 FILLER PIC X.        
            03 D.                         
               04 E PIC X(1).             
               04 F PIC S9(9) COMP.     
```

This will produce this memory layout:

```
|               |               | Doubleword boundary
|       |       |       |       | Fullword boundary
|   |   |   |   |   |   |   |   | Halfword boundary
| | | | | | | | | | | | | | | | | Byte boundary
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
 B                                <-- Byte aligned
   C C                            <-- Byte aligned
       -                          <-- Padding byte
         E                        <-- Byte aligned
           F F F F                <-- Byte aligned
```

Note that the COBOL SYNCHRONIZED clause does not help here. We need to explicitly add a FILLER where PL/I inserts padding bytes.

# Conclusion #

When exchanging structured data with existing PL/I programs, it is important to be aware of the potential alignment issues.

Although PL/I programmers were often aware of these issues and structured their data in order to avoid them, there are still probably a large number of situations where PL/I programs were only exchanging data with other PL/I programs and are only now being opened to other languages.