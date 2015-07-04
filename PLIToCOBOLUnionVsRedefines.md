﻿#summary Mapping PL/I UNION to COBOL REDEFINES.
#labels Phase-Requirements

# Introduction #

Mapping PL/I UNION types to COBOL REDEFINES raises a number of issues that are discussed here.

As often, PL/I is more flexible than COBOL, which means certain tricks must be applied to get the COBOL structure to map the PL/I one. Unfortunately, there are also cases where mapping is impossible.

This study is based on Enterprise COBOL for z/OS 3.3.0 and Enterprise PL/I for z/OS 3.3.0.

# Differences between PL/I UNION and COBOL REDEFINES #

## UNIONS are named ##

A PL/I UNION has a name (or an asterisk). COBOL does not identify the union per se. In COBOL, there is a structure or an elementary data type that is redefined by other structures or elementary data types (redefining items).

The loss of information can be avoided by using a parent COBOL structure name. For instance:

```
dcl 1 a union,
      2 b fixed binary(31),
      2 c fixed binary(15);
```

might translate to:
```
       01 a.
           02 b PIC S9(8) COMP.
           02 c REDEFINES b PIC S9(4) COMP.
```

## In COBOL, neither the redefined data item nor any of its subordinate entries can contain a VALUE clause. ##

Actually Enterprise COBOL for z/OS 3.3.0 treats a VALUE as a comment in such a case. No errors or warnings are generated.

Therefore we can ignore this restriction.

## In COBOL, the data description entry for the redefined item cannot contain an OCCURS clause ##

Assuming a valid PL/I union such as:
```
dcl 1 a union,                       
    2 b(2) fixed binary(15),
    2 c fixed binary(31);
```
The seemingly equivalent COBOL structure:
```
        01 a.                               
            02 b OCCURS 2 PIC S9(4) COMP.        
            02 c REDEFINES b PIC S9(8) COMP.
```

does not compile with COBOL 3.3.0. Compilation error is: IGYDS1152-S   A definition of an object of a "REDEFINES" clause contained an "OCCURS" clause.

According to documentation, this restriction has been removed in Enterprise COBOL for z/OS 3.4.0 but now applies to redefining items.

We could attempt to translate the PL/I union to a slightly different COBOL structure though:
```
        01 a.                               
            02 b-redef.        
               03 b OCCURS 2 PIC S9(4) COMP.        
            02 c REDEFINES b-redef PIC S9(8) COMP.
```

This one compiles fine with COBOL 3.3.0. It could be used to map the PL/I structure but has a different structure morphology (it is now 3 levels deep and artificial items with no PL/I match are created).

## In COBOL, neither redefined nor redefining items can contain an OCCURS DEPENDING ON clause. ##

Assuming this PL/I structure:
```
     dcl 1 a based,                
         2 b fixed binary(31),     
         2 c union,                
           3 d,
             4 e(2 REFER(b)) char(1),
           3 f fixed binary(15);   
```

A naive translation like so:
```
         01 a.                                          
            02 b PIC S9(8) COMP.                             
            02 c.                                       
               03 d.
                  04 e PIC X OCCURS 2 DEPENDING ON b. 
               03 f REDEFINES d PIC S9(4) COMP.            
```
yields this compilation error: IGYDS1170-S   A "REDEFINES" clause object "D" was a variable-length item.

There does not seem to be a way around this restriction so this is a case where there is no COBOL equivalent to the PL/I structure at all.

## In COBOL, if the redefined data item has level-number other than 01, its size must be greater than or equal to the size of the redefining data item ##

This is again a restriction that seems to have been lifted in COBOL 3.4.0.

Typically:
```
dcl 1 a union,  
    2 b char(1),
    2 c char(2);
```
if translated to:
```
        01 a.                         
            02 b PIC X(1).            
            02 c REDEFINES b PIC X(2).
```
will not compile with COBOL 3.3.0. Error is: IGYDS1154-S   "C" redefined a smaller item.

Knowing that PL/I compiler will allocate the storage required by the largest member of the union, a close COBOL structure that compiles would be:
```
       01 a.                         
           02 b-redef.                     
               03 b PIC X(1).   
               03 FILLER PIC X(1).   
           02 c REDEFINES b-redef PIC X(2).
```
Again this is a case where we can find an alternate COBOL structure that maps the PL/I structure but with a slight morphology change.

# Conclusion #

Not all PL/I unions can be mapped to COBOL REDEFINES.

The PL/I to COBOL structure converter should issue a warning in these situations. We should still generate COBOL code, even if it does not compile, knowing that more recent compilers, such as 3.4.0, might be more permissive and if not, then there is not a whole lot we can do.