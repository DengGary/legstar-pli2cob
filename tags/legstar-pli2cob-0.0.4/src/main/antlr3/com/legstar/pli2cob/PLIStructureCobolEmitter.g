tree grammar PLIStructureCobolEmitter;
/*------------------------------------------------------------------
 *  LegStar PL/I structures grammar
 * --------------------------------
 *  Copyright (C) 2009 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
*------------------------------------------------------------------*/
/*------------------------------------------------------------------
 * COBOL emitter tree grammar
 *------------------------------------------------------------------*/

options {
    tokenVocab=PLIStructureParser;
    ASTLabelType=CommonTree;
    output=template;
    superClass=AbstractPLIStructureCobolEmitter;
}
/*------------------------------------------------------------------
 * Java overrides
 *------------------------------------------------------------------*/
@header {
package com.legstar.pli2cob;
}
@members {
    /* This will keep track of UNION structures and their association
       with their first child (the one that is REDEFINED by siblings) */
    private HashMap < String, String > unions = new  HashMap < String, String >();
  
}

pl1code
    : (di+=data_item["", "root"])*
    ->cobolFragment(dataItems={$di})
    ;

data_item[String parentIndent, String parentQualifiedName]
scope {
  /* Make the data item level and name accessible to all sub-rules */
  int level;
  String name;

  /* Qualified name is the data item name prefixed by its ancestors names */
  String qualifiedName;

  /* Number f indent characters */
  String indent;
  
  /* Allow rules down the line to inform this rule that the data item should
     not emit any code (probably due to some incompatibility between PL/I and COBOL)*/
  boolean emitStatement;
}
@init {
  /* By default, a data item level is 1 */
  $data_item::level = 1;
  
  /* By default, the data item is a filler */
  $data_item::name = "*";
  
  /* Will be non null if this item redefines the first child of a parent union. */
  String redefines;
  
  /* Normally all PL/I statement should emit an equivalent COBOL statement */
  $data_item::emitStatement = true;
  
}
    : ^(DATA_ITEM level? {$data_item::indent=formatIndent(parentIndent,$data_item::level);}
                  data_item_name {$data_item::qualifiedName = parentQualifiedName + '.' + $data_item::name;}
                  (a+=misc_attribute)*
                  (c+=data_item[$data_item::indent, $data_item::qualifiedName])*)
     {
        /* Check if our direct parent is a union */
        redefines = unions.get(parentQualifiedName);
        if (redefines != null) {
            /* If our parent is a union and is not yet bound to a child, then
               we are the first */
            if (redefines.equals("")) {
                unions.put(parentQualifiedName, formatCobolName($data_item::name));
                /* As the first child we should not redefine ourself */
                redefines = null;
            }
        }
     }
    ->{$data_item::emitStatement}? dataDescription(level={formatLevel($data_item::indent,$data_item::level)},
                      name={formatCobolName($data_item::name)},
                      redefines={redefines},
                      attributes={$a},
                      children={$c})
    ->
    ;

level
    : ^(LEVEL UNSIGNED_INTEGER)
    {$data_item::level = Integer.parseInt($UNSIGNED_INTEGER.text);}
    ;
    
data_item_name
    : ^(NAME DATA_ITEM_NAME) {$data_item::name = $DATA_ITEM_NAME.text;}
    | ^(NAME ASTERISK) {$data_item::name = $ASTERISK.text;}
    | ^(NAME DECLARE_KEYWORD) {$data_item::name = $DECLARE_KEYWORD.text;}
    | ^(NAME REAL_KEYWORD) {$data_item::name = $REAL_KEYWORD.text;}
    | ^(NAME COMPLEX_KEYWORD) {$data_item::name = $COMPLEX_KEYWORD.text;}
    | ^(NAME FIXED_KEYWORD) {$data_item::name = $FIXED_KEYWORD.text;}
    | ^(NAME FLOAT_KEYWORD) {$data_item::name = $FLOAT_KEYWORD.text;}
    | ^(NAME PRECISION_KEYWORD) {$data_item::name = $PRECISION_KEYWORD.text;}
    | ^(NAME DECIMAL_KEYWORD) {$data_item::name = $DECIMAL_KEYWORD.text;}
    | ^(NAME BINARY_KEYWORD) {$data_item::name = $BINARY_KEYWORD.text;}
    | ^(NAME BIT_KEYWORD) {$data_item::name = $BIT_KEYWORD.text;} 
    | ^(NAME SIGNED_KEYWORD) {$data_item::name = $SIGNED_KEYWORD.text;}
    | ^(NAME UNSIGNED_KEYWORD) {$data_item::name = $UNSIGNED_KEYWORD.text;}
    | ^(NAME CHARACTER_KEYWORD) {$data_item::name = $CHARACTER_KEYWORD.text;} 
    | ^(NAME WIDECHAR_KEYWORD) {$data_item::name = $WIDECHAR_KEYWORD.text;} 
    | ^(NAME GRAPHIC_KEYWORD) {$data_item::name = $GRAPHIC_KEYWORD.text;} 
    | ^(NAME REFER_KEYWORD) {$data_item::name = $REFER_KEYWORD.text;} 
    | ^(NAME NONVARYING_KEYWORD) {$data_item::name = $NONVARYING_KEYWORD.text;} 
    | ^(NAME VARYING_KEYWORD) {$data_item::name = $VARYING_KEYWORD.text;} 
    | ^(NAME VARYINGZ_KEYWORD) {$data_item::name = $VARYINGZ_KEYWORD.text;} 
    | ^(NAME PICTURE_KEYWORD) {$data_item::name = $PICTURE_KEYWORD.text;} 
    | ^(NAME DIMENSION_KEYWORD) {$data_item::name = $DIMENSION_KEYWORD.text;}
    | ^(NAME ALIGNED_KEYWORD) {$data_item::name = $ALIGNED_KEYWORD.text;}
    | ^(NAME UNALIGNED_KEYWORD) {$data_item::name = $UNALIGNED_KEYWORD.text;}
    | ^(NAME INITIAL_KEYWORD) {$data_item::name = $INITIAL_KEYWORD.text;}
    | ^(NAME AUTOMATIC_KEYWORD) {$data_item::name = $AUTOMATIC_KEYWORD.text;}
    | ^(NAME STATIC_KEYWORD) {$data_item::name = $STATIC_KEYWORD.text;}
    | ^(NAME BASED_KEYWORD) {$data_item::name = $BASED_KEYWORD.text;}
    | ^(NAME CONTROLLED_KEYWORD) {$data_item::name = $CONTROLLED_KEYWORD.text;}
    | ^(NAME UNION_KEYWORD) {$data_item::name = $UNION_KEYWORD.text;}
    ;

misc_attribute
    : string_attribute->{$string_attribute.st}
    | picture_attribute->{$picture_attribute.st}
    | arithmetic_attribute->{$arithmetic_attribute.st}
    | dimension_attribute->{$dimension_attribute.st} 
    | alignment_attribute->{$alignment_attribute.st}
    | initial_attribute->{$initial_attribute.st}
    | storage_attribute->{$storage_attribute.st}
    | union_attribute->{$union_attribute.st}
    ;

/*------------------------------------------------------------------
 * -- String attributes
*------------------------------------------------------------------*/
string_attribute
scope {
  String pl1Type;
  HashMap pl12CobolType;
  String varying;
  int stringLength;
}
@init {
  /* Helper map to translate PL/I string types to COBOL types */
  $string_attribute::pl12CobolType = new HashMap();
  $string_attribute::pl12CobolType.put("BIT", "X");
  $string_attribute::pl12CobolType.put("CHARACTER", "X");
  $string_attribute::pl12CobolType.put("GRAPHIC", "G");
  $string_attribute::pl12CobolType.put("WIDECHAR", "N");
  
  /* By default, PL/I strings have a length of 1 */
  $string_attribute::stringLength = 1;

  /* By default, PL/I strings are non varying */
  $string_attribute::varying = "NONVARYING";
}
      : ^(STRING string_type string_length_specification? varying_attribute?)
      -> stringPicture(
            stringName={$data_item::name},
            stringType={$string_attribute::pl12CobolType.get($string_attribute::pl1Type)},
            stringLength={$string_attribute::stringLength},
            stringDbcs={$string_attribute::pl1Type.equals("GRAPHIC")},
            stringVarying={$string_attribute::varying.equals("VARYING")},
            subLevel={formatLevel(formatIndent($data_item::indent, $data_item::level + 1),$data_item::level + 1)},
            dependingOn={$string_length_specification.st})
      ; 

string_type
  : BIT {$string_attribute::pl1Type="BIT";}
  | CHARACTER {$string_attribute::pl1Type="CHARACTER";}
  | GRAPHIC {$string_attribute::pl1Type="GRAPHIC";}
  | WIDECHAR {$string_attribute::pl1Type="WIDECHAR";}
  ;
  
/* This ryle updates the string length when it is explicitly set. It also returns the
   depending on clause, if any, as its production template */
string_length_specification
    : ^(LENGTH UNSIGNED_INTEGER refer_specification?)
    {
      $string_attribute::stringLength = Integer.parseInt($UNSIGNED_INTEGER.text);
      if ($string_attribute::pl1Type.equals("BIT")) {
            issueWarning("Item " + $data_item::name + ", PL/I BIT(" + $string_attribute::stringLength + ") type translated to PIC X(" + ($string_attribute::stringLength / 8) + ")");
            $string_attribute::stringLength = $string_attribute::stringLength / 8;
      }
      if ($string_attribute::stringLength == 0) {
            issueWarning("Item " + $data_item::name + ", is a zero length string which is not supported by COBOL. Statement ignored");
            $data_item::emitStatement = false;
      }
    }
    ->{$refer_specification.st}
    ;

varying_attribute
    : ^(VARYING NONVARYING) {$string_attribute::varying="NONVARYING";}
    | ^(VARYING VARYING) {$string_attribute::varying="VARYING";}
    | ^(VARYING VARYINGZ) 
      {$string_attribute::varying="VARYINGZ";
       issueWarning("Item " + $data_item::name + ", VARYINGZ has no COBOL counterpart. Translated to PIC X");
      }
    ;

/*------------------------------------------------------------------
 * -- Refer length or occurs attribute
*------------------------------------------------------------------*/
/* TODO add keywords*/
refer_specification
    : ^(REFER DATA_ITEM_NAME)
    ->dependingOn(dependingOn={formatCobolName($DATA_ITEM_NAME.text)})
    ;
    
/*------------------------------------------------------------------
 * -- Picture attribute
*------------------------------------------------------------------*/
picture_attribute
    : ^(PICTURE STRING_LITERAL)
    ->pictureValue(picture={formatCobolPicture($STRING_LITERAL.text)})
    ;

/*------------------------------------------------------------------
 * -- Arithmetic attributes
*------------------------------------------------------------------*/
arithmetic_attribute
scope {
  String scale;
  String base;
  String mode;
  boolean isSigned;
  int digits;
  int scalingFactor;
}
@init {
  /* By default, PL/I arithmetic items have a FLOAT scale  */
  $arithmetic_attribute::scale = "FLOAT";
  /* By default, PL/I arithmetic items have a DECIMAL base  */
  $arithmetic_attribute::base = "DECIMAL";
  /* By default, PL/I arithmetic items have a REAL mode  */
  $arithmetic_attribute::mode = "REAL";
  /* By default, PL/I arithmetic items are signed  */
  $arithmetic_attribute::isSigned = true;
  /* We don' set a default here because it will dependend on scale/base  */
  $arithmetic_attribute::digits = -1;
  /* By default, PL/I arithmetic have a scaling factor of 0  */
  $arithmetic_attribute::scalingFactor = 0;
  /* Helper for cases where PL/I numeric is one byte long and has no
     numeric equivalent in COBOL*/
  boolean singleByte;
  
  /* Helper for fixed binary where the PL/I digits are expressed in
  bits while COBOL expects actual digits. */
  int integerDigits;
}
    : (^(ARITHMETIC arithmetic_keyword  precision_specification? other_arithmetic_attribute*)
    | ^(ARITHMETIC implicit_precision_specification other_arithmetic_attribute*))
      {
        /* Make sure we have a default digits number
           assumes DEFAULT(IBM) compiler option */
        if ($arithmetic_attribute::digits == - 1) {
            if ($arithmetic_attribute::scale.equals("FLOAT")) {
                if ($arithmetic_attribute::base.equals("DECIMAL")) {
                    $arithmetic_attribute::digits = 6;
                } else {
                    $arithmetic_attribute::digits = 21;
                }
            } else {
                if ($arithmetic_attribute::base.equals("DECIMAL")) {
                    $arithmetic_attribute::digits = 5;
                } else {
                    $arithmetic_attribute::digits = 15;
                }
            }
        }
        
        /* Issue warnings when translation is not optimal and determine
        number of COBOL digits for fixed binary. */
        singleByte = false;
        integerDigits = 0;
        if ($arithmetic_attribute::scale.equals("FIXED") && $arithmetic_attribute::base.equals("BINARY")) {
            if ($arithmetic_attribute::isSigned) {
                if ($arithmetic_attribute::digits <=7) {
                    issueWarning("Item " + $data_item::name + ", One byte integer translated to PIC X");
                    singleByte = true;
                } else if ($arithmetic_attribute::digits  <= 15) {
                    integerDigits = 4;
                } else if ($arithmetic_attribute::digits  <= 31) {
                    integerDigits = 9;
                } else {
                    integerDigits = 18;
                }
            } else {
                if ($arithmetic_attribute::digits <=8) {
                    issueWarning("Item " + $data_item::name + ", One byte integer translated to PIC X");
                    singleByte = true;
                } else if ($arithmetic_attribute::digits  <= 16) {
                    integerDigits = 4;
                } else if ($arithmetic_attribute::digits  <= 32) {
                    integerDigits = 9;
                } else {
                    integerDigits = 18;
                }
            }
        }
        if ($arithmetic_attribute::scale.equals("FLOAT") && $arithmetic_attribute::base.equals("DECIMAL")) {
            if ($arithmetic_attribute::digits > 16) {
                issueWarning("Item " + $data_item::name + ", precision " + $arithmetic_attribute::digits + " too large for COBOL");
            }
        }
        if ($arithmetic_attribute::scale.equals("FLOAT") && $arithmetic_attribute::base.equals("BINARY")) {
            if ($arithmetic_attribute::digits > 53) {
                issueWarning("Item " + $data_item::name + ", precision " + $arithmetic_attribute::digits + " too large for COBOL");
            }
        }
      }
      ->{$arithmetic_attribute::scale.equals("FLOAT") && $arithmetic_attribute::base.equals("DECIMAL")}?
        floatUsage(single={$arithmetic_attribute::digits <= 6})

      ->{$arithmetic_attribute::scale.equals("FLOAT") && $arithmetic_attribute::base.equals("BINARY")}?
        floatUsage(single={$arithmetic_attribute::digits <= 21})

      ->{$arithmetic_attribute::scale.equals("FIXED") && $arithmetic_attribute::base.equals("DECIMAL")}?
        packedPictureUsage(
            integerDigits={$arithmetic_attribute::digits - $arithmetic_attribute::scalingFactor},
            fractionDigits={($arithmetic_attribute::scalingFactor <= 0) ? null : $arithmetic_attribute::scalingFactor})

      ->{$arithmetic_attribute::scale.equals("FIXED") && $arithmetic_attribute::base.equals("BINARY")}?
        binaryPictureUsage(
            singleByte={singleByte},
            signed={$arithmetic_attribute::isSigned},
            integerDigits={integerDigits})

      ->
    ;
 
arithmetic_keyword
    : FLOAT {$arithmetic_attribute::scale = "FLOAT";}
    | FIXED {$arithmetic_attribute::scale = "FIXED";}
    | BINARY {$arithmetic_attribute::base = "BINARY";}
    | DECIMAL {$arithmetic_attribute::base = "DECIMAL";}
    | SIGNED {$arithmetic_attribute::isSigned = true;}
    | UNSIGNED {$arithmetic_attribute::isSigned = false;}
    | REAL {$arithmetic_attribute::mode = "REAL";}
    | COMPLEX {$arithmetic_attribute::mode = "COMPLEX";}
    ;

other_arithmetic_attribute
    : arithmetic_keyword precision_specification?
    ;

precision_specification
    : implicit_precision_specification
    ;

implicit_precision_specification
    : ^(PRECISION UNSIGNED_INTEGER scaling_factor?)
     {$arithmetic_attribute::digits = Integer.parseInt($UNSIGNED_INTEGER.text);}
    ;

scaling_factor
    : ^(SCALING_FACTOR (v=UNSIGNED_INTEGER | v=SIGNED_INTEGER))
     {$arithmetic_attribute::scalingFactor = Integer.parseInt($v.text);}
    ;

sign_sequence
    : ^(SIGNED (
     SIGNED_KEYWORD {$arithmetic_attribute::isSigned = true;}
     | UNSIGNED_KEYWORD {$arithmetic_attribute::isSigned = false;}
     ))
     
    ;

/*------------------------------------------------------------------
 * -- Dimension attribute
 * Produce a COBOL occurs clause for array items.
 * <p/>
 * COBOL does not support lower bounds in the PLI sense. If a lower bound
 * is present, we assume it reduces the array capacity.
 * COBOL has a notion of minimum capacity (as in OCCURS n TO ...) but there
 * is no equivalent in PLI.
 * <p/>
 * In PLI both lower and upper bound can be variable. Here we support variability
 * only if there are no lower bounds.
 * <p/>
 * Multiple dimension arrays are supported with severe restrictions in this version:
 * <ul>
 * <li>Dimensions must not have a lower bound</li>
 * <li>Dimensions must not be variable</li>
 * </ul>
*------------------------------------------------------------------*/
dimension_attribute
scope {
  int maxOccurs;
}
@init {
    $dimension_attribute::maxOccurs = 1;
}
    : implicit_dimension_attribute
    ->{$implicit_dimension_attribute.st}
    ;
    
implicit_dimension_attribute
    : ^(DIMENSIONS c+=bound_attribute+)
    ->occurs(maxOccurs={$dimension_attribute::maxOccurs},dependingOn={c.st})
    ;
    
bound_attribute
scope {
    int lBound;
    int hBound;
}
@init {
    $bound_attribute::lBound = 1;
    $bound_attribute::hBound = 1;
}
    : ^(DIMENSION (v=double_bound_expression | w=single_bound_expression))
    {
        /* COBOL has no equivalent lower bound so we simply reduce the array size.
           If array has multiple dimensions, we merge them all into a single one  */
        $dimension_attribute::maxOccurs *= ($bound_attribute::hBound + 1 - $bound_attribute::lBound);
    }
    ->{($v.st == null) ? $w.st : $v.st}
    ;
    
double_bound_expression
    : ^(LBOUND UNSIGNED_INTEGER refer_specification?) single_bound_expression
    {
        $bound_attribute::lBound =  Integer.parseInt($UNSIGNED_INTEGER.text);
        if ($refer_specification.st != null) {
            issueWarning("Item " + $data_item::name + ", Array with variable lower bound. Refer ignored.");
        }
    }
    ->{$single_bound_expression.st}
    ;    

single_bound_expression
    : ^(HBOUND UNSIGNED_INTEGER refer_specification?)
    {$bound_attribute::hBound = Integer.parseInt($UNSIGNED_INTEGER.text);}
    ->{$refer_specification.st}
    ;    

/*------------------------------------------------------------------
 * -- Initial attribute
*------------------------------------------------------------------*/
initial_attribute
    : ^(INITIAL (v=STRING_LITERAL | v=SIGNED_INTEGER | v=UNSIGNED_INTEGER | v=FLOAT))
    ->value(initial={$v.text})
    ;

/*------------------------------------------------------------------
 * -- Alignment attribute
*------------------------------------------------------------------*/
alignment_attribute
    : ^(ALIGNMENT ALIGNED)
    | ^(ALIGNMENT UNALIGNED)
        ;

/*------------------------------------------------------------------
 * -- Storage attribute
*------------------------------------------------------------------*/
storage_attribute
    : ^(STORAGE AUTOMATIC)
    | ^(STORAGE STATIC)
    | ^(STORAGE BASED)
    | ^(STORAGE CONTROLLED)
    ;

/*------------------------------------------------------------------
 * -- Union attribute
 * Stores unions qualified name for later reference by children
*------------------------------------------------------------------*/
union_attribute
    : (UNION) {unions.put($data_item::qualifiedName, "");}
    ;
