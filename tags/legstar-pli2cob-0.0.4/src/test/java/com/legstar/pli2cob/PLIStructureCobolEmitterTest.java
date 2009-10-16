/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pli2cob;

/**
 * Test the generated PLIStructureCobolEmitter tree walker.
 *
 */
public class PLIStructureCobolEmitterTest extends AbstractTester {

    /**
     * A data item with implicit level.
     * @throws Exception if test fails
     */
    public void testMissingLevel() throws Exception {
        emitAndCheck("dcl A;", "       01 A.");
    }


    /**
     * A data item with explicit level.
     * @throws Exception if test fails
     */
    public void testExplicitLevel() throws Exception {
        emitAndCheck("dcl 5 A;", "           05 A.");
    }

    /**
     * A series of declares.
     * @throws Exception if test fails
     */
    public void testMultipleDeclares() throws Exception {
        emitAndCheck("dcl 1 A; dcl 1 B;",

                ""
                + "       01 A." + LS
                + "       01 B."
        );
    }

    /**
     * A data item with star name.
     * @throws Exception if test fails
     */
    public void testStarName() throws Exception {
        emitAndCheck("dcl *;", "       01 FILLER.");
    }

    /**
     * A data item with a PL/I keyword name.
     * @throws Exception if test fails
     */
    public void testPl1KeywordName() throws Exception {
        emitAndCheck("dcl FIXED;", "       01 FIXED.");
    }

    /**
     * A data item with a COBOL reserved name.
     * @throws Exception if test fails
     */
    public void testCobolReservedName() throws Exception {
        emitAndCheck("dcl PICTURE;", "       01 R-PICTURE.");
    }
    /**
     * A structure with only 2 levels.
     * @throws Exception if test fails
     */
    public void testStructure2Levels() throws Exception {
        emitAndCheck("dcl 1 A, 2 B;",

                ""
                + "       01 A." + LS
                + "           02 B."
        );
    }

    /**
     * A structure with multiple levels and level sequence rupture.
     * @throws Exception if test fails
     */
    public void testStructureSequenceRupture() throws Exception {
        emitAndCheck("dcl 1 A, 2 B, 5 C, 7 D, 4 E, 4 F, 3 G, 2 H, 3 I;",

                ""
                + "       01 A." + LS
                + "           02 B." + LS
                + "             05 C." + LS
                + "               07 D." + LS
                + "             04 E." + LS
                + "             04 F." + LS
                + "             03 G." + LS
                + "           02 H." + LS
                + "             03 I."
        );
    }

    /**
     * Test attribute detection.
     * @throws Exception if conversion fails
     */
    public void testAligmentDetection() throws Exception {
        emitAndCheck(
                "Declare 1 ClientID bin fixed(31) unaligned;",

                "       01 ClientID PIC S9(9) COMP-5.");
    }

    /**
     * An elementary character string.
     * @throws Exception if test fails
     */
    public void testElementaryString() throws Exception {
        emitAndCheck("dcl a CHAR(3);", "       01 a PIC X(3).");
    }

    /**
     * An elementary character string with default length.
     * @throws Exception if test fails
     */
    public void testElementaryStringDefaultLength() throws Exception {
        emitAndCheck("dcl a CHAR;", "       01 a PIC X(1).");
    }

    /**
     * An elementary character string with zero length.
     * @throws Exception if test fails
     */
    public void testElementaryStringZeroLength() throws Exception {
        emitAndCheck("dcl a CHAR(0);", "");
    }

    /**
     * An elementary widechar.
     * @throws Exception if test fails
     */
    public void testElementaryWidechar() throws Exception {
        emitAndCheck("dcl a WIDECHAR(3);", "       01 a PIC N(3).");
    }

    /**
     * An elementary widechar with default length.
     * @throws Exception if test fails
     */
    public void testElementaryWidecharDefaultLength() throws Exception {
        emitAndCheck("dcl a wchar;", "       01 a PIC N(1).");
    }

    /**
     * An elementary graphic.
     * @throws Exception if test fails
     */
    public void testElementaryGraphic() throws Exception {
        emitAndCheck("dcl a GRAPHIC(3);", "       01 a PIC G(3) DISPLAY-1.");
    }

    /**
     * An elementary graphic with default length.
     * @throws Exception if test fails
     */
    public void testElementaryGraphicDefaultLength() throws Exception {
        emitAndCheck("dcl a g;", "       01 a PIC G(1) DISPLAY-1.");
    }

    /**
     * An elementary bit.
     * @throws Exception if test fails
     */
    public void testElementaryBit() throws Exception {
        emitAndCheck("dcl a BIT(8);", "       01 a PIC X(1).");
    }

    /**
     * BIT strings with length not multiple of 8 produce warnings.
     * @throws Exception if test fails
     */
    public void testElementaryBitNot8Multiple() throws Exception {
        emitAndCheck("dcl a BIT(20);", "       01 a PIC X(2).");
    }

    /**
     * An elementary bit with default length.
     * @throws Exception if test fails
     */
    public void testElementaryBitDefaultLength() throws Exception {
        emitAndCheck("dcl a BIT;", "       01 a PIC X(1).");
    }

    /**
     * An elementary character string with varying size.
     * @throws Exception if test fails
     */
    public void testElementaryStringVarying() throws Exception {
        emitAndCheck("dcl a CHAR(3) VARYING;"
                ,
                ""
                + "       01 a." + LS
                + "           02 LEN PIC 9(4) BINARY." + LS
                + "           02 FILLER PIC X OCCURS 1 TO 3 DEPENDING LEN OF a.");
    }

    /**
     * An elementary character string with varying size with more indent.
     * @throws Exception if test fails
     */
    public void testElementaryStringVaryingIndent() throws Exception {
        emitAndCheck("dcl a, 2 b CHAR(3) VARYING;"
                ,
                ""
                + "       01 a." + LS
                + "           02 b." + LS
                + "             03 LEN PIC 9(4) BINARY." + LS
                + "             03 FILLER PIC X OCCURS 1 TO 3 DEPENDING LEN OF b.");
    }

    /**
     * An elementary string with C-style VARYINGZ. Should issue a warning.
     * @throws Exception if test fails
     */
    public void testElementaryStringVaryingz() throws Exception {
        emitAndCheck("dcl a CHAR VARYINGZ;", "       01 a PIC X(1).");
    }

    /**
     * An elementary string with a variable length depending upon another item.
     * @throws Exception if test fails
     */
    public void testElementaryStringLengthDependingOn() throws Exception {
        emitAndCheck("dcl a CHAR(15 REFER (b));"
                ,
                ""
                + "       01 a." + LS
                + "           02 FILLER PIC X OCCURS 1 TO 15 DEPENDING b.");
    }

    /**
     * Picture alphabetic single item.
     * @throws Exception if translation fails
     */
    public void testPictureAlphabeticItem() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar pic '(5)A9XX';",

                "       01 Myvar PIC A(5)9XX."
        );
    }

    /**
     * Picture numeric single item.
     * @throws Exception if translation fails
     */
    public void testPictureNumericItem() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar pic 'ZZV(3)9';",

                "       01 Myvar PIC ZZV9(3)."
        );
    }

    /**
     * Picture numeric with overpunch character.
     * @throws Exception if translation fails
     */
    public void testPictureOverpunchItem() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar PIC '(5)9V(2)9T';",

                "       01 Myvar PIC 9(5)V9(2)9."
        );
        emitAndCheck(
                "Declare 1 Myvar PIC 'T(4)9V(3)9';",

                "       01 Myvar PIC S99(4)V9(3) LEADING."
        );
        emitAndCheck(
                "Declare 1 Myvar PIC '(5)9V(3)9S';",

                "       01 Myvar PIC S9(5)V9(3) SIGN TRAILING SEPARATE."
        );
        emitAndCheck(
                "Declare 1 Myvar PIC 'S(5)9V(3)9';",

                "       01 Myvar PIC S9(5)V9(3) SIGN LEADING SEPARATE."
        );
    }

    /**
     * Float decimal item with all defaults.
     * @throws Exception if translation fails
     */
    public void testFloatDecimalAllDefaults() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FLOAT;",

                "       01 Myvar COMP-1."
        );
    }

    /**
     * Float decimal item becomes a single float.
     * @throws Exception if translation fails
     */
    public void testFloatDecimalSingle() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar float dec(6);",

                "       01 Myvar COMP-1."
        );
    }
    /**
     * Float decimal item with enough digits to make it a double.
     * @throws Exception if translation fails
     */
    public void testFloatDecimalDouble() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar PRECISION(7) FLOAT;",

                "       01 Myvar COMP-2."
        );
        emitAndCheck(
                "Declare 1 Myvar FLOAT(7);",

                "       01 Myvar COMP-2."
        );
        emitAndCheck(
                "Declare 1 Myvar FLOAT dec(7);",

                "       01 Myvar COMP-2."
        );
    }

    /**
     * Float decimal item becomes a double float with a warning.
     * @throws Exception if translation fails
     */
    public void testFloatDecimalTooLarge() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar float dec(20);",

                "       01 Myvar COMP-2."
        );
    }
    /**
     * Float Binary item with default precision.
     * @throws Exception if translation fails
     */
    public void testFloatBinaryDefaultPrecision() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FLOAT BINARY;",

                "       01 Myvar COMP-1."
        );
    }

    /**
     * Float Binary item with gives a single.
     * @throws Exception if translation fails
     */
    public void testFloatBinarySingle() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar BINARY PRECISION(21) FLOAT;",

                "       01 Myvar COMP-1."
        );
        emitAndCheck(
                "Declare 1 Myvar FLOAT(21) BINARY;",

                "       01 Myvar COMP-1."
        );
    }

    /**
     * Float Binary item with enough digits to make it a double.
     * @throws Exception if translation fails
     */
    public void testFloatBinaryDouble() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar BINARY PRECISION(22) FLOAT;",

                "       01 Myvar COMP-2."
        );
        emitAndCheck(
                "Declare 1 Myvar FLOAT(22) BINARY;",

                "       01 Myvar COMP-2."
        );
    }

    /**
     * Float binary item becomes a double float with a warning.
     * @throws Exception if translation fails
     */
    public void testFloatBinaryTooLarge() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar float dec(54);",

                "       01 Myvar COMP-2."
        );
    }
    /**
     * Fixed decimal item with default precision.
     * @throws Exception if translation fails
     */
    public void testFixedDecimalDefaultPrecision() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED;",

                "       01 Myvar PIC S9(5) PACKED-DECIMAL."
        );
    }

    /**
     * Fixed decimal item with no scaling factor.
     * @throws Exception if translation fails
     */
    public void testFixedDecimalNoScaling() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED(10);",

                "       01 Myvar PIC S9(10) PACKED-DECIMAL."
        );
    }
    /**
     * Fixed decimal item with a scaling factor.
     * @throws Exception if translation fails
     */
    public void testFixedDecimalScalingFactor() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED DEC(7,2);",

                "       01 Myvar PIC S9(5)V9(2) PACKED-DECIMAL."
        );
    }

    /**
     * Fixed binary item with default precision.
     * @throws Exception if translation fails
     */
    public void testFixedBinaryDefaultPrecision() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN;",

                "       01 Myvar PIC S9(4) COMP-5."
        );
    }

    /**
     * Fixed binary item with a 1 byte storage.
     * @throws Exception if translation fails
     */
    public void testFixedBinaryNoPrecision() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN(7);",

                "       01 Myvar PIC X."
        );
    }

    /**
     * Fixed binary item with a 1 byte storage.
     * @throws Exception if translation fails
     */
    public void testFixedBinarySingleByte() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN(7,2);",

                "       01 Myvar PIC X."
        );
    }

    /**
     * Fixed binary item with a 2 byte storage.
     * FIXED + BINARY + SIGNED   7 < PRECISION <= 15 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryTwoBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN(8,2);",

                "       01 Myvar PIC S9(4) COMP-5."
        );
        emitAndCheck(
                "Declare 1 Myvar PRECISION(15) FIXED BIN;",

                "       01 Myvar PIC S9(4) COMP-5."
        );
    }

    /**
     * Fixed binary item with a 4 byte storage.
     * FIXED + BINARY + SIGNED   15 < PRECISION <= 31 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryFourBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar SIGNED FIXED BIN(16,2);",

                "       01 Myvar PIC S9(9) COMP-5."
        );
        emitAndCheck(
                "Declare 1 Myvar PRECISION(31) FIXED BIN;",

                "       01 Myvar PIC S9(9) COMP-5."
        );
    }

    /**
     * Fixed binary item with a 8 byte storage.
     * FIXED + BINARY + SIGNED   31 < PRECISION <= 63 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryEightBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED SIGNED BIN(32);",

                "       01 Myvar PIC S9(18) COMP-5."
        );
    }

    /**
     * Fixed binary unsigned item with default precision.
     * @throws Exception if translation fails
     */
    public void testFixedBinaryUnsignedDefaultPrecision() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar UNSIGNED FIXED BIN;",

                "       01 Myvar PIC 9(4) COMP-5."
        );
    }

    /**
     * Fixed binary unsigned item with a 1 byte storage.
     * FIXED + BINARY + UNSIGNED   PRECISION <= 8 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryUnsignedSingleByte() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED UNSIGNED BIN(8,2);",

                "       01 Myvar PIC X."
        );
    }

    /**
     * Fixed binary item unsigned with a 2 byte storage.
     * FIXED + BINARY + UNSIGNED   8 < PRECISION <= 16 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryUnsignedTwoBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN(9,2) UNSIGNED;",

                "       01 Myvar PIC 9(4) COMP-5."
        );
        emitAndCheck(
                "Declare 1 Myvar PRECISION(16) FIXED BIN UNSIGNED;",

                "       01 Myvar PIC 9(4) COMP-5."
        );
    }

    /**
     * Fixed binary unsigned item with a 4 byte storage.
     * FIXED + BINARY + UNSIGNED   16 < PRECISION <= 32 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryUnsignedFourBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar UNSIGNED FIXED BIN(17,2);",

                "       01 Myvar PIC 9(9) COMP-5."
        );
        emitAndCheck(
                "Declare 1 Myvar PRECISION(32) UNSIGNED FIXED BIN;",

                "       01 Myvar PIC 9(9) COMP-5."
        );
    }

    /**
     * Fixed binary unsigned item with a 8 byte storage.
     * FIXED + BINARY + UNSIGNED   32 < PRECISION <= 64 .
     * @throws Exception if translation fails
     */
    public void testFixedBinaryUnsignedEightBytes() throws Exception {
        emitAndCheck(
                "Declare 1 Myvar FIXED BIN(33) UNSIGNED;",

                "       01 Myvar PIC 9(18) COMP-5."
        );
    }

    /**
     * Test a simple array with simple dimension.
     * @throws Exception if translation fails
     */
    public void testArraySingleDimension() throws Exception {
        emitAndCheck(
                "declare List fixed decimal(3) dimension(8);",

                "       01 List PIC S9(3) PACKED-DECIMAL OCCURS 8."
        );
    }

    /**
     * Test an array with a lower bound dimension.
     * @throws Exception if translation fails
     */
    public void testArrayLowerBoundDimension() throws Exception {
        emitAndCheck(
                "declare List_A dimension(4:11);",

                "       01 List-A OCCURS 8."
        );
    }

    /**
     * Test an array with a variable lower bound dimension.
     * @throws Exception if translation fails
     */
    public void testArrayUnsupportedLowerBoundDimension() throws Exception {
        emitAndCheck(
                "declare List_A dimension(4 refer(X):11);",

                "       01 List-A OCCURS 8."
        );
    }

    /**
     * Test an array with a variable upper bound dimension.
     * @throws Exception if translation fails
     */
    public void testArrayVariableUpperBoundDimension() throws Exception {
        emitAndCheck(
                "declare List_B dimension(1:11 refer(X));",

                "       01 List-B OCCURS 11 DEPENDING X."
        );
    }

    /**
     * Test an array with multiple dimensions.
     * @throws Exception if translation fails
     */
    public void testArrayMultipleDimensions() throws Exception {
        emitAndCheck(
                "declare Table (4,2) fixed dec (3);",

                "       01 R-Table OCCURS 8 PIC S9(3) PACKED-DECIMAL."
        );
    }

    /**
     * Single branch structure.
     * @throws Exception if conversion fails
     */
    public void testSingleBranchStructure() throws Exception {
        emitAndCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",
                ""
                + "       01 Payroll." + LS
                + "           04 Name." + LS
                + "             05 R-Last PIC X(20)."
        );
    }

    /**
     * Multiple branch structure.
     * @throws Exception if conversion fails
     */
    public void testMultipleBranchStructure() throws Exception {
        emitAndCheck(
                "declare 1 Payroll, /* major structure name */"
                + " 2 Name, /* minor structure name */"
                + "     3 Last char(20), /* elementary name */"
                + "     3 First char(15),"
                + " 2 Hours,"
                + "     3 Regular fixed dec(5,2),"
                + "     3 Overtime fixed dec(5,2),"
                + " 2 Rate,"
                + "     3 Regular fixed dec(3,2),"
                + "     3 Overtime fixed dec(3,2);",

                ""
                + "       01 Payroll." + LS
                + "           02 Name." + LS
                + "             03 R-Last PIC X(20)." + LS
                + "             03 R-First PIC X(15)." + LS
                + "           02 Hours." + LS
                + "             03 Regular PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "             03 Overtime PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           02 Rate." + LS
                + "             03 Regular PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "             03 Overtime PIC S9(1)V9(2) PACKED-DECIMAL."
        );
    }

    /**
     * Test a string item with an INITIAL clause.
     * @throws Exception if conversion fails
     */
    public void testStringInitial() throws Exception {
        emitAndCheck(
                "DCL BASD_STRUC_ID CHAR(4) STATIC INIT('BASD');",

                "       01 BASD-STRUC-ID PIC X(4) VALUE 'BASD'."
        );
    }

    /**
     * Test a numeric item with an INITIAL clause.
     * @throws Exception if conversion fails
     */
    public void testNumericInitial() throws Exception {
        emitAndCheck(
                "DCL BASD_LENGTH_CURRENT FIXED BIN(31) INIT(32);",

                "       01 BASD-LENGTH-CURRENT PIC S9(9) COMP-5 VALUE 32."
        );
    }

    /**
     * Test a a simple union.
     * @throws Exception if conversion fails
     */
    public void testSimpleUnion() throws Exception {
        emitAndCheck(
                "dcl 1 * union, 2 b3 bit(32), 2 b4 bit(16);",
                ""
                + "       01 FILLER." + LS
                + "           02 b3 PIC X(4)." + LS
                + "           02 b4 REDEFINES b3 PIC X(2)."
        );
    }

    /**
     * Test a a more complex union.
     * @throws Exception if conversion fails
     */
    public void testComplexUnion() throws Exception {
        emitAndCheck(
                "dcl 1 NT2 union static,"
                + "   2 Numeric_translate_table2 char(256),"
                + "   2 Alpha_translate_table2 char(256),"
                + "   2 *,"
                + "     3 * char(46),"
                + "     3 * char(10) init('0123456789'),"
                + "     3 * char(200);",

                ""
                + "       01 NT2." + LS
                + "           02 Numeric-translate-table2 PIC X(256)." + LS
                + "           02 Alpha-translate-table2 REDEFINES Numeric-translate-table2 PIC X(256)." + LS
                + "           02 FILLER REDEFINES Numeric-translate-table2." + LS
                + "             03 FILLER PIC X(46)." + LS
                + "             03 FILLER PIC X(10) VALUE '0123456789'." + LS
                + "             03 FILLER PIC X(200)."
        );
    }

    /**
     * Test problematic unions (see http://code.google.com/p/legstar-pli2cob/wiki/PLIToCOBOLUnionVsRedefines).
     * @throws Exception if conversion fails
     */
    public void testProblematicUnions() throws Exception {
        emitAndCheck(
                "dcl 1 a union,"                       
                + "2 b(2) fixed binary(31),"
                + "2 c fixed binary(15);",

                ""
                + "       01 a." + LS
                + "           02 b OCCURS 2 PIC S9(9) COMP-5." + LS
                + "           02 c REDEFINES b PIC S9(4) COMP-5."
        );
    }

    /**
     * Test problematic unions (see http://code.google.com/p/legstar-pli2cob/wiki/PLIToCOBOLUnionVsRedefines).
     * @throws Exception if conversion fails
     */
    public void testProblematicUnions2() throws Exception {
        emitAndCheck(
                "dcl 1 a based,"                       
                + "2 b fixed binary(31),"
                + "2 c union,"
                + "3 d,"
                + "4 e(2 REFER(b)) char(1),"
                + "3 f fixed binary(15);",

                ""
                + "       01 a." + LS
                + "           02 b PIC S9(9) COMP-5." + LS
                + "           02 c." + LS
                + "             03 d." + LS
                + "               04 e OCCURS 2 DEPENDING b PIC X(1)." + LS
                + "             03 f REDEFINES d PIC S9(4) COMP-5."
        );
    }
}
