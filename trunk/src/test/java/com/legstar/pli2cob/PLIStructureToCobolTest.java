package com.legstar.pli2cob;


/**
 * Test the PLI to COBOL converter.
 *
 */
public class PLIStructureToCobolTest extends AbstractTester {

    /**
     * Group single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testGroupItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Group;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Group." + LS
                + "" + LS);
    }

    /**
     * Character single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testCharacterItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last char(20);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC X(20)." + LS
                + "" + LS);
    }

    /**
     * Character single item with varying attribute.
     * @throws CobolFormatException if conversion fails
     */
    public void testCharacterVaryingItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last char(20) varying;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last." + LS
                + "           02 LEN PIC 9(4) BINARY." + LS
                + "           02 CHAR PIC X OCCURS 1 TO 20 DEPENDING ON LEN." + LS
                + "" + LS);
    }
    /**
     * Graphic single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testGraphicItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last graphic(20);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC G(20) DISPLAY-1." + LS
                + "" + LS);
    }

    /**
     * Widechar single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testWidecharItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last widechar(20);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC N(20)." + LS
                + "" + LS);
    }

    /**
     * BIT strings are not supported.
     * @throws CobolFormatException if conversion fails
     */
    public void testBitItem() throws CobolFormatException {
        try {
            convertCheck(
                    "Declare 1 Last bit(20);",

                    "      *" + LS
                    + "      *" + LS
                    + "      *" + LS
                    + "       01 Last PIC X(20)." + LS
                    + "" + LS);
            fail();
        } catch (CobolFormatException e) {
            assertEquals("Unsupported string type: "
                    + "[level : 1,"
                    + " name : Last,"
                    + " type : BIT,"
                    + " length : 20,"
                    + " varying : NONVARYING]", e.getMessage());
        }
    }

    /**
     * Picture alphabetic single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testPictureAlphabeticItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last pic '(5)A9XX';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC A(5)9XX." + LS
                + "" + LS);
    }

    /**
     * Picture numeric single item.
     * @throws CobolFormatException if conversion fails
     */
    public void testPictureNumericItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last pic 'ZZV(3)9';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC ZZV9(3)." + LS
                + "" + LS);
    }

    /**
     * Picture numeric with overpunch character.
     * @throws CobolFormatException if conversion fails
     */
    public void testPictureOverpunchItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 Last PIC '(5)9V(2)9T';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC 9(5)V9(2)9." + LS
                + "" + LS);
        convertCheck(
                "Declare 1 Last PIC 'T(4)9V(3)9';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC 99(4)V9(3) LEADING." + LS
                + "" + LS);
        convertCheck(
                "Declare 1 Last PIC '(5)9V(3)9S';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC S9(5)V9(3) SIGN SEPARATE." + LS
                + "" + LS);
        convertCheck(
                "Declare 1 Last PIC 'S(5)9V(3)9';",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Last PIC S9(5)V9(3) SIGN SEPARATE LEADING." + LS
                + "" + LS);
    }

    /**
     * FLOAT + DECIMAL single float.
     * @throws CobolFormatException if conversion fails
     */
    public void testFloatDecimalItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A float dec(6);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A COMP-1." + LS
                + "" + LS);
    }

    /**
     * FLOAT + DECIMAL double float.
     * @throws CobolFormatException if conversion fails
     */
    public void testDoubleDecimalItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A float dec(16);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A COMP-2." + LS
                + "" + LS);
    }

    /**
     * FLOAT + DECIMAL single item with large precision.
     * @throws CobolFormatException if conversion fails
     */
    public void testFloatDecimalItemTooLarge() throws CobolFormatException {
        try {
            convertCheck(
                    "Declare 1 A float dec(20);",

                    "      *" + LS
                    + "      *" + LS
                    + "      *" + LS
                    + "       01 Last PIC X(20)." + LS
                    + "" + LS);
            fail();
        } catch (CobolFormatException e) {
            assertEquals("Unsupported precision: "
                    + "[level : 1,"
                    + " name : A,"
                    + " scale : FLOAT,"
                    + " base : DECIMAL,"
                    + " signed : true,"
                    + " precision : 20,"
                    + " scaling factor : 0]", e.getMessage());
        }
    }

    /**
     * FLOAT + BINARY single float.
     * @throws CobolFormatException if conversion fails
     */
    public void testFloatBinaryItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A float bin(21);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A COMP-1." + LS
                + "" + LS);
    }

    /**
     * FLOAT + BINARY double float.
     * @throws CobolFormatException if conversion fails
     */
    public void testDoubleBinaryItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A float bin(53);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A COMP-2." + LS
                + "" + LS);
    }

    /**
     * FIXED + DECIMAL .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedDecimalItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed dec(10,2);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC S9(8)V9(2) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * FIXED + DECIMAL no decimal part.
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedDecimalNoDecimalsItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed dec(10);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC S9(10) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + SIGNED   PRECISION <= 7 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinarySignedTinyItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(7);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC X(1)." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + SIGNED   7 < PRECISION <= 15 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinarySignedShortItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(15);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC S9(4) COMP-5." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + SIGNED   15 < PRECISION <= 31 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinarySignedIntegerItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(31);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC S9(9) COMP-5." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + SIGNED   31 < PRECISION <= 63 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinarySignedLongItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(63);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC S9(18) COMP-5." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + UNSIGNED   PRECISION <= 8 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinaryUnsignedTinyItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(8) unsigned;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC X(1)." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + UNSIGNED   8 < PRECISION <= 16 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinaryUnsignedShortItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(16) unsigned;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC 9(4) COMP-5." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + UNSIGNED   16 < PRECISION <= 32 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinaryUnsignedIntegerItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(32) unsigned;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC 9(9) COMP-5." + LS
                + "" + LS);
    }

    /**
     * FIXED + BINARY + UNSIGNED   32 < PRECISION <= 64 .
     * @throws CobolFormatException if conversion fails
     */
    public void testFixedBinaryUnsignedLongItem() throws CobolFormatException {
        convertCheck(
                "Declare 1 A fixed bin(64) unsigned;",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 A PIC 9(18) COMP-5." + LS
                + "" + LS);
    }

    /**
     * Test a simple array with simple dimension.
     * @throws CobolFormatException if conversion fails
     */
    public void testArraySingleDimension() throws CobolFormatException {
        convertCheck(
                "declare List fixed decimal(3) dimension(8);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 List OCCURS 8 PIC S9(3) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * Test an array with a lower bound dimension.
     * @throws CobolFormatException if conversion fails
     */
    public void testArrayLowerBoundDimension() throws CobolFormatException {
        convertCheck(
                "declare List_A dimension(4:11);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 List-A OCCURS 8." + LS
                + "" + LS);
    }

    /**
     * Test an array with a variable lower bound dimension.
     * @throws CobolFormatException if conversion fails
     */
    public void testArrayUnsupportedLowerBoundDimension() throws CobolFormatException {
        try {
            convertCheck(
                    "declare List_A dimension(4 refer(X):11);",

                    "      *" + LS
                    + "      *" + LS
                    + "      *" + LS
                    + "       01 List-A OCCURS 8." + LS
                    + "" + LS);
            fail();
        } catch (CobolFormatException e) {
            assertEquals("Unsupported variable lower bound: "
                    + "[level : 1,"
                    + " name : List_A,"
                    + " dimensions :"
                    + " [lbound :"
                    + " [bound : 4,"
                    + " refer : X],"
                    + " hbound :"
                    + " [bound : 11]]]", e.getMessage());
        }
    }

    /**
     * Test an array with a variable upper bound dimension.
     * @throws CobolFormatException if conversion fails
     */
    public void testArrayVariableUpperBoundDimension() throws CobolFormatException {
        convertCheck(
                "declare List_B dimension(1:11 refer(X));",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 List-B OCCURS 11 DEPENDING X." + LS
                + "" + LS);
    }

    /**
     * Test an array with multiple dimensions.
     * @throws CobolFormatException if conversion fails
     */
    public void testArrayMultipleDimensions() throws CobolFormatException {
        convertCheck(
                "declare Table (4,2) fixed dec (3);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Table OCCURS 8 PIC S9(3) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * Single branch structure.
     * @throws CobolFormatException if conversion fails
     */
    public void testSingleBranchStructure() throws CobolFormatException {
        convertCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Payroll." + LS
                + "           04 Name." + LS
                + "           05 Last PIC X(20)." + LS
                + "" + LS);
    }

    /**
     * Multiple branch structure.
     * @throws CobolFormatException if conversion fails
     */
    public void testMultipleBranchStructure() throws CobolFormatException {
        convertCheck(
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

                "      *" + LS
                + "      *" + LS
                + "      *" + LS
                + "       01 Payroll." + LS
                + "           02 Name." + LS
                + "           03 Last PIC X(20)." + LS
                + "           03 First PIC X(15)." + LS
                + "           02 Hours." + LS
                + "           03 Regular PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           03 Overtime PIC S9(3)V9(2) PACKED-DECIMAL." + LS
                + "           02 Rate." + LS
                + "           03 Regular PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "           03 Overtime PIC S9(1)V9(2) PACKED-DECIMAL." + LS
                + "" + LS);
    }

    /**
     * Test level formatting.
     * @throws Exception if something goes wrong
     */
    public void testFormatCobolLevel() throws Exception {
        PLIStructureToCobol converter = new PLIStructureToCobol();
        assertEquals("       01", converter.formatLevel(1));
        assertEquals("           12", converter.formatLevel(12));
        try {
            assertEquals("12", converter.formatLevel(145));
            fail();
        } catch (CobolFormatException e) {
            assertEquals("Level 145 is invalid for COBOL", e.getMessage());
        }
    }

    /**
     * Test name formatting.
     * @throws Exception if something goes wrong
     */
    public void testFormatCobolName() throws Exception {
        PLIStructureToCobol converter = new PLIStructureToCobol();
        assertEquals("to-5z", converter.formatName("to_5z"));
        assertEquals("a12345678901234567890123456789",
                converter.formatName("a123456789012345678901234567890"));
        assertEquals("to5z", converter.formatName("to5z"));
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected sub graph
     * @throws CobolFormatException if conversion fails
     */
    private void convertCheck(
            final String source,
            final String expected) throws CobolFormatException {
        PLIStructureToCobol converter = new PLIStructureToCobol();
        String cobol = converter.convert(normalize(source));
        assertEquals(expected, cobol);
    }
}
