package com.legstar.pli2cob;

import junit.framework.TestCase;

/**
 * Test the PLIStructureToCobol class.
 *
 */
public class PLIStructureToCobolTest extends TestCase {

    /**
     * Make sure nulls don't get through.
     */
    public void testNullEntry() {
        PLIStructureToCobol pli2cob = new PLIStructureToCobol();
        try {
            pli2cob.execute(null);
            fail();
        } catch (PLIStructureLexingException e) {
            fail(e.getMessage());
        } catch (PLIStructureParsingException e) {
            fail(e.getMessage());
        } catch (CobolFormatException e) {
            fail(e.getMessage());
        } catch (PLIStructureReadingException e) {
            assertEquals("Source is null", e.getMessage());
        }
    }

    /**
     * Empty source should be supported.
     */
    public void testEmptyEntry() {
        executeCheck("",
                "      *" + '\n'
                + "      * Generated by legstar-pli2cob" + '\n'
                + "      *" + '\n',
                false,
                true);
    }

    /**
     * Try garbage.
     */
    public void testGarbageEntry() {
        PLIStructureToCobol pli2cob = new PLIStructureToCobol();
        try {
            pli2cob.execute(" %�$ ,;:");
            fail();
        } catch (PLIStructureLexingException e) {
            fail(e.getMessage());
        } catch (PLIStructureParsingException e) {
            assertEquals("Parsing token stream failed.", e.getMessage());
        } catch (CobolFormatException e) {
            fail(e.getMessage());
        } catch (PLIStructureReadingException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Empty and syncpad option.
     */
    public void testEmptyAndSyncPad() {
        executeCheck("",
                "      *" + '\n'
                + "      * Generated by legstar-pli2cob" + '\n'
                + "      *" + '\n',
                true,
                true);
    }

    /**
     * Simple structure and syncpad option.
     */
    public void testSimpleAndSyncPad() {
        executeCheck(
                "dcl 1 Client,"
                + "   2 Number pic '999999',"
                + "   2 Type bit(8),"
                + "   2 Name,"
                + "     3 Individual,"
                + "       5 Last_Name char(20),"
                + "       5 First_Name,"
                + "         7 First char(15),"
                + "         7 Initial char(1),"
                + "     3 Company char(35),"
                + "   2 * char(0);",

                "      *" + '\n'
                + "      * Generated by legstar-pli2cob" + '\n'
                + "      *" + '\n'
                + "       01 Client." + '\n'
                + "           02 Number PIC 999999." + '\n'
                + "           02 Type PIC X(1)." + '\n'
                + "           02 Name." + '\n'
                + "             03 Individual." + '\n'
                + "               05 Last-Name PIC X(20)." + '\n'
                + "               05 First-Name." + '\n'
                + "                 07 First PIC X(15)." + '\n'
                + "                 07 Initial PIC X(1)." + '\n'
                + "             03 Company PIC X(35)." + '\n'
                + "\n"
                ,
                true,
                false);
    }

    /**
     * Multiple declares and syncpad option.
     */
    public void testMultipleDeclaresAndSyncPad() {
        executeCheck(
                "dcl 1 Client,"
                + "   2 Number pic '999999',"
                + "   2 Type bit(8);"
                + "dcl 1 Last_Name char(20);"
                + "dcl 1 First_Name,"
                + "   2 First char(15),"
                + "   2 Initial char(1);"
                ,

                "      *" + '\n'
                + "      * Generated by legstar-pli2cob" + '\n'
                + "      *" + '\n'
                + "       01 Client." + '\n'
                + "           02 Number PIC 999999." + '\n'
                + "           02 Type PIC X(1)." + '\n'
                + "       01 Last-Name PIC X(20)." + '\n'
                + "       01 First-Name." + '\n'
                + "           02 First PIC X(15)." + '\n'
                + "           02 Initial PIC X(1)." + '\n'
                + "\n"
                ,
                true,
                false);
    }

    /**
     * A helper to perform a PLI to COBOL conversion.
     * @param source the PLI source fragment
     * @param expected the expected COBOL result
     * @param syncpad whether to generate padding for PLI alignments
     * @param failonerror whether to fail when errors are found
     */
    private void executeCheck(
            final String source,
            final String expected,
            final boolean syncpad,
            final boolean failonerror) {
        try {
            PLIStructureToCobol pli2cob = new PLIStructureToCobol();
            pli2cob.getContext().setSyncpad(true);
            pli2cob.getContext().setFailonerror(failonerror);
            assertEquals(expected, pli2cob.execute(source));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

}
