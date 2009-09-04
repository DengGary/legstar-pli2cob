package com.legstar.pli2cob;


import junit.framework.TestCase;

/**
 * Test the PLISourceCleaner class.
 *
 */
public class PLISourceCleanerTest extends TestCase {

    /**
     * Check that we know how to remove a sequence number from a line.
     */
    public void testRemoveLineSequenceNumber() {
        PLISourceCleaner cleaner = new PLISourceCleaner();
        assertEquals("", cleaner.removeLineSequenceNumbering(""));
        /*       12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        assertEquals(
                " DCL DSGTCBMO    CHAR(1) STATIC INIT('02'X);                            ",
                cleaner.removeLineSequenceNumbering(
                " DCL DSGTCBMO    CHAR(1) STATIC INIT('02'X);                            91000000"));
    }

    /**
     * Check that we know how to remove all sequence numbers from a source.
     */
    public void testRemoveSourceSequenceNumber() {
        try {
            PLISourceCleaner cleaner = new PLISourceCleaner();
            assertEquals(
                    " DCL DSGTCBPJ BIN FIXED(15) STATIC INIT(2);                             \n"
                    + "                               /* 2 = TCB Pool JVM                   */ \n",
                    cleaner.removeSourceSequenceNumbering(
                            " DCL DSGTCBPJ BIN FIXED(15) STATIC INIT(2);                             97000000\n"
                            + "                               /* 2 = TCB Pool JVM                   */ 97600000"));
        } catch (PLIStructureReadingException e) {
            fail(e.getMessage());
        }
    }
    /**
     * Try removing unwanted chars.
     */
    public void testRemovingUnwantedChars() {
        PLISourceCleaner cleaner = new PLISourceCleaner();
        assertEquals("no unwanted chars",
                cleaner.removeUnwantedCharacters("no unwanted chars"));
        assertEquals("one unwanted char here",
                cleaner.removeUnwantedCharacters("one unwanted char here" + (char) 0x1A));
        assertEquals("one unwanted char here and here",
                cleaner.removeUnwantedCharacters("one unwanted char here" + (char) 0x1A + " and here" + (char) 0x1A));
    }
}
