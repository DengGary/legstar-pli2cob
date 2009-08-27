package com.legstar.pli2cob;

import com.legstar.pli2cob.PLISourceCleaner.SequenceNumbering;

import junit.framework.TestCase;

/**
 * Test the PLISourceCleaner class.
 *
 */
public class PLISourceCleanerTest extends TestCase {
    
    /**
     * Check that line sequences are detected correctly.
     */
    public void testLineSequences() {
        PLISourceCleaner cleaner = new PLISourceCleaner();
        SequenceNumbering result = cleaner.detectLineSequenceNumbering("");
        assertFalse(result.isOn());
        result = cleaner.detectLineSequenceNumbering("5BCD7F56");
        assertFalse(result.isOn());
        result = cleaner.detectLineSequenceNumbering("00080000");
        assertTrue(result.isOn());
        assertEquals(1, result.getColumn());
        result = cleaner.detectLineSequenceNumbering("000100000200");
        assertTrue(result.isOn());
        assertEquals(5, result.getColumn());
        result = cleaner.detectLineSequenceNumbering("      00080000");
        assertTrue(result.isOn());
        assertEquals(7, result.getColumn());
    }
    
    /**
     * Check the source behavior.
     */
    public void testSourceSequences() {
        try {
            PLISourceCleaner cleaner = new PLISourceCleaner();
            SequenceNumbering result = cleaner.detectSourceSequenceNumbering("");
            assertFalse(result.isOn());

            result = cleaner.detectSourceSequenceNumbering("      00080000");
            assertTrue(result.isOn());
            assertEquals(7, result.getColumn());

            result = cleaner.detectSourceSequenceNumbering("      00080000\n");
            assertTrue(result.isOn());
            assertEquals(7, result.getColumn());

            result = cleaner.detectSourceSequenceNumbering("      00080000\n      00080000");
            assertTrue(result.isOn());
            assertEquals(7, result.getColumn());

            result = cleaner.detectSourceSequenceNumbering("      00080000\n     00080000");
            assertFalse(result.isOn());

            result = cleaner.detectSourceSequenceNumbering("      00080000\n ");
            assertFalse(result.isOn());
        } catch (PLIStructureReadingException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Check that we know how to remove a sequence number from a line.
     */
    public void testRemoveLineSequenceNumber() {
        PLISourceCleaner cleaner = new PLISourceCleaner();
        assertEquals("", cleaner.removeLineSequenceNumbering(""));
        assertEquals("", cleaner.removeLineSequenceNumbering("00080000"));
        assertEquals("      ", cleaner.removeLineSequenceNumbering("      00080000"));
    }

    /**
     * Check that we know how to remove all sequence numbers from a source.
     */
    public void testRemoveSourceSequenceNumber() {
        try {
            PLISourceCleaner cleaner = new PLISourceCleaner();
            assertEquals("      \n      \n", cleaner.removeSourceSequenceNumbering(
                    "      00080000\n      00080000"));
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
