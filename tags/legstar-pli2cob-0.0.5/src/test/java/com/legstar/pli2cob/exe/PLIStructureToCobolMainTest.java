package com.legstar.pli2cob.exe;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;

import com.legstar.pli2cob.AbstractTester;

/**
 * Test PLIStructureToCobolMain.
 *
 */
public class PLIStructureToCobolMainTest extends AbstractTester {

    /**
     * Test with no options at all.
     */
    public void testNoOption() {
        assertEquals(
                "usage: pli2cob",
                command(null).substring(0, "usage: pli2cob".length()));
    }

    /**
     * Test with an invalid option.
     */
    public void testInvalidOption() {
        assertEquals(
                "Parsing failed.  Reason: Unrecognized option: -buggy" + LS,
                commandErr(new String[] {"-buggy"}));
    }
    /**
     * Test the command line version option.
     */
    public void testVersionOption() {
        assertEquals(
                "version " + PLIStructureToCobolMain.getVersion() + LS,
                command(new String[] {"-version"}));
    }

    /**
     * Test case where input does not exist.
     */
    public void testInputNotFound() {
        assertEquals(
                "Input file or folder buggy not found" + LS,
                commandErr(new String[] {"-i buggy"}));
    }

    /**
     * Test case where input is empty.
     */
    public void testInputEmpty() {
        File empty = new File("target/empty");
        empty.mkdir();
        assertEquals(
                "Folder target/empty is empty" + LS,
                commandErr(new String[] {"-i target/empty"}));
    }

    /**
     * Test case where output is not a folder.
     */
    public void testOutputNotAFolder() {
        assertEquals(
                "File target/generated-sources/version/version.properties is not a folder" + LS,
                commandErr(new String[] {"-o target/generated-sources/version/version.properties"}));
    }

    /**
     * Test with a single PL/I file.
     */
    public void testSinglePLIFile() {
        (new File("target/cobol")).delete();
        command(new String[] {
                "-isrc/test/resources/pli/SAMPLE01", "-otarget/cobol"}
        );
        assertTrue(
                getFileContent(new File("target/cobol/SAMPLE01.cbl"))
                .contains("       01 SAMP-COMMAREA.")
        );
    }

    /**
     * Test with a folder of PL/I files.
     */
    public void testMultiplePLIFiles() {
        (new File("target/cobol")).delete();
        command(new String[] {
                "-isrc/test/resources/pli", "-otarget/cobol"}
        );
        assertTrue(
                getFileContent(new File("target/cobol/SAMPLE01.cbl"))
                .contains("       01 SAMP-COMMAREA.")
        );
        assertTrue(
                getFileContent(new File("target/cobol/SAMPLE02.cbl"))
                .contains("       01 Client.")
        );
    }

    /**
     * Run the main class as a command line passing arguments and sending
     * back the output (System.out).
     * @param args the options selected
     * @return the output produced
     */
    private String command(final String[] args) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        System.setOut(new PrintStream(bos));
        PLIStructureToCobolMain.main(args);
        return bos.toString();
    }

    /**
     * Run the main class as a command line passing arguments and sending
     * back the error (System.err).
     * @param args the options selected
     * @return the output produced
     */
    private String commandErr(final String[] args) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        System.setErr(new PrintStream(bos));
        PLIStructureToCobolMain.main(args);
        return bos.toString();
    }
}
