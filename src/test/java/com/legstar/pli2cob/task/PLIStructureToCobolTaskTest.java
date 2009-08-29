package com.legstar.pli2cob.task;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;

import junit.framework.TestCase;

/**
 * Test PLIStructureToCobolTask class.
 *
 */
public class PLIStructureToCobolTaskTest extends TestCase {

    /**
     * Task must check parameters.
     */
    public void testControls() {
        PLIStructureToCobolTask task = new PLIStructureToCobolTask();
        task.setProject(new Project());
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("No fileset specified", e.getMessage());
        }
        FileSet fileset = task.createFileset();
        fileset.setFile(new File("src/test/resources/pli/SAMPLE01"));
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("You must provide a target directory", e.getMessage());
        }
        task.setTargetDir(new File("target"));
        try {
            task.execute();
            assertEquals(
                    "      *\n"
                    + "      * Generated by legstar-pli2cob\n"
                    + "      *\n"
                    + "       01 SAMP-COMMAREA.\n"
                    + "           02 SAMP-STANDARD-HEADER.\n"
                    + "             03 SAMP-FUNCTION PIC X(1) VALUE '1'.\n"
                    + "             03 SAMP-COMPONENT-CODE PIC X(2) VALUE 'XM'.\n"
                    + "             03 FILLER--001 PIC X(1).\n"
                    + "           02 SAMP-READ PIC X(1).\n"
                    + "           02 SAMP-WRITE PIC X(1).\n"
                    + "           02 SAMP-SYNCPOINT PIC X(1).\n"
                    + "           02 SAMP-RESTART PIC X(1).\n"
                    + "           02 SAMP-RESTART-COUNT PIC S9(4) COMP-5.\n"
                    + "           02 FILLER--002 PIC X(2).\n"
                    + "           02 SAMP-ORIGINAL-ABEND-CODE PIC X(4).\n"
                    + "           02 SAMP-CURRENT-ABEND-CODE PIC X(4).\n"
                    + "\n"
                    ,
                    getFileContent(new File("target/SAMPLE01.cbl")));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Use a complex structure.
     */
    public void testComplexStructure() {
        PLIStructureToCobolTask task = new PLIStructureToCobolTask();
        task.setProject(new Project());
        FileSet fileset = task.createFileset();
        fileset.setFile(new File("src/test/resources/pli/SAMPLE02"));
        task.setTargetDir(new File("target"));
        try {
            task.execute();
            assertEquals(
                    "      *\n"
                    + "      * Generated by legstar-pli2cob\n"
                    + "      *\n"
                    + "       01 Client.\n"
                    + "           02 Number PIC 999999.\n"
                    + "           02 Type PIC X(1).\n"
                    + "           02 Name.\n"
                    + "             03 Individual.\n"
                    + "               05 Last-Name PIC X(20).\n"
                    + "               05 First-Name.\n"
                    + "                 07 First PIC X(15).\n"
                    + "                 07 Initial PIC X(1).\n"
                    + "             03 Company PIC X(35).\n"
                    + "\n"
                    ,
                    getFileContent(new File("target/SAMPLE02.cbl")));
        } catch (BuildException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Fetch the entire contents of a text file, and return it in a String.
     * @param file some file
     * @return the file content
     */
    private String getFileContent(final File file) {
        StringBuilder contents = new StringBuilder();
        try {
            BufferedReader input =  new BufferedReader(new FileReader(file));
            try {
                String line = null;
                while ((line = input.readLine()) != null) {
                    contents.append(line);
                    contents.append('\n');
                }
            } finally {
                input.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

        return contents.toString();
    }

}
