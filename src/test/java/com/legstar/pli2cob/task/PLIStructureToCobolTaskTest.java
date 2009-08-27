package com.legstar.pli2cob.task;

import java.io.File;

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
        fileset.setFile(new File("src/test/resources/pli/DFHAID"));
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("You must provide a target directory", e.getMessage());
        }
        task.setTargetDir(new File("target"));
        try {
            task.execute();
        } catch (BuildException e) {
            assertEquals("com.legstar.pli2cob.PLIStructureParsingException: Parsing token stream failed.", e.getMessage());
        }
    }
    
    

}
