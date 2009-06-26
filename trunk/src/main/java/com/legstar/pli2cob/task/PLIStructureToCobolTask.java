package com.legstar.pli2cob.task;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.stringtemplate.StringTemplate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.FileSet;

import com.legstar.pli2cob.CobolFormatException;
import com.legstar.pli2cob.PLIStructureLexer;
import com.legstar.pli2cob.PLIStructureParser;
import com.legstar.pli2cob.PLIStructureToCobol;
import com.legstar.pli2cob.PLIStructureTreeNormalizer;
import com.legstar.pli2cob.PLIStructureParser.script_return;

/**
 * PLI to COBOL structure ANT Task.
 * <code>
 * Usage:<br>
 *
 * &nbsp;&lt;project ...&gt;<br>
 * &nbsp;&nbsp;&lt;taskdef name="pli2cob" classname="com.legstar.pli2cob.task.PLIStructureToCobolTask" /&gt;<br>
 * &nbsp;&nbsp;&lt;property name="pli.dir" value="../pli"/&gt;<br>
 * &nbsp;&nbsp;&lt;property name="cobol.dir" value="../cobol"/&gt;<br>
 * &nbsp;&nbsp;&lt;target name="generate"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&lt;pli2cob targetDir="${cobol.dir}"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&lt;fileset dir="${pli.dir}"&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&lt;include name="*.pli" /&gt;<br>
 * &nbsp;&nbsp;&nbsp;&nbsp;&lt;/fileset&gt;<br>
 * &nbsp;&nbsp;&nbsp;&lt;/pli2cob&gt;<br>
 * &nbsp;&nbsp;&&lt;/target&gt;<br>
 * &nbsp;&lt;/project&gt;<br>
 * </code>
 *
 */
public class PLIStructureToCobolTask extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** The list of filesets that were setup.*/
    private List < FileSet > _fileSets = new LinkedList < FileSet >();

    /** The target directory where cobol files will be created. */
    private File _targetDir;

    /**
     *  The ant execution method.
     *  Check parameters and produce COBOL fragment.
     */
    public final void execute() {
        _log.info("Converting PLI files");

        checkParameters();

        Iterator < FileSet > itor = _fileSets.iterator();
        while (itor.hasNext()) {
            FileSet fileset = itor.next();

            DirectoryScanner scanner = fileset.getDirectoryScanner(getProject());
            scanner.scan();
            String[] files = scanner.getIncludedFiles();
            for (int i = 0; i < files.length; i++) {
                File pliSourceFile = new File(fileset.getDir(getProject()), files[i]);
                _log.info("Converting PLI file: " + pliSourceFile);
                String cobolSource = convert(normalize(parse(lexify(fileToString(pliSourceFile)))));
                File cobolSourceFile = stringToFile(getTargetDir(), pliSourceFile, cobolSource);
                _log.info("Created COBOL file: " + cobolSourceFile);
            }
        }
    }
    
    /**
     * Check that we have enough parameters to get started.
     */
    private void checkParameters() {
        if (_fileSets.isEmpty()) {
            throw new BuildException("No fileset specified");
        }

        /* Check that we have a valid target directory.  */
        if (getTargetDir() == null) {
            throw (new BuildException(
            "You must provide a target directory"));
        }
        if (!getTargetDir().exists()) {
            throw (new BuildException(
                    "Directory " + getTargetDir() + " does not exist"));
        }
        if (!getTargetDir().isDirectory() || !getTargetDir().canWrite()) {
            throw (new BuildException(
                    getTargetDir() + " is not a directory or is not writable"));
        }
    }
    
    /**
     * Reads a file content (assumed to a characters) into a String.
     * @param file the input file
     * @return the cointent as a String
     */
    private String fileToString(final File file) {
        String errorMessage = "Unable to read file " + file;
        StringBuilder sb = new StringBuilder();
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(file));
            String line = null;
            while ((line = reader.readLine()) != null) {
                sb.append(line + '\n');
            }
            return sb.toString();
        } catch (FileNotFoundException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    _log.warn("Unable to close file " + file, e);
                }
            }
        }
    }
    
    /**
     * Creates a COBOL file converted from a PLI source.
     * @param targetDir the target directory we know exists
     * @param sourceFile the original source file (used to derive a name for the target file)
     * @param cobolSource the COBOL source code as a string
     * @return the cobol file created
     */
    private File stringToFile(final File targetDir, final File sourceFile, final String cobolSource) {
        String cobolFileName = sourceFile.getName() + ".cbl";
        File cobolFile = new File(targetDir, cobolFileName);
        String errorMessage = "Unable to write file " + cobolFile;
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(cobolFile));
            writer.write(cobolSource);
            return cobolFile;
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    _log.warn("Unable to close file " + cobolFile, e);
                }
            }
        }
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     */
    public CommonTokenStream lexify(final String source) {
        if (_log.isDebugEnabled()) {
            debug("Lexing source:", source);
        }
        String errorMessage = "Lexing source: " + source + " failed.";
        try {
            PLIStructureLexer lex = new PLIStructureLexer(
                    new ANTLRReaderStream(new StringReader(source)));
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() != 0 || tokens == null) {
                _log.error(errorMessage);
                throw (new BuildException(errorMessage));
            }
            return tokens;
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream. 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     */
    public CommonTree parse(final CommonTokenStream tokens) {
        if (_log.isDebugEnabled()) {
            debug("Parsing tokens:", tokens.toString());
        }
        String errorMessage = "Parsing token stream: " + tokens + " failed.";
        try {
            PLIStructureParser parser = new PLIStructureParser(tokens);
            script_return parserResult = parser.script();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                _log.error(errorMessage);
                throw (new BuildException(errorMessage));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        }
    }

    /**
     * Apply Normalizer to produce a normalized abstract syntax tree from source. 
     * @param ast the abstract syntax tree produced by parser (flat)
     * @return an antlr abstract syntax tree where nodes are organized in a hierarchy
     */
    public CommonTree normalize(final CommonTree ast) {
        if (_log.isDebugEnabled()) {
            debug("Flat abstract syntax tree:", ast);
        }
        return PLIStructureTreeNormalizer.normalize(ast);
    }

    /**
     * Final conversion starting from normalized abstract syntax tree. 
     * @param ast the abstract syntax tree produced by normalizer (hierarchy)
     * @return a PLI source
     */
    public String convert(final CommonTree ast) {
        if (_log.isDebugEnabled()) {
            debug("Normalized abstract syntax tree:", ast);
        }
        String errorMessage = "Converting abstract syntax tree: " + ast + " failed.";
        try {
            PLIStructureToCobol converter = new PLIStructureToCobol();
            String cobolSource = converter.convert(ast);
            if (_log.isDebugEnabled()) {
                debug("COBOL structure produced:", cobolSource);
            }
            return cobolSource;
        } catch (CobolFormatException e) {
            _log.error(errorMessage, e);
            throw (new BuildException(errorMessage));
        }
    }

    /**
     * @return a new FileSet
     */
    public FileSet createFileset() {
        FileSet fileset = new FileSet();
        _fileSets.add(fileset);
        return fileset;
    }    

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return _targetDir;
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        _targetDir = targetDir;
    }
    
    /**
     * Produce long text in a delimited debug zone.
     * @param title the debug text title
     * @param text the text itself
     */
    private void debug(final String title, final String text) {
        _log.debug(title);
        _log.debug(text);
        _log.debug("----------------------------------------------------------------");
    }

    /**
     * Produce a graph of an abstract syntax tree.
     * @param title the debug text title
     * @param ast the abstract syntax tree
     */
    private void debug(final String title, final CommonTree ast) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(ast);
        _log.debug(title);
        _log.debug(st.toString());
        _log.debug("----------------------------------------------------------------");
    }
}
