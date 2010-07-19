package com.legstar.pli2cob.exe;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import com.legstar.pli2cob.PLIStructureToCobol;
import com.legstar.pli2cob.Pli2CobContext;
import com.legstar.pli2cob.util.CobolNameResolver;

/**
 * PL/I to COBOL structure standalone executable.
 * <p/>
 * This is the main class for the executable jar. It takes options from the
 * command line and calls the {@link PLIStructureToCobol} API.
 * <p/>
 * Usage:
 * <code>
 * java -jar legstar-pli2cob-x.y.z-exe.jar -i&lt;input file or folder&gt; -o&lt;output folder&gt;
 * </code>
 *
 */
public class PLIStructureToCobolMain {

    /** The version properties file name. */
    private static final String VERSION_FILE_NAME = "/version.properties";

    /** 
     * Indicates whether additional padding bytes should be added to COBOL
     * structures to accommodate PL/I hidden alignment mapping bytes.
     * */
    private boolean _addPad = false;

    /** 
     * Indicates whether initial padding bytes should be added to COBOL
     * structures to accommodate PL/I potential structure offsetting from
     * doubleword boundaries.
     * */
    private boolean _addHang = false;

    /** A file or folder containing PL/I code to translate to COBOL. Defaults to cobol relative folder.*/
    private File _input = new File("pl1");

    /** A folder containing translated COBOL code. Defaults to cobol relative folder. */
    private File _output = new File("cobol");

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /**
     * @param args translator options. Provides help if no arguments passed.
     */
    public static void main(final String[] args) {
        PLIStructureToCobolMain main = new PLIStructureToCobolMain();
        main.execute(args);
    }

    /**
     * Process command line options and run translator.
     * <p/>
     * If no options are passed, prints the help. Help is also printed
     * if the command line options are invalid.
     * @param args translator options
     */
    public void execute(final String[] args) {
        Options options = createOptions();
        if (args != null && args.length > 0) {
            CommandLineParser parser = new PosixParser();
            try {
                CommandLine line = parser.parse(options, args);
                if (processLine(line)) {
                    execute(_input, _output);
                }
                return;
            } catch (ParseException e) {
                System.err.println("Parsing failed.  Reason: " + e.getMessage());
            }
        }
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("pli2cob", options);
    }

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", "version", false, "print the version information and exit");
        options.addOption(version);

        Option input = new Option("i", "input", true,
        "file or folder holding the PL/I code to translate. Name is relative or absolute");
        options.addOption(input);

        Option output = new Option("o", "output", true,
        "folder holding the translated COBOL code");
        options.addOption(output);

        Option addPad = new Option("addPad",
        "Add padding bytes to COBOL structures to accommodate PL/I structures mapping optimization");
        options.addOption(addPad);

        Option addHang = new Option("addHang",
        "Add initial padding bytes to COBOL structures to accommodate PL/I hang bytes");
        options.addOption(addHang);

        return options;
    }

    /**
     * Process the command line options selected.
     * @param line the parsed command line
     * @return false if processing needs to stop, true if its ok to continue
     */
    protected boolean processLine(final CommandLine line) {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("addPad")) {
            _addPad = true;
        }
        if (line.hasOption("addHang")) {
            _addHang = true;
        }
        if (line.hasOption("input")) {
            String input = line.getOptionValue("input").trim();
            if (!setInput(input)) {
                return false;
            }
        }
        if (line.hasOption("output")) {
            String output = line.getOptionValue("output").trim();
            if (!setOutput(output)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Translate a single file or all files from an input folder.
     * Place results in the output folder.
     * @param input the input PL/I file or folder
     * @param targetDir the output folder where COBOL file must go
     */
    protected void execute(final File input, final File targetDir) {
        try {
            PLIStructureToCobol pli2cob = new PLIStructureToCobol(createContext());
            if (_input != null && _output != null) {
                if (input.isFile()) {
                    pli2cob.translate(input, targetDir);
                } else {
                    for (File pl1SourceFile : input.listFiles()) {
                        if (pl1SourceFile.isFile()) {
                            pli2cob.translate(pl1SourceFile, targetDir);
                        }
                    }
                }
            } else {
                System.err.println("No input or output was specified.");
            }
        } catch (Exception e) {
            System.err.println("Exception caught: " + e.getMessage());
        }
        
    }

    /**
     * Gather all parameters into a context object.
     * @return a parameter context to be used throughout all code
     */
    private Pli2CobContext createContext() {
        Pli2CobContext context = new Pli2CobContext();
        context.setAddPAd(isAddPad());
        context.setAddHang(isAddHang());
        return context;
    }

    /**
     * Pick up the version from the properties file.
     * @return the product version
     */
    public static String getVersion() {
        InputStream stream = null;
        try {
            Properties version = new Properties();
            stream = CobolNameResolver.class.getResourceAsStream(
                            VERSION_FILE_NAME);
            version.load(stream);
            return  version.getProperty("version");
        } catch (IOException e) {
            System.err.println("Unable to locate COBOL reserved word file " + VERSION_FILE_NAME
                    + ". Will not check for COBOL reserved words");
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    System.err.println("Unable to close stream");
                }
            }
        }
        return null;
    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * @param input a file or folder name (relative or absolute)
     * @return false if input is invalid
     */
    public boolean setInput(final String input) {
        File file = new File(input);
        if (!file.exists()) {
            System.err.println("Input file or folder " + input + " not found");
            return false;
        } else {
            if (file.isDirectory() && file.list().length == 0) {
                System.err.println("Folder " + input + " is empty");
                return false;
            } else {
                _input = file;
                return true;
            }
        }

    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * @param output a file or folder name (relative or absolute)
     * @return false if output is invalid
     */
    public boolean setOutput(final String output) {
        File file = new File(output);
        if (!file.exists()) {
            if (!file.mkdir()) {
                System.err.println("Output folder " + output + " cannot be created");
                return false;
            } else {
                _output = file;
                return true;
            }
        } else {
            if (!file.isDirectory()) {
                System.err.println("File " + output + " is not a folder");
                return false;
            } else {
                _output = file;
                return true;
            }
        }
    }

    /**
     * @return whether additional padding bytes should be added to COBOL
     * structures to accommodate PL/I hidden alignment mapping bytes
     */
    public boolean isAddPad() {
        return _addPad;
    }

    /**
     * @return whether initial padding bytes should be added to COBOL
     * structures to accommodate PL/I potential structure offsetting from
     * doubleword boundaries
     */
    public boolean isAddHang() {
        return _addHang;
    }

    /**
     * @deprecated
     * Fetch the entire contents of a text file, and return it in a String.
     * @param file some file
     * @return the file content
     */
    public String getFileContent(final File file) {
        StringBuilder contents = new StringBuilder();
        try {
            BufferedReader input =  new BufferedReader(new FileReader(file));
            try {
                String line = null;
                while ((line = input.readLine()) != null) {
                    contents.append(line);
                    contents.append(LS);
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
    
    /**
     * @deprecated
     * Store a string in a file.
     * @param string the string text
     * @param file a file
     */
    public void storeStringToFile(final String string, final File file) {
        try {
            FileWriter out = null;    
            try {
                out = new FileWriter(file);
                out.write(string);
                out.close();
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
