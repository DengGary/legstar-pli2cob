package com.legstar.pli2cob;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * PLI Source code might contain non-PLI characters such as sequence numbers or
 * non Structure PLI statements.
 * The antlr based Lexer/parser would choke on such things so we try to remove
 * them here.
 *
 */
public class PLISourceCleaner {

    /** A pattern to detect ending sequence numbers.*/
    private static final Pattern ENDING_SEQUENCE_NUMBER_PATTERN = Pattern.compile("\\d{8}$");
    
    /** Characters that might be left over by file transfer in a source file. */
    private static final Pattern UNWANTED_CHARACTERS = Pattern.compile("[\\x1A]");

    /**
     * Takes in a raw PLI source potentially containing sequence numbers or
     * non structure statements and produces a clean source code.
     * @param pliSource the raw PLI source
     * @return the source cleaned up
     * @throws PLIStructureReadingException if source cannot be read
     */
    public String execute(final String pliSource) throws PLIStructureReadingException {
        if (pliSource == null) {
            throw new PLIStructureReadingException("Source is null");
        }
        String cleanedSource = removeUnwantedCharacters(pliSource);
        SequenceNumbering result = detectSourceSequenceNumbering(cleanedSource);
        if (result.isOn()) {
            cleanedSource = removeSourceSequenceNumbering(cleanedSource);
        }
        return cleanedSource;
    }
    
    /**
     * Sometimes special meaningless characters get inserted in files transfered from
     * the mainframe. This will remove such characters.
     * @param pliSource the original source
     * @return the source without unwanted characters
     */
    public String removeUnwantedCharacters(final String pliSource) {
        Matcher matcher = UNWANTED_CHARACTERS.matcher(pliSource);
        return matcher.replaceAll("");
    }

    /**
     * Detect line numbering in the source code.
     * <p/>
     * A source will be found to hold sequence numbering if all its lines contain
     * a potential sequence number starting all at the same column.
     * <p>
     * @param source the source being analyzed
     * @return if a sequence number is there on all lines and which column it starts
     * @throws PLIStructureReadingException if source cannot be read
     */
    public SequenceNumbering detectSourceSequenceNumbering(final String source) throws PLIStructureReadingException {
        SequenceNumbering sourceResult = new SequenceNumbering();
        BufferedReader reader = new BufferedReader(new StringReader(source));
        String line;
        boolean firstLine = true;
        try {
            while ((line = reader.readLine()) != null) {
                SequenceNumbering lineResult = detectLineSequenceNumbering(line);
                if (lineResult.isOn()) {
                    if (firstLine) {
                        sourceResult.setOn(true);
                        sourceResult.setColumn(lineResult.getColumn());
                        firstLine = false;
                    } else {
                        if (sourceResult.getColumn() != lineResult.getColumn()) {
                            return new SequenceNumbering();
                        }
                    }
                } else {
                    return new SequenceNumbering();
                }
            }
            return sourceResult;
        } catch (IOException e) {
            throw new PLIStructureReadingException(e);
        }
    }

    /**
     * Sequence numbers are 8 digits that might appear as the very last characters
     * of the line.
     * @param line the statement being analyzed
     * @return if a potential sequence number is there and which column it starts
     */
    public SequenceNumbering detectLineSequenceNumbering(final String line) {
        SequenceNumbering result = new SequenceNumbering();
        Matcher matcher = ENDING_SEQUENCE_NUMBER_PATTERN.matcher(line);
        if (matcher.find()) {
            result.setOn(true);
            result.setColumn(matcher.start() + 1);
        }
        return result;
    }
    
    
    /**
     * Removes sequence numbers from a source file.
     * @param source the PLI source
     * @return a source without sequence numbers
     * @throws PLIStructureReadingException  if source cannot be read
     */
    public String removeSourceSequenceNumbering(final String source) throws PLIStructureReadingException {
        BufferedReader reader = new BufferedReader(new StringReader(source));
        String line;
        StringBuffer cleanedSource =  new StringBuffer();
        try {
            while ((line = reader.readLine()) != null) {
                String cleanedLine = removeLineSequenceNumbering(line);
                cleanedSource.append(cleanedLine + '\n');
            }
            return cleanedSource.toString();
        } catch (IOException e) {
            throw new PLIStructureReadingException(e);
        }
    }

    /**
     * If a line numbering exist on this line, this method returns the
     * statement without the line numbering.
     * @param line with potential sequence number
     * @return a line without a sequence number
     */
    public String removeLineSequenceNumbering(final String line) {
        Matcher matcher = ENDING_SEQUENCE_NUMBER_PATTERN.matcher(line);
        if (matcher.find()) {
            return line.substring(0, matcher.start());
        } else {
            return line;
        }
    }
    
    /**
     * Holder for sequence numbering status and starting column.
     *
     */
    public class SequenceNumbering {
        /** True if there is a sequence number. */
        private boolean _on = false;

        /** Column where the sequence number starts.*/
        private int _column = -1;

        /**
         * @return true if there is a sequence number
         */
        public boolean isOn() {
            return _on;
        }

        /**
         * @param on true if there is a sequence number
         */
        public void setOn(final boolean on) {
            this._on = on;
        }

        /**
         * @return column where the sequence number starts
         */
        public int getColumn() {
            return _column;
        }

        /**
         * @param column column where the sequence number starts
         */
        public void setColumn(final int column) {
            this._column = column;
        }

    }

}
