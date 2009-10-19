/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.pli2cob;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * PL/I Source code might contain non-PLI characters such as sequence numbers or
 * non Structure PL/I statements.
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
     * Takes in a raw PL/I source potentially containing sequence numbers or
     * non structure statements and produces a clean source code.
     * @param pliSource the raw PL/I source
     * @return the source cleaned up
     * @throws PLIStructureReadingException if source cannot be read
     */
    public String execute(final String pliSource) throws PLIStructureReadingException {
        if (pliSource == null) {
            throw new PLIStructureReadingException("Source is null");
        }
        String cleanedSource = removeUnwantedCharacters(pliSource);
        cleanedSource = removeSourceSequenceNumbering(cleanedSource);
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
     * Removes sequence numbers from a source file.
     * @param source the PL/I source
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
        if (matcher.find() && matcher.start() >= 72) {
            return line.substring(0, matcher.start());
        } else {
            return line;
        }
    }

    /**
     * Holder for sequence numbering status and starting column.
     *
     */
    public static class SequenceNumbering {
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
