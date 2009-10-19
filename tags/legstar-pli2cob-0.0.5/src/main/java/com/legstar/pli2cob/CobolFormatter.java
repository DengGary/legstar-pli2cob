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
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Provides indentation and column fitting services for COBOL data description code.
 * <p/>
 * Cobol data descriptions are usually indented for better readability, it is also
 * important that no code extends past the 72nd column.
 *
 */
public class CobolFormatter {

    /** Indentation of data description area A. */
    public static final int INDENT_AREA_A = 7;

    /** Indentation of data description area B. */
    public static final int INDENT_AREA_B = 11;

    /** Used to prepend whitespaces as indentation.*/
    private static final String WHITESPACES = "          " //10
        + "          " //20
        + "          " //30
        + "          " //40
        + "          " //50
        + "          " //60
        + "          " //70
        + "  "; //72

    /** Space characters representing AREA B. */
    private static final String AREA_B = WHITESPACES.substring(0, INDENT_AREA_B);

    /** AREA B when continuing a literal. */
    private static final String AREA_B_CONTINUATION = "      -    ";

    /** Maximum number of characters in a data description code line. */
    public static final int MAX_LINE_CHARS = 72;

    /** Maximum size of a literal that can fit on a single line.
     * All literals are assumed to start on area B.*/
    public static final int MAX_LITERAL_CHARS = MAX_LINE_CHARS - INDENT_AREA_B;


    /** Number of whitespaces to indent a child relative to its parent. */
    public static final int INDENT_INCREMENT = 2;

    /** Stores indentation per data description level. */
    private Map < Integer, Integer > _levelIndents =
        new HashMap < Integer, Integer >();

    /** A pattern to locate level and current leading whitespaces in a cobol data description.*/
    private static final Pattern LEVEL_PATTERN = Pattern.compile("^\\s*(\\d\\d)");

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /**
     * Indents levels in area B and makes sure statements do not extend beyond column 72.
     * @param reader unformatted data description source
     * @return a formatted data description source
     * @throws CobolFormatException if formatting fails
     */
    public String format(final Reader reader) throws CobolFormatException {
        BufferedReader in = null;
        try {
            StringBuilder sb = new StringBuilder();
            in = new BufferedReader(reader);
            String line = null;
            while ((line = in.readLine()) != null) {
                Matcher matcher = LEVEL_PATTERN.matcher(line);
                if (matcher.find()) {
                    int level = Integer.parseInt(
                            matcher.group().substring(
                                    matcher.group().length() - 2));
                    String indentedLine = indent(
                            line.substring(matcher.end() - 2),
                            calcIndent(level));
                    fitIn72Columns(indentedLine, sb);
                } else {
                    fitIn72Columns(line, sb);
                }
            }
            return sb.toString();
        } catch (IOException e) {
            throw new CobolFormatException(e);
        } finally {
            try {
                in.close();
            } catch (IOException e) {
                throw new CobolFormatException(e);
            }
        }
    }

    /**
     * Determine how many whitespaces should be prepended depending on the level.
     * @param level the data description level
     * @return number of whitespaces to prepend
     */
    public int calcIndent(final int level) {
        int indent = lookupIndent(level);
        if (indent == -1) {
            int parentLevel = lookupImmediateParentLevel(level);
            if (parentLevel > -1) {
                if (parentLevel == 1) {
                    indent = store(level, INDENT_AREA_B);
                } else {
                    indent = store(level,
                            _levelIndents.get(parentLevel) + INDENT_INCREMENT);
                }
            } else {
                if (level == 1) {
                    indent = store(level, INDENT_AREA_A);
                } else {
                    indent = store(level, INDENT_AREA_B);
                }
            }
        }
        return indent;
    }

    /**
     * Since we save an associated indentation for each level we process, this
     * code looks up the level in case it has already been processed.
     * @param level the data description level we are looking for
     * @return the indentation level or -1 if not processed yet
     */
    private int lookupIndent(final int level) {
        if (_levelIndents.get(level) != null) {
            return _levelIndents.get(level);
        }
        return -1;
    }

    /**
     * Search data description items already saved for a potential parent.
     * We are interested in the immediate parent.
     * @param level the data description level for which we look for va parent
     * @return the immediate parent level
     */
    private int lookupImmediateParentLevel(final int level) {
        int immediateParent = -1;
        for (Map.Entry < Integer, Integer > entry : _levelIndents.entrySet()) {
            if (entry.getKey() < level && entry.getKey() > immediateParent) {
                immediateParent = entry.getKey();
            }
        }
        return immediateParent;
    }

    /**
     * Keeps the last indentation for a given level and returns it.
     * @param level the data description item level
     * @param indent the indentation associated with that level
     * @return the associated indentation
     */
    private int store(final int level, final int indent) {
        _levelIndents.put(level, indent);
        return indent;
    }

    /**
     * Add the number of whitespaces needed in front of the line code.
     * @param line the line to be indented (assumed not to start with whitespaces)
     * @param indent number of whitespaces to add
     * @return the indented line of code
     */
    private String indent(final String line, final int indent) {
        return WHITESPACES.substring(0, indent) + line.trim();
    }

    /**
     * If a line is too long, this code breaks the line in as many lines as needed
     * to fit the content.
     * @param line the line to be fitted in 72 columns
     * @param sb the formatted code
     * @throws CobolFormatException if line cannot be fitted in 72 columns
     */
    public void fitIn72Columns(
            final String line,
            final StringBuilder sb) throws CobolFormatException {
        if (line.length() > MAX_LINE_CHARS + 1) {
            int i = tokenDelimiter(line, MAX_LINE_CHARS);
            if (i > -1) {
                /* Splitter location is accepted only if beyond area B, otherwise
                 * we would be looping forever. */
                if (i >= INDENT_AREA_B) {
                    sb.append(line.substring(0, i) + LS);
                    fitIn72Columns(AREA_B + line.substring(i + 1), sb);
                } else {
                    /* We probably have a long literal that does not fit in 72 columns*/
                    String literal = extractLiteral(line);
                    if (literal == null || literal.length() == 0) {
                        throw new CobolFormatException(
                                "Unable to fit this in " + MAX_LINE_CHARS + " columns: "
                                + line);
                    }
                    splitLiteral(literal, literal.charAt(0), sb);
                }
            } else {
                throw new CobolFormatException(
                        "Unable to fit this in " + MAX_LINE_CHARS + " columns: "
                        + line);
            }
        } else {
            sb.append(line + LS);
        }
    }

    /**
     * COBOL lines can be split on token delimiters.
     * A token delimiter is a white space unless that white space is in a
     * string literal.
     * This looks into a string starting at the position requested and moving
     * backward to the start of the string until a token delimiter is found. 
     * @param line the line to search for a token delimiter
     * @param startPos where to start in the line (index position)
     * @return the position of the token delimiter or -1 if none is found
     */
    private int tokenDelimiter(final String line, final int startPos) {
        int i = startPos;
        while (i > -1) {
            /* A white space if found and we are not in the middle of a literal */
            if (line.charAt(i) == ' ' && !isInLiteral(line, i)) {
                return i;
            }
            i--;
        }
        return i;
    }

    /**
     * Given a position in a COBOL sentence, determines if that position is in the middle
     * of a literal.
     * @param line COBOL sentence or fragment
     * @param pos position within the sentence or fragment
     * @return true if position is in the middle of a literal
     */
    private boolean isInLiteral(final String line, final int pos) {
        boolean isInLiteral = false;
        char delim = '\0';
        for (int i = 0; i < pos; i++) {
            char c = line.charAt(i);
            if (c == '\'' || c == '\"') {
                if (isInLiteral) {
                    /* This is the literal closing delimiter */
                    if (delim == c) {
                        isInLiteral = false;
                    }
                } else {
                    /* This is a literal opening delimiter */
                    delim = c;
                    isInLiteral = true;
                }
            }
        }
        return isInLiteral;
    }

    /**
     * Look for the first literal starting at the beginning of the line.
     * This actually extracts the literal, along with the remaining of the line.
     * @param line the line containing a literal
     * @return the literal (including its delimiters) or null if none is found
     */
    private String extractLiteral(final String line) {
        char delim = '\0';
        int startPos = -1;
        boolean isInLiteral = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '\'' || c == '\"') {
                if (isInLiteral) {
                    /* This is the literal closing delimiter */
                    if (delim == c) {
                        return line.substring(startPos);
                    }
                } else {
                    /* This is a literal opening delimiter */
                    delim = c;
                    isInLiteral = true;
                    startPos = i;
                }
            }
        }
        return null;
    }

    /**
     * Splits a literal COBOL style.
     * @param literal the literal, or part of the literal, to split (including its delimiters)
     * @param delim the literal delimiter
     * @param sb the output code
     */
    private void splitLiteral(final String literal, final char delim, final StringBuilder sb) {
        if (literal.length() > MAX_LITERAL_CHARS) {
            int written = writeLiteral(literal.substring(0, MAX_LITERAL_CHARS), delim, sb);
            splitLiteral(literal.substring(written), delim, sb);
        } else  {
            writeLiteral(literal, delim, sb);
        }
    }

    /**
     * A split literal needs a continuation character starting from line 2 onward.
     * Furthermore the continuation literal needs to start with the delimiter.
     * @param literal the literal, or part of the literal, to write
     * @param delim the literal delimiter
     * @param sb the output code
     * @return the number of characters written
     */
    private int writeLiteral(final String literal, final char delim, final StringBuilder sb) {
        if (literal.charAt(0) == delim) {
            sb.append(AREA_B + literal + LS);
        } else {
            /* We need to add an extra delimiter. That might result in a statement that is
             * again too long to fit on a line*/
            if (literal.length() >= MAX_LITERAL_CHARS) {
                sb.append(AREA_B_CONTINUATION + delim + literal.substring(0, MAX_LITERAL_CHARS - 1) + LS);
                return MAX_LITERAL_CHARS - 1;
            }
            sb.append(AREA_B_CONTINUATION + delim + literal + LS);
        }
        return literal.length();
    }


}
