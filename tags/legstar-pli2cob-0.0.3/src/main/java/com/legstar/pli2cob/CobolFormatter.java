package com.legstar.pli2cob;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Provides indentation and column fitting services for cobol data description code.
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

    /** Maximum number of characters in a data description code line. */
    public static final int MAX_LINE_CHARS = 72;

    /** Number of whitespaces to indent a child relative to its parent. */
    public static final int INDENT_INCREMENT = 2;

    /** Used to prepend whitespaces as indentation.*/
    private static final String WHITESPACES = "          " //10
        + "          " //20
        + "          " //30
        + "          " //40
        + "          " //50
        + "          " //60
        + "          " //70
        + "  "; //72

    /** Stores indentation per data description level. */
    private Map < Integer, Integer > _levelIndents =
        new HashMap < Integer, Integer >();

    /** A pattern to locate level and current leading whitespaces in a cobol data description.*/
    private static final Pattern LEVEL_PATTERN = Pattern.compile("^\\s*(\\d\\d)");

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
            int i = MAX_LINE_CHARS;
            while (i > -1 && line.charAt(i) != ' ') {
                i--;
            }
            if (i > 0) {
                sb.append(line.substring(0, i) + '\n');
                fitIn72Columns(WHITESPACES.substring(0, INDENT_AREA_B)
                        + line.substring(i + 1), sb);
            } else {
                if (i == 0) {
                    /* Line contains a single whitespace character, ignore. */
                    return;
                } else {
                    throw new CobolFormatException(
                            "Unable to fit this in " + MAX_LINE_CHARS + " columns: "
                            + line);
                }
            }
        } else {
            sb.append(line + '\n');
        }
    }

}
