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

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.TreeNodeStream;
import org.antlr.runtime.tree.TreeParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.util.CobolNameResolver;


/**
 * COBOL emitting requires a lot of actions when walking the tree produced by the PL/I parser.
 * <p/>
 * Rather than embedding these actions into the tree grammar, we use this abstract class to
 * hold the methods referenced from the tree grammer.
 * <p/>
 * The consequence is that ANTLRWorks cannot debug the tree grammar without setting its
 * classpath appropriately.
 *
 */
public abstract class AbstractPLIStructureCobolEmitter extends TreeParser {

    /** COBOL level 01 starts in AREA A (column 8).*/
    private static final String LEVEL01_INDENT = "       ";

    /** COBOL subsequent levels start in AREA B (column 12).*/
    private static final String LEVEL02_INDENT = "           ";

    /** Indentation from one level to the next (2 allows for more levels).*/
    private static final String INDENT = "  ";

    /** A helper class to check for valid COBOL names. */
    private CobolNameResolver _nameResolver = new CobolNameResolver();

    /** Detects repetition factors in PL/I picture.*/
    private static final Pattern REPETITION_FACTOR_PATTERN =
        Pattern.compile("\\(\\d+\\)");

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct from a tree nodes stream.
     * @param input the tree nodes stream
     */
    public AbstractPLIStructureCobolEmitter(final TreeNodeStream input) {
        this(input, new RecognizerSharedState());
    }

    /**
     * Construct from a tree nodes stream and a shared state.
     * @param input the tree nodes stream
     * @param state the shared state
     */
    public AbstractPLIStructureCobolEmitter(final TreeNodeStream input, final RecognizerSharedState state) {
        super(input, state);
         
    }
    
    /**
     * Creates the indentation for a specific level, relative to its parent
     * indentation.
     * <p/>
     * Level 01 starts in area A (column 8) while subsequent levels must
     * start in area B (column 12).
     * @param parentIndent the parent indentation characters (empty string if root)
     * @param level this item level
     * @return the indentation characters as a string
     */
    public String formatIndent(final String parentIndent, final int level) {
        String indent = "";
        if (level == 1) {
            indent = LEVEL01_INDENT;
        } else {
            if (parentIndent.length() > 0) {
                if (parentIndent.equals(LEVEL01_INDENT)) {
                    indent = LEVEL02_INDENT;
                } else {
                    indent = parentIndent + INDENT;
                }
            } else {
                indent = LEVEL02_INDENT;
            }
        }
        return indent;
    }
    
    /**
     * Indenting levels and formatting levels as 2 digits.
     * This is not strictly necessary but the result looks nicer.
     * @param indent the indentation characters as a string
     * @param level the data item level
     * @return a formatted level positioned at the right column
     */
    public String formatLevel(final String indent, final int level) {
        if (level > 49) {
            issueWarning("Level " + level + " is invalid for COBOL (greater than 49)");
        }
        return indent + String.format("%1$02d", level);
    }

    /**
     * Formats a COBOL name using the PL/I name as a hint.
     * COBOL is quite restrictive concerning data item names:
     * <ul>
     * <li>From 1 to 30 characters</li>
     * <li>Characters from (A..Z) (a..z) (0..9) - (hyphen)</li>
     * <li>The hyphen cannot appear as the first or last character</li>
     * <li>Must contain at least one alphabetic character</li>
     * <li>Must not conflict with a COBOL reserved word</li>
     * </ul>
     * @param name the PL/I hint name
     * @return a valid COBOL name
     */
    public String formatCobolName(
            final String name) {
        if (name == null || name.length() == 0 || name.equals("*")) {
            return "FILLER";
        }
        return _nameResolver.getName(name);
    }

    /**
     * COBOL pictures are close to PL/I pictures. They are differences though:
     * <ul>
     * <li>Repetition factors are inverted: (n)9 --> 9(n)</li>
     * <li>T signals overpunch which is implicit in COBOL</li>
     * </ul>
     * @param picture the PL/I picture
     * @return the COBOL picture
     */
    public String formatCobolPicture(final String picture) {
        if (picture == null || picture.length() == 0) {
            return picture;
        }
        String cobolPicture = removeDelimiter(picture);
        cobolPicture = invertRepetitionFactor(cobolPicture);
        cobolPicture = resolveOverpunch(cobolPicture);
        return cobolPicture;
    }

    
    /**
     * PL/I picture is enclosed in apostrophe while COBOL picture
     * clause is not enclosed in delimiters.
     * @param picture the PL/I picture clause
     * @return the COBOL picture clause
     */
    protected String removeDelimiter(final String picture) {
        String cobolPicture = picture;
        if (cobolPicture.charAt(0) == '\'') {
            cobolPicture = cobolPicture.substring(1);
        }
        if (cobolPicture.charAt(cobolPicture.length() - 1) == '\'') {
            cobolPicture = cobolPicture.substring(0, cobolPicture.length() - 1);
        }
        return cobolPicture;
    }

    /**
     * In PL/I any character can be overpunched. In COBOL, only the first
     * or last. Here we oversimplify by assuming the PL/I picture has only one
     * T as the first or last position.
     * @param picture the PL/I picture
     * @return a picture with overpunch replaced
     */
    protected String resolveOverpunch(final String picture) {
        if (picture.charAt(0) == 'T') {
            return "S9" + picture.substring(1) + " LEADING";
        }
        if (picture.charAt(0) == 'S') {
            return picture + " SIGN LEADING SEPARATE";
        }
        if (picture.charAt(picture.length() - 1) == 'T') {
            return  picture.substring(0, picture.length() - 1) + '9';
        }
        if (picture.charAt(picture.length() - 1) == 'S') {
            return  'S' + picture.substring(0, picture.length() - 1) + " SIGN TRAILING SEPARATE";
        }
        return picture;
    }

    /**
     * Inverts the repetition factors.
     * @param picture the PL/I picture
     * @return a string with repetition factors inverted
     * TODO scaling factors are missed for repetition factors
     */
    protected String invertRepetitionFactor(final String picture) {
        StringBuilder sb = new StringBuilder();
        int current = 0;
        Matcher matcher = REPETITION_FACTOR_PATTERN.matcher(picture);
        while (matcher.find()) {
            sb.append(picture.substring(current, matcher.start()));
            sb.append(picture.charAt(matcher.end()));
            sb.append(matcher.group());
            current = matcher.end() + 1;
        }
        if (current < picture.length()) {
            sb.append(picture.substring(current));
        }
        return sb.toString();
    }

    /**
     * Something issue with COBOL needs to be raised.  
     * @param message what is wrong
     */
    public void issueWarning(final String message) {
        _log.warn(message);
    }
    
    /** {@inheritDoc} */
    public String getErrorMessage(final RecognitionException e, final String[] tokenNames) {
        if (_log.isDebugEnabled()) {
            List < ? > stack = getRuleInvocationStack(e, this.getClass().getName());
            String msg = null;
            if (e instanceof NoViableAltException) {
                NoViableAltException nvae = (NoViableAltException) e;
                msg = super.getErrorMessage(e, tokenNames)
                    + " (decision=" + nvae.decisionNumber
                    + " state=" + nvae.stateNumber + ")"
                    + " decision=<<" + nvae.grammarDecisionDescription + ">>";
            } else {
               msg = super.getErrorMessage(e, tokenNames);
            }
            return msg + ". Stack=" + stack;
        } else {
            return super.getErrorMessage(e, tokenNames);
        }
    }

    /** {@inheritDoc} */
    public String getTokenErrorDisplay(final Token t) {
        if (_log.isDebugEnabled()) {
            return t.toString();
        } else {
            return super.getTokenErrorDisplay(t);
        }
    }

    /** {@inheritDoc} */
    public void emitErrorMessage(final String msg) {
        _log.error(msg);
    }

}
