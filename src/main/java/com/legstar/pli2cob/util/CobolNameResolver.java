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
package com.legstar.pli2cob.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This class is used to determine a valid Cobol name. A valid cobol name
 * verifies the following rules:
 * <ul>
 * <li>less than or equal to 30 characters long</li>
 * <li>characters taken from A through Z, a through z, 0 through 9, - (hyphen)</li>
 * <li>First and last characters cannot be hyphens</li>
 * <li>First character cannot be a digit</li>
 * <li>Such a name must not be identical to one of the Cobol reserved words</li>
 * </ul>
 * For simplicity (this is not a Cobol requirement) we also make sure the
 * proposed Cobol name is unique within the context of this name resolver.
 *
 */
public class CobolNameResolver {

    /** The COBOL reserved words substitution file name. */
    private static final String RWS_FILE_NAME = "/cobolrws.properties";

    /** In memory reserved words substitution properties. */
    private static Properties _reservedWords;

    /** Names already mapped. */
    private Map < String, String > _namesMap = new HashMap < String, String >();

    /** Names already used. */
    private List < String > _usedNames = new ArrayList < String >();

    /** Valid COBOL characters table. */
    private static char[] mC =
    {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
        'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
        'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
        'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '-'};

    /** Maximum size of a Cobol item name. */
    public static final int MAX_COBOLNAME_LEN = 30;

    /** For reserved words with no substitution this prefix will be
     * prepended to ensure cobol var is not reserved.  */
    public static final String RESERVED_PREFIX = "R-";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    /**
     * Create an instance of the cobol name resolver.
     */
    public CobolNameResolver() {
        InputStream stream = null;
        if (_reservedWords == null) {
            try {
                _reservedWords = new Properties();
                stream = CobolNameResolver.class.getResourceAsStream(
                                RWS_FILE_NAME);
                _reservedWords.load(stream);
            } catch (IOException e) {
                _log.warn("Unable to locate COBOL reserved word file " + RWS_FILE_NAME
                        + ". Will not check for COBOL reserved words.");
            } finally {
                if (stream != null) {
                    try {
                        stream.close();
                    } catch (IOException e) {
                        _log.warn("Unable to close stream");
                    }
                }
            }
        }
    }
    
    /**
     * This makes sure the same hint always maps to the same COBOL name.
     * If the hint has not been previously mapped to a valid COBOL name,
     * we construct a new COBOL name guaranteed to be unique and we map
     * that new name with the hint.
      * @param hint a potential content for the COBOL name
     * @return a valid COBOL name
     */
    public final String getName(final String hint) {
        String cobolName = _namesMap.get(hint);
        if (cobolName == null) {
            /* Make sure this is a unique name otherwise build a unique one */
            cobolName = makeUnique(formatName(hint));
            _namesMap.put(hint, cobolName);
        }
        return cobolName;
    }

    /**
     * Formats a valid COBOL name based on the hint.
     * @param hint a potential content for the COBOL name
     * @return a valid COBOL name
     */
    private String formatName(final String hint) {

        /* Make sure proposed name only contains valid cobol characters */
        String cobolName = switchCharacters(hint);

        /* Trim the proposed name if it is too long */
        if (cobolName.length() > MAX_COBOLNAME_LEN) {
            cobolName = cobolName.substring(0, MAX_COBOLNAME_LEN);
            /* After trimming, the last character might now be invalid */
            int i = MAX_COBOLNAME_LEN - 1;
            while (!isValidLast(cobolName.charAt(i))) {
                i--;
            }
            cobolName = cobolName.substring(0, i + 1);
        }

        /* Check if this is a reserved word and get a substitution */
        String subst = _reservedWords.getProperty(cobolName.toUpperCase(Locale.getDefault()));
        if (subst != null) {
            if (subst.length() == 0) {
                cobolName = RESERVED_PREFIX + cobolName;
            } else {
                cobolName = subst;
            }
        }

        return cobolName;

    }

    /**
     * Creates a new string built from each valid character from name.
     * @param name the proposed name with potentially invalid characters
     * @return a name that is guaranteed to contain only valid characters
     */
    private String switchCharacters(final String name) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < name.length(); i++) {
            boolean valid = false;
            for (int j = 0; j < mC.length; j++) {
                if (name.charAt(i) == mC[j]) {
                    sb.append(name.charAt(i));
                    valid = true;
                    break;
                }
            }
            /* Ignore invalid characters unless it is an
             * underscore */
            if (!valid && name.charAt(i) == '_') {
                sb.append('-');
            }
        }

        /* Check first and last characters */
        while (sb.length() > 0 && !isValidFirst(sb.charAt(0))) {
            sb.deleteCharAt(0);
        }
        while (sb.length() > 0 && !isValidLast(sb.charAt(sb.length() - 1))) {
            sb.deleteCharAt(sb.length() - 1);
        }
        return sb.toString();
    }

    /**
     * Determines if this is a valid first character in a cobol word.
     * @param c the character to check
     * @return true if this is a valid first character
     */
    private boolean isValidFirst(final char c) {
        if (c == '-' || Character.isDigit(c)) {
            return false;
        }
        return true;
    }

    /**
     * Determines if this is a valid last character in a cobol word.
     * @param c the character to check
     * @return true if this is a valid last character
     */
    private boolean isValidLast(final char c) {
        if (c == '-') {
            return false;
        }
        return true;
    }
    /**
     * Look up a proposed name in the used names list. If it is already
     * used, a new name is built with a numeric suffix meant to disambiguate.
     * @param name the name to check for unicity
     * @return a unique name within the context of this resolver
     */
    private String makeUnique(final String name) {
        int sfx = 0;
        String s = name;
        while (_usedNames.contains(s)) {
            String suffix = Integer.toString(sfx);
            if ((name + suffix).length() > MAX_COBOLNAME_LEN) {
                s = name.substring(
                        0, MAX_COBOLNAME_LEN - suffix.length())
                        + suffix;
            } else {
                s = name + suffix;
            }
            sfx++;
        }
        /* Keep it in the used list */
        _usedNames.add(s);
        return s;
    }

}
