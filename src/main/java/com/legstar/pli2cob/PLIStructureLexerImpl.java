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

import org.antlr.runtime.CharStream;
import org.antlr.runtime.NoViableAltException;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.Token;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Overrides some of the ANTLR generated lexer methods so that the resulting
 * java class behaves like other LegStar classes, particularly for logging purposes.
 * <p/>
 * This code could be imbedded in the lexer grammar as well but its harder to
 * debug using ANTLRWorks because this code might have dependencies on jars which
 * are not naturally in ANTLRWorks classpath.
 *
 */
public class PLIStructureLexerImpl extends PLIStructureLexer {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct from a character stream.
     * @param input the character stream
     */
    public PLIStructureLexerImpl(final CharStream input) {
        this(input, new RecognizerSharedState());
    }
    
    /**
     * Construct from a character stream and a shared state.
     * @param input the character stream
     * @param state the shared state
     */
    public PLIStructureLexerImpl(final CharStream input, final RecognizerSharedState state) {
        super(input, state);

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
