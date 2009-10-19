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
// $ANTLR 3.1.3 Mar 17, 2009 19:23:44 D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g 2009-10-09 09:10:11

package com.legstar.pli2cob;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class PLIStructureLexer extends Lexer {
    public static final int SIGN=37;
    public static final int DATA_ITEM_NAME=40;
    public static final int DIMENSION_KEYWORD=23;
    public static final int LT=51;
    public static final int EXPONENT=47;
    public static final int DECIMAL_KEYWORD=10;
    public static final int CONTROLLED_KEYWORD=30;
    public static final int WIDECHAR_KEYWORD=16;
    public static final int LETTER=38;
    public static final int MULTI_COMMENT=56;
    public static final int SIGNED_KEYWORD=13;
    public static final int VARYINGZ_KEYWORD=21;
    public static final int GRAPHIC_KEYWORD=17;
    public static final int REAL_KEYWORD=5;
    public static final int VARYING_KEYWORD=20;
    public static final int AUTOMATIC_KEYWORD=27;
    public static final int EQUALS=50;
    public static final int FLOAT=48;
    public static final int CHARACTER_KEYWORD=15;
    public static final int UNALIGNED_KEYWORD=25;
    public static final int SPACE=53;
    public static final int EOF=-1;
    public static final int BREAK=62;
    public static final int BINARY_KEYWORD=11;
    public static final int FLOAT_KEYWORD=8;
    public static final int ASTERISK=49;
    public static final int UNION_KEYWORD=31;
    public static final int QUOTE=63;
    public static final int RIGHT_PAREN=33;
    public static final int UNSIGNED_INTEGER=44;
    public static final int STRING_LITERAL=43;
    public static final int COMMA=34;
    public static final int SIGNED_INTEGER=45;
    public static final int COLUMN=36;
    public static final int ALIGNED_KEYWORD=24;
    public static final int APOSTROPHE=64;
    public static final int DIGIT=39;
    public static final int DOT=57;
    public static final int INITIAL_KEYWORD=26;
    public static final int FIXED_KEYWORD=7;
    public static final int PICTURE_KEYWORD=22;
    public static final int SYMBOL=58;
    public static final int WHITESPACE=54;
    public static final int REFER_KEYWORD=18;
    public static final int SEMICOLON=35;
    public static final int NONVARYING_KEYWORD=19;
    public static final int UNSIGNED_KEYWORD=14;
    public static final int STATIC_KEYWORD=28;
    public static final int NEWLINE=55;
    public static final int STRING_DELIMITER=41;
    public static final int NONCONTROL_CHAR=42;
    public static final int COMPLEX_KEYWORD=6;
    public static final int BASED_KEYWORD=29;
    public static final int LEFT_PAREN=32;
    public static final int GT=52;
    public static final int DECLARE_KEYWORD=4;
    public static final int LOWER=60;
    public static final int FRACTION=46;
    public static final int PRECISION_KEYWORD=9;
    public static final int BIT_KEYWORD=12;
    public static final int UPPER=61;
    public static final int HEXDIGIT=59;

        /** Logger. */
        private final Log _log = LogFactory.getLog(getClass());

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


    // delegates
    // delegators

    public PLIStructureLexer() {;} 
    public PLIStructureLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public PLIStructureLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g"; }

    // $ANTLR start "DECLARE_KEYWORD"
    public final void mDECLARE_KEYWORD() throws RecognitionException {
    traceIn("DECLARE_KEYWORD", 1);
        try {
            int _type = DECLARE_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:16: ( 'DECLARE' | 'declare' | 'Declare' | 'DCL' | 'dcl' | 'Dcl' )
            int alt1=6;
            int LA1_0 = input.LA(1);

            if ( (LA1_0=='D') ) {
                switch ( input.LA(2) ) {
                case 'E':
                    {
                    alt1=1;
                    }
                    break;
                case 'e':
                    {
                    alt1=3;
                    }
                    break;
                case 'C':
                    {
                    alt1=4;
                    }
                    break;
                case 'c':
                    {
                    alt1=6;
                    }
                    break;
                default:
                    NoViableAltException nvae =
                        new NoViableAltException("", 1, 1, input);

                    throw nvae;
                }

            }
            else if ( (LA1_0=='d') ) {
                int LA1_2 = input.LA(2);

                if ( (LA1_2=='e') ) {
                    alt1=2;
                }
                else if ( (LA1_2=='c') ) {
                    alt1=5;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 1, 2, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:18: 'DECLARE'
                    {
                    match("DECLARE"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:30: 'declare'
                    {
                    match("declare"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:42: 'Declare'
                    {
                    match("Declare"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:54: 'DCL'
                    {
                    match("DCL"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:62: 'dcl'
                    {
                    match("dcl"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:55:70: 'Dcl'
                    {
                    match("Dcl"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("DECLARE_KEYWORD", 1);
        }
    }
    // $ANTLR end "DECLARE_KEYWORD"

    // $ANTLR start "REAL_KEYWORD"
    public final void mREAL_KEYWORD() throws RecognitionException {
    traceIn("REAL_KEYWORD", 2);
        try {
            int _type = REAL_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:56:13: ( 'REAL' | 'real' | 'Real' )
            int alt2=3;
            int LA2_0 = input.LA(1);

            if ( (LA2_0=='R') ) {
                int LA2_1 = input.LA(2);

                if ( (LA2_1=='E') ) {
                    alt2=1;
                }
                else if ( (LA2_1=='e') ) {
                    alt2=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 2, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA2_0=='r') ) {
                alt2=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:56:15: 'REAL'
                    {
                    match("REAL"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:56:24: 'real'
                    {
                    match("real"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:56:33: 'Real'
                    {
                    match("Real"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("REAL_KEYWORD", 2);
        }
    }
    // $ANTLR end "REAL_KEYWORD"

    // $ANTLR start "COMPLEX_KEYWORD"
    public final void mCOMPLEX_KEYWORD() throws RecognitionException {
    traceIn("COMPLEX_KEYWORD", 3);
        try {
            int _type = COMPLEX_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:16: ( 'COMPLEX' | 'complex' | 'Complex' | 'CPLX' | 'cplx' | 'Cplx' )
            int alt3=6;
            int LA3_0 = input.LA(1);

            if ( (LA3_0=='C') ) {
                switch ( input.LA(2) ) {
                case 'O':
                    {
                    alt3=1;
                    }
                    break;
                case 'o':
                    {
                    alt3=3;
                    }
                    break;
                case 'P':
                    {
                    alt3=4;
                    }
                    break;
                case 'p':
                    {
                    alt3=6;
                    }
                    break;
                default:
                    NoViableAltException nvae =
                        new NoViableAltException("", 3, 1, input);

                    throw nvae;
                }

            }
            else if ( (LA3_0=='c') ) {
                int LA3_2 = input.LA(2);

                if ( (LA3_2=='o') ) {
                    alt3=2;
                }
                else if ( (LA3_2=='p') ) {
                    alt3=5;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 3, 2, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:18: 'COMPLEX'
                    {
                    match("COMPLEX"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:30: 'complex'
                    {
                    match("complex"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:42: 'Complex'
                    {
                    match("Complex"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:54: 'CPLX'
                    {
                    match("CPLX"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:63: 'cplx'
                    {
                    match("cplx"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:57:72: 'Cplx'
                    {
                    match("Cplx"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("COMPLEX_KEYWORD", 3);
        }
    }
    // $ANTLR end "COMPLEX_KEYWORD"

    // $ANTLR start "FIXED_KEYWORD"
    public final void mFIXED_KEYWORD() throws RecognitionException {
    traceIn("FIXED_KEYWORD", 4);
        try {
            int _type = FIXED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:58:14: ( 'FIXED' | 'fixed' | 'Fixed' )
            int alt4=3;
            int LA4_0 = input.LA(1);

            if ( (LA4_0=='F') ) {
                int LA4_1 = input.LA(2);

                if ( (LA4_1=='I') ) {
                    alt4=1;
                }
                else if ( (LA4_1=='i') ) {
                    alt4=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 4, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA4_0=='f') ) {
                alt4=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:58:16: 'FIXED'
                    {
                    match("FIXED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:58:26: 'fixed'
                    {
                    match("fixed"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:58:36: 'Fixed'
                    {
                    match("Fixed"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("FIXED_KEYWORD", 4);
        }
    }
    // $ANTLR end "FIXED_KEYWORD"

    // $ANTLR start "FLOAT_KEYWORD"
    public final void mFLOAT_KEYWORD() throws RecognitionException {
    traceIn("FLOAT_KEYWORD", 5);
        try {
            int _type = FLOAT_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:59:14: ( 'FLOAT' | 'float' | 'Float' )
            int alt5=3;
            int LA5_0 = input.LA(1);

            if ( (LA5_0=='F') ) {
                int LA5_1 = input.LA(2);

                if ( (LA5_1=='L') ) {
                    alt5=1;
                }
                else if ( (LA5_1=='l') ) {
                    alt5=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 5, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA5_0=='f') ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:59:16: 'FLOAT'
                    {
                    match("FLOAT"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:59:26: 'float'
                    {
                    match("float"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:59:36: 'Float'
                    {
                    match("Float"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("FLOAT_KEYWORD", 5);
        }
    }
    // $ANTLR end "FLOAT_KEYWORD"

    // $ANTLR start "PRECISION_KEYWORD"
    public final void mPRECISION_KEYWORD() throws RecognitionException {
    traceIn("PRECISION_KEYWORD", 6);
        try {
            int _type = PRECISION_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:18: ( 'PRECISION' | 'precision' | 'Precision' | 'PREC' | 'prec' | 'Prec' )
            int alt6=6;
            alt6 = dfa6.predict(input);
            switch (alt6) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:20: 'PRECISION'
                    {
                    match("PRECISION"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:34: 'precision'
                    {
                    match("precision"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:48: 'Precision'
                    {
                    match("Precision"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:62: 'PREC'
                    {
                    match("PREC"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:71: 'prec'
                    {
                    match("prec"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:60:80: 'Prec'
                    {
                    match("Prec"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("PRECISION_KEYWORD", 6);
        }
    }
    // $ANTLR end "PRECISION_KEYWORD"

    // $ANTLR start "DECIMAL_KEYWORD"
    public final void mDECIMAL_KEYWORD() throws RecognitionException {
    traceIn("DECIMAL_KEYWORD", 7);
        try {
            int _type = DECIMAL_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:16: ( 'DECIMAL' | 'decimal' | 'Decimal' | 'DEC' | 'dec' | 'Dec' )
            int alt7=6;
            alt7 = dfa7.predict(input);
            switch (alt7) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:18: 'DECIMAL'
                    {
                    match("DECIMAL"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:30: 'decimal'
                    {
                    match("decimal"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:42: 'Decimal'
                    {
                    match("Decimal"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:54: 'DEC'
                    {
                    match("DEC"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:62: 'dec'
                    {
                    match("dec"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:61:70: 'Dec'
                    {
                    match("Dec"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("DECIMAL_KEYWORD", 7);
        }
    }
    // $ANTLR end "DECIMAL_KEYWORD"

    // $ANTLR start "BINARY_KEYWORD"
    public final void mBINARY_KEYWORD() throws RecognitionException {
    traceIn("BINARY_KEYWORD", 8);
        try {
            int _type = BINARY_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:15: ( 'BINARY' | 'binary' | 'Binary' | 'BIN' | 'bin' | 'Bin' )
            int alt8=6;
            alt8 = dfa8.predict(input);
            switch (alt8) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:17: 'BINARY'
                    {
                    match("BINARY"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:28: 'binary'
                    {
                    match("binary"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:39: 'Binary'
                    {
                    match("Binary"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:50: 'BIN'
                    {
                    match("BIN"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:58: 'bin'
                    {
                    match("bin"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:62:66: 'Bin'
                    {
                    match("Bin"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("BINARY_KEYWORD", 8);
        }
    }
    // $ANTLR end "BINARY_KEYWORD"

    // $ANTLR start "BIT_KEYWORD"
    public final void mBIT_KEYWORD() throws RecognitionException {
    traceIn("BIT_KEYWORD", 9);
        try {
            int _type = BIT_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:63:12: ( 'BIT' | 'bit' | 'Bit' )
            int alt9=3;
            int LA9_0 = input.LA(1);

            if ( (LA9_0=='B') ) {
                int LA9_1 = input.LA(2);

                if ( (LA9_1=='I') ) {
                    alt9=1;
                }
                else if ( (LA9_1=='i') ) {
                    alt9=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 9, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA9_0=='b') ) {
                alt9=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 9, 0, input);

                throw nvae;
            }
            switch (alt9) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:63:14: 'BIT'
                    {
                    match("BIT"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:63:22: 'bit'
                    {
                    match("bit"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:63:30: 'Bit'
                    {
                    match("Bit"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("BIT_KEYWORD", 9);
        }
    }
    // $ANTLR end "BIT_KEYWORD"

    // $ANTLR start "SIGNED_KEYWORD"
    public final void mSIGNED_KEYWORD() throws RecognitionException {
    traceIn("SIGNED_KEYWORD", 10);
        try {
            int _type = SIGNED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:64:15: ( 'SIGNED' | 'signed' | 'Signed' )
            int alt10=3;
            int LA10_0 = input.LA(1);

            if ( (LA10_0=='S') ) {
                int LA10_1 = input.LA(2);

                if ( (LA10_1=='I') ) {
                    alt10=1;
                }
                else if ( (LA10_1=='i') ) {
                    alt10=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 10, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA10_0=='s') ) {
                alt10=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 10, 0, input);

                throw nvae;
            }
            switch (alt10) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:64:17: 'SIGNED'
                    {
                    match("SIGNED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:64:28: 'signed'
                    {
                    match("signed"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:64:39: 'Signed'
                    {
                    match("Signed"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("SIGNED_KEYWORD", 10);
        }
    }
    // $ANTLR end "SIGNED_KEYWORD"

    // $ANTLR start "UNSIGNED_KEYWORD"
    public final void mUNSIGNED_KEYWORD() throws RecognitionException {
    traceIn("UNSIGNED_KEYWORD", 11);
        try {
            int _type = UNSIGNED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:65:17: ( 'UNSIGNED' | 'unsigned' | 'Unsigned' )
            int alt11=3;
            int LA11_0 = input.LA(1);

            if ( (LA11_0=='U') ) {
                int LA11_1 = input.LA(2);

                if ( (LA11_1=='N') ) {
                    alt11=1;
                }
                else if ( (LA11_1=='n') ) {
                    alt11=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 11, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA11_0=='u') ) {
                alt11=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 11, 0, input);

                throw nvae;
            }
            switch (alt11) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:65:19: 'UNSIGNED'
                    {
                    match("UNSIGNED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:65:32: 'unsigned'
                    {
                    match("unsigned"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:65:45: 'Unsigned'
                    {
                    match("Unsigned"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("UNSIGNED_KEYWORD", 11);
        }
    }
    // $ANTLR end "UNSIGNED_KEYWORD"

    // $ANTLR start "CHARACTER_KEYWORD"
    public final void mCHARACTER_KEYWORD() throws RecognitionException {
    traceIn("CHARACTER_KEYWORD", 12);
        try {
            int _type = CHARACTER_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:18: ( 'CHARACTER' | 'character' | 'Character' | 'CHAR' | 'char' | 'Char' )
            int alt12=6;
            alt12 = dfa12.predict(input);
            switch (alt12) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:20: 'CHARACTER'
                    {
                    match("CHARACTER"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:34: 'character'
                    {
                    match("character"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:48: 'Character'
                    {
                    match("Character"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:62: 'CHAR'
                    {
                    match("CHAR"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:71: 'char'
                    {
                    match("char"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:66:80: 'Char'
                    {
                    match("Char"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("CHARACTER_KEYWORD", 12);
        }
    }
    // $ANTLR end "CHARACTER_KEYWORD"

    // $ANTLR start "WIDECHAR_KEYWORD"
    public final void mWIDECHAR_KEYWORD() throws RecognitionException {
    traceIn("WIDECHAR_KEYWORD", 13);
        try {
            int _type = WIDECHAR_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:17: ( 'WIDECHAR' | 'widechar' | 'Widechar' | 'WCHAR' | 'wchar' | 'Wchar' )
            int alt13=6;
            int LA13_0 = input.LA(1);

            if ( (LA13_0=='W') ) {
                switch ( input.LA(2) ) {
                case 'I':
                    {
                    alt13=1;
                    }
                    break;
                case 'i':
                    {
                    alt13=3;
                    }
                    break;
                case 'C':
                    {
                    alt13=4;
                    }
                    break;
                case 'c':
                    {
                    alt13=6;
                    }
                    break;
                default:
                    NoViableAltException nvae =
                        new NoViableAltException("", 13, 1, input);

                    throw nvae;
                }

            }
            else if ( (LA13_0=='w') ) {
                int LA13_2 = input.LA(2);

                if ( (LA13_2=='i') ) {
                    alt13=2;
                }
                else if ( (LA13_2=='c') ) {
                    alt13=5;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 13, 2, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 13, 0, input);

                throw nvae;
            }
            switch (alt13) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:19: 'WIDECHAR'
                    {
                    match("WIDECHAR"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:32: 'widechar'
                    {
                    match("widechar"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:45: 'Widechar'
                    {
                    match("Widechar"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:58: 'WCHAR'
                    {
                    match("WCHAR"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:68: 'wchar'
                    {
                    match("wchar"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:67:78: 'Wchar'
                    {
                    match("Wchar"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("WIDECHAR_KEYWORD", 13);
        }
    }
    // $ANTLR end "WIDECHAR_KEYWORD"

    // $ANTLR start "GRAPHIC_KEYWORD"
    public final void mGRAPHIC_KEYWORD() throws RecognitionException {
    traceIn("GRAPHIC_KEYWORD", 14);
        try {
            int _type = GRAPHIC_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:16: ( 'GRAPHIC' | 'graphic' | 'Graphic' | 'G' | 'g' )
            int alt14=5;
            int LA14_0 = input.LA(1);

            if ( (LA14_0=='G') ) {
                switch ( input.LA(2) ) {
                case 'R':
                    {
                    alt14=1;
                    }
                    break;
                case 'r':
                    {
                    alt14=3;
                    }
                    break;
                default:
                    alt14=4;}

            }
            else if ( (LA14_0=='g') ) {
                int LA14_2 = input.LA(2);

                if ( (LA14_2=='r') ) {
                    alt14=2;
                }
                else {
                    alt14=5;}
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 14, 0, input);

                throw nvae;
            }
            switch (alt14) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:18: 'GRAPHIC'
                    {
                    match("GRAPHIC"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:30: 'graphic'
                    {
                    match("graphic"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:42: 'Graphic'
                    {
                    match("Graphic"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:54: 'G'
                    {
                    match('G'); 

                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:68:60: 'g'
                    {
                    match('g'); 

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("GRAPHIC_KEYWORD", 14);
        }
    }
    // $ANTLR end "GRAPHIC_KEYWORD"

    // $ANTLR start "REFER_KEYWORD"
    public final void mREFER_KEYWORD() throws RecognitionException {
    traceIn("REFER_KEYWORD", 15);
        try {
            int _type = REFER_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:69:14: ( 'REFER' | 'refer' | 'Refer' )
            int alt15=3;
            int LA15_0 = input.LA(1);

            if ( (LA15_0=='R') ) {
                int LA15_1 = input.LA(2);

                if ( (LA15_1=='E') ) {
                    alt15=1;
                }
                else if ( (LA15_1=='e') ) {
                    alt15=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 15, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA15_0=='r') ) {
                alt15=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 15, 0, input);

                throw nvae;
            }
            switch (alt15) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:69:16: 'REFER'
                    {
                    match("REFER"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:69:26: 'refer'
                    {
                    match("refer"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:69:36: 'Refer'
                    {
                    match("Refer"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("REFER_KEYWORD", 15);
        }
    }
    // $ANTLR end "REFER_KEYWORD"

    // $ANTLR start "NONVARYING_KEYWORD"
    public final void mNONVARYING_KEYWORD() throws RecognitionException {
    traceIn("NONVARYING_KEYWORD", 16);
        try {
            int _type = NONVARYING_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:19: ( 'NONVARYING' | 'nonvarying' | 'Nonvarying' | 'NONVAR' | 'nonvar' | 'Nonvar' )
            int alt16=6;
            alt16 = dfa16.predict(input);
            switch (alt16) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:21: 'NONVARYING'
                    {
                    match("NONVARYING"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:36: 'nonvarying'
                    {
                    match("nonvarying"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:51: 'Nonvarying'
                    {
                    match("Nonvarying"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:66: 'NONVAR'
                    {
                    match("NONVAR"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:77: 'nonvar'
                    {
                    match("nonvar"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:70:88: 'Nonvar'
                    {
                    match("Nonvar"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("NONVARYING_KEYWORD", 16);
        }
    }
    // $ANTLR end "NONVARYING_KEYWORD"

    // $ANTLR start "VARYING_KEYWORD"
    public final void mVARYING_KEYWORD() throws RecognitionException {
    traceIn("VARYING_KEYWORD", 17);
        try {
            int _type = VARYING_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:16: ( 'VARYING' | 'varying' | 'Varying' | 'VAR' | 'var' | 'Var' )
            int alt17=6;
            alt17 = dfa17.predict(input);
            switch (alt17) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:18: 'VARYING'
                    {
                    match("VARYING"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:30: 'varying'
                    {
                    match("varying"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:42: 'Varying'
                    {
                    match("Varying"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:54: 'VAR'
                    {
                    match("VAR"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:62: 'var'
                    {
                    match("var"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:71:70: 'Var'
                    {
                    match("Var"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("VARYING_KEYWORD", 17);
        }
    }
    // $ANTLR end "VARYING_KEYWORD"

    // $ANTLR start "VARYINGZ_KEYWORD"
    public final void mVARYINGZ_KEYWORD() throws RecognitionException {
    traceIn("VARYINGZ_KEYWORD", 18);
        try {
            int _type = VARYINGZ_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:17: ( 'VARYINGZ' | 'varyingz' | 'Varyingz' | 'VARZ' | 'varz' | 'Varz' )
            int alt18=6;
            alt18 = dfa18.predict(input);
            switch (alt18) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:19: 'VARYINGZ'
                    {
                    match("VARYINGZ"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:32: 'varyingz'
                    {
                    match("varyingz"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:45: 'Varyingz'
                    {
                    match("Varyingz"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:59: 'VARZ'
                    {
                    match("VARZ"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:68: 'varz'
                    {
                    match("varz"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:72:77: 'Varz'
                    {
                    match("Varz"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("VARYINGZ_KEYWORD", 18);
        }
    }
    // $ANTLR end "VARYINGZ_KEYWORD"

    // $ANTLR start "PICTURE_KEYWORD"
    public final void mPICTURE_KEYWORD() throws RecognitionException {
    traceIn("PICTURE_KEYWORD", 19);
        try {
            int _type = PICTURE_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:16: ( 'PICTURE' | 'picture' | 'Picture' | 'PIC' | 'pic' | 'Pic' )
            int alt19=6;
            alt19 = dfa19.predict(input);
            switch (alt19) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:18: 'PICTURE'
                    {
                    match("PICTURE"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:30: 'picture'
                    {
                    match("picture"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:42: 'Picture'
                    {
                    match("Picture"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:55: 'PIC'
                    {
                    match("PIC"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:63: 'pic'
                    {
                    match("pic"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:73:71: 'Pic'
                    {
                    match("Pic"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("PICTURE_KEYWORD", 19);
        }
    }
    // $ANTLR end "PICTURE_KEYWORD"

    // $ANTLR start "DIMENSION_KEYWORD"
    public final void mDIMENSION_KEYWORD() throws RecognitionException {
    traceIn("DIMENSION_KEYWORD", 20);
        try {
            int _type = DIMENSION_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:18: ( 'DIMENSION' | 'dimension' | 'Dimension' | 'DIM' | 'dim' | 'Dim' )
            int alt20=6;
            alt20 = dfa20.predict(input);
            switch (alt20) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:20: 'DIMENSION'
                    {
                    match("DIMENSION"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:34: 'dimension'
                    {
                    match("dimension"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:48: 'Dimension'
                    {
                    match("Dimension"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:63: 'DIM'
                    {
                    match("DIM"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:71: 'dim'
                    {
                    match("dim"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:74:79: 'Dim'
                    {
                    match("Dim"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("DIMENSION_KEYWORD", 20);
        }
    }
    // $ANTLR end "DIMENSION_KEYWORD"

    // $ANTLR start "ALIGNED_KEYWORD"
    public final void mALIGNED_KEYWORD() throws RecognitionException {
    traceIn("ALIGNED_KEYWORD", 21);
        try {
            int _type = ALIGNED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:75:16: ( 'ALIGNED' | 'aligned' | 'Aligned' )
            int alt21=3;
            int LA21_0 = input.LA(1);

            if ( (LA21_0=='A') ) {
                int LA21_1 = input.LA(2);

                if ( (LA21_1=='L') ) {
                    alt21=1;
                }
                else if ( (LA21_1=='l') ) {
                    alt21=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 21, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA21_0=='a') ) {
                alt21=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 21, 0, input);

                throw nvae;
            }
            switch (alt21) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:75:18: 'ALIGNED'
                    {
                    match("ALIGNED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:75:30: 'aligned'
                    {
                    match("aligned"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:75:42: 'Aligned'
                    {
                    match("Aligned"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("ALIGNED_KEYWORD", 21);
        }
    }
    // $ANTLR end "ALIGNED_KEYWORD"

    // $ANTLR start "UNALIGNED_KEYWORD"
    public final void mUNALIGNED_KEYWORD() throws RecognitionException {
    traceIn("UNALIGNED_KEYWORD", 22);
        try {
            int _type = UNALIGNED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:76:18: ( 'UNALIGNED' | 'unaligned' | 'Unaligned' )
            int alt22=3;
            int LA22_0 = input.LA(1);

            if ( (LA22_0=='U') ) {
                int LA22_1 = input.LA(2);

                if ( (LA22_1=='N') ) {
                    alt22=1;
                }
                else if ( (LA22_1=='n') ) {
                    alt22=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 22, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA22_0=='u') ) {
                alt22=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 22, 0, input);

                throw nvae;
            }
            switch (alt22) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:76:20: 'UNALIGNED'
                    {
                    match("UNALIGNED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:76:34: 'unaligned'
                    {
                    match("unaligned"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:76:48: 'Unaligned'
                    {
                    match("Unaligned"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("UNALIGNED_KEYWORD", 22);
        }
    }
    // $ANTLR end "UNALIGNED_KEYWORD"

    // $ANTLR start "INITIAL_KEYWORD"
    public final void mINITIAL_KEYWORD() throws RecognitionException {
    traceIn("INITIAL_KEYWORD", 23);
        try {
            int _type = INITIAL_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:16: ( 'INITIAL' | 'initial' | 'Initial' | 'INIT' | 'Init' | 'init' )
            int alt23=6;
            alt23 = dfa23.predict(input);
            switch (alt23) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:18: 'INITIAL'
                    {
                    match("INITIAL"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:30: 'initial'
                    {
                    match("initial"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:42: 'Initial'
                    {
                    match("Initial"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:54: 'INIT'
                    {
                    match("INIT"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:63: 'Init'
                    {
                    match("Init"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:77:72: 'init'
                    {
                    match("init"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("INITIAL_KEYWORD", 23);
        }
    }
    // $ANTLR end "INITIAL_KEYWORD"

    // $ANTLR start "AUTOMATIC_KEYWORD"
    public final void mAUTOMATIC_KEYWORD() throws RecognitionException {
    traceIn("AUTOMATIC_KEYWORD", 24);
        try {
            int _type = AUTOMATIC_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:18: ( 'AUTOMATIC' | 'automatic' | 'Automatic' | 'AUTO' | 'Auto' | 'auto' )
            int alt24=6;
            alt24 = dfa24.predict(input);
            switch (alt24) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:20: 'AUTOMATIC'
                    {
                    match("AUTOMATIC"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:34: 'automatic'
                    {
                    match("automatic"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:48: 'Automatic'
                    {
                    match("Automatic"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:62: 'AUTO'
                    {
                    match("AUTO"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:71: 'Auto'
                    {
                    match("Auto"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:78:80: 'auto'
                    {
                    match("auto"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("AUTOMATIC_KEYWORD", 24);
        }
    }
    // $ANTLR end "AUTOMATIC_KEYWORD"

    // $ANTLR start "STATIC_KEYWORD"
    public final void mSTATIC_KEYWORD() throws RecognitionException {
    traceIn("STATIC_KEYWORD", 25);
        try {
            int _type = STATIC_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:79:15: ( 'STATIC' | 'Static' | 'static' )
            int alt25=3;
            int LA25_0 = input.LA(1);

            if ( (LA25_0=='S') ) {
                int LA25_1 = input.LA(2);

                if ( (LA25_1=='T') ) {
                    alt25=1;
                }
                else if ( (LA25_1=='t') ) {
                    alt25=2;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 25, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA25_0=='s') ) {
                alt25=3;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 25, 0, input);

                throw nvae;
            }
            switch (alt25) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:79:17: 'STATIC'
                    {
                    match("STATIC"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:79:28: 'Static'
                    {
                    match("Static"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:79:39: 'static'
                    {
                    match("static"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("STATIC_KEYWORD", 25);
        }
    }
    // $ANTLR end "STATIC_KEYWORD"

    // $ANTLR start "BASED_KEYWORD"
    public final void mBASED_KEYWORD() throws RecognitionException {
    traceIn("BASED_KEYWORD", 26);
        try {
            int _type = BASED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:80:14: ( 'BASED' | 'Based' | 'based' )
            int alt26=3;
            int LA26_0 = input.LA(1);

            if ( (LA26_0=='B') ) {
                int LA26_1 = input.LA(2);

                if ( (LA26_1=='A') ) {
                    alt26=1;
                }
                else if ( (LA26_1=='a') ) {
                    alt26=2;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 26, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA26_0=='b') ) {
                alt26=3;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 26, 0, input);

                throw nvae;
            }
            switch (alt26) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:80:16: 'BASED'
                    {
                    match("BASED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:80:26: 'Based'
                    {
                    match("Based"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:80:36: 'based'
                    {
                    match("based"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("BASED_KEYWORD", 26);
        }
    }
    // $ANTLR end "BASED_KEYWORD"

    // $ANTLR start "CONTROLLED_KEYWORD"
    public final void mCONTROLLED_KEYWORD() throws RecognitionException {
    traceIn("CONTROLLED_KEYWORD", 27);
        try {
            int _type = CONTROLLED_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:19: ( 'CONTROLLED' | 'controlled' | 'Controlled' | 'CTL' | 'ctl' | 'Ctl' )
            int alt27=6;
            int LA27_0 = input.LA(1);

            if ( (LA27_0=='C') ) {
                switch ( input.LA(2) ) {
                case 'O':
                    {
                    alt27=1;
                    }
                    break;
                case 'o':
                    {
                    alt27=3;
                    }
                    break;
                case 'T':
                    {
                    alt27=4;
                    }
                    break;
                case 't':
                    {
                    alt27=6;
                    }
                    break;
                default:
                    NoViableAltException nvae =
                        new NoViableAltException("", 27, 1, input);

                    throw nvae;
                }

            }
            else if ( (LA27_0=='c') ) {
                int LA27_2 = input.LA(2);

                if ( (LA27_2=='o') ) {
                    alt27=2;
                }
                else if ( (LA27_2=='t') ) {
                    alt27=5;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 27, 2, input);

                    throw nvae;
                }
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 27, 0, input);

                throw nvae;
            }
            switch (alt27) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:21: 'CONTROLLED'
                    {
                    match("CONTROLLED"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:36: 'controlled'
                    {
                    match("controlled"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:51: 'Controlled'
                    {
                    match("Controlled"); 


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:66: 'CTL'
                    {
                    match("CTL"); 


                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:74: 'ctl'
                    {
                    match("ctl"); 


                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:81:82: 'Ctl'
                    {
                    match("Ctl"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("CONTROLLED_KEYWORD", 27);
        }
    }
    // $ANTLR end "CONTROLLED_KEYWORD"

    // $ANTLR start "UNION_KEYWORD"
    public final void mUNION_KEYWORD() throws RecognitionException {
    traceIn("UNION_KEYWORD", 28);
        try {
            int _type = UNION_KEYWORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:82:14: ( 'UNION' | 'union' | 'Union' )
            int alt28=3;
            int LA28_0 = input.LA(1);

            if ( (LA28_0=='U') ) {
                int LA28_1 = input.LA(2);

                if ( (LA28_1=='N') ) {
                    alt28=1;
                }
                else if ( (LA28_1=='n') ) {
                    alt28=3;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 28, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA28_0=='u') ) {
                alt28=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 28, 0, input);

                throw nvae;
            }
            switch (alt28) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:82:16: 'UNION'
                    {
                    match("UNION"); 


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:82:26: 'union'
                    {
                    match("union"); 


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:82:36: 'Union'
                    {
                    match("Union"); 


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("UNION_KEYWORD", 28);
        }
    }
    // $ANTLR end "UNION_KEYWORD"

    // $ANTLR start "LEFT_PAREN"
    public final void mLEFT_PAREN() throws RecognitionException {
    traceIn("LEFT_PAREN", 29);
        try {
            int _type = LEFT_PAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:87:11: ( '(' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:87:13: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("LEFT_PAREN", 29);
        }
    }
    // $ANTLR end "LEFT_PAREN"

    // $ANTLR start "RIGHT_PAREN"
    public final void mRIGHT_PAREN() throws RecognitionException {
    traceIn("RIGHT_PAREN", 30);
        try {
            int _type = RIGHT_PAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:88:12: ( ')' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:88:14: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("RIGHT_PAREN", 30);
        }
    }
    // $ANTLR end "RIGHT_PAREN"

    // $ANTLR start "COMMA"
    public final void mCOMMA() throws RecognitionException {
    traceIn("COMMA", 31);
        try {
            int _type = COMMA;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:89:6: ( ',' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:89:8: ','
            {
            match(','); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("COMMA", 31);
        }
    }
    // $ANTLR end "COMMA"

    // $ANTLR start "SEMICOLON"
    public final void mSEMICOLON() throws RecognitionException {
    traceIn("SEMICOLON", 32);
        try {
            int _type = SEMICOLON;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:90:10: ( ';' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:90:12: ';'
            {
            match(';'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("SEMICOLON", 32);
        }
    }
    // $ANTLR end "SEMICOLON"

    // $ANTLR start "COLUMN"
    public final void mCOLUMN() throws RecognitionException {
    traceIn("COLUMN", 33);
        try {
            int _type = COLUMN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:91:7: ( ':' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:91:9: ':'
            {
            match(':'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("COLUMN", 33);
        }
    }
    // $ANTLR end "COLUMN"

    // $ANTLR start "SIGN"
    public final void mSIGN() throws RecognitionException {
    traceIn("SIGN", 34);
        try {
            int _type = SIGN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:92:5: ( '+' | '-' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("SIGN", 34);
        }
    }
    // $ANTLR end "SIGN"

    // $ANTLR start "DATA_ITEM_NAME"
    public final void mDATA_ITEM_NAME() throws RecognitionException {
    traceIn("DATA_ITEM_NAME", 35);
        try {
            int _type = DATA_ITEM_NAME;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:97:15: ( LETTER ( LETTER | DIGIT | '_' )* )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:97:17: LETTER ( LETTER | DIGIT | '_' )*
            {
            mLETTER(); 
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:97:24: ( LETTER | DIGIT | '_' )*
            loop29:
            do {
                int alt29=2;
                int LA29_0 = input.LA(1);

                if ( ((LA29_0>='0' && LA29_0<='9')||(LA29_0>='A' && LA29_0<='Z')||LA29_0=='_'||(LA29_0>='a' && LA29_0<='z')) ) {
                    alt29=1;
                }


                switch (alt29) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            	    {
            	    if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop29;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("DATA_ITEM_NAME", 35);
        }
    }
    // $ANTLR end "DATA_ITEM_NAME"

    // $ANTLR start "STRING_LITERAL"
    public final void mSTRING_LITERAL() throws RecognitionException {
    traceIn("STRING_LITERAL", 36);
        try {
            int _type = STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:98:15: ( STRING_DELIMITER ( NONCONTROL_CHAR )* STRING_DELIMITER )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:98:17: STRING_DELIMITER ( NONCONTROL_CHAR )* STRING_DELIMITER
            {
            mSTRING_DELIMITER(); 
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:98:34: ( NONCONTROL_CHAR )*
            loop30:
            do {
                int alt30=2;
                int LA30_0 = input.LA(1);

                if ( (LA30_0=='\t'||(LA30_0>=' ' && LA30_0<='!')||(LA30_0>='#' && LA30_0<='&')||(LA30_0>='(' && LA30_0<='~')) ) {
                    alt30=1;
                }


                switch (alt30) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:98:34: NONCONTROL_CHAR
            	    {
            	    mNONCONTROL_CHAR(); 

            	    }
            	    break;

            	default :
            	    break loop30;
                }
            } while (true);

            mSTRING_DELIMITER(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("STRING_LITERAL", 36);
        }
    }
    // $ANTLR end "STRING_LITERAL"

    // $ANTLR start "SIGNED_INTEGER"
    public final void mSIGNED_INTEGER() throws RecognitionException {
    traceIn("SIGNED_INTEGER", 37);
        try {
            int _type = SIGNED_INTEGER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:99:15: ( SIGN UNSIGNED_INTEGER )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:99:17: SIGN UNSIGNED_INTEGER
            {
            mSIGN(); 
            mUNSIGNED_INTEGER(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("SIGNED_INTEGER", 37);
        }
    }
    // $ANTLR end "SIGNED_INTEGER"

    // $ANTLR start "UNSIGNED_INTEGER"
    public final void mUNSIGNED_INTEGER() throws RecognitionException {
    traceIn("UNSIGNED_INTEGER", 38);
        try {
            int _type = UNSIGNED_INTEGER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:100:17: ( ( DIGIT )+ )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:100:19: ( DIGIT )+
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:100:19: ( DIGIT )+
            int cnt31=0;
            loop31:
            do {
                int alt31=2;
                int LA31_0 = input.LA(1);

                if ( ((LA31_0>='0' && LA31_0<='9')) ) {
                    alt31=1;
                }


                switch (alt31) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:100:19: DIGIT
            	    {
            	    mDIGIT(); 

            	    }
            	    break;

            	default :
            	    if ( cnt31 >= 1 ) break loop31;
                        EarlyExitException eee =
                            new EarlyExitException(31, input);
                        throw eee;
                }
                cnt31++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("UNSIGNED_INTEGER", 38);
        }
    }
    // $ANTLR end "UNSIGNED_INTEGER"

    // $ANTLR start "FLOAT"
    public final void mFLOAT() throws RecognitionException {
    traceIn("FLOAT", 39);
        try {
            int _type = FLOAT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:6: ( ( UNSIGNED_INTEGER | SIGNED_INTEGER ) ( FRACTION ( EXPONENT )? | EXPONENT ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:8: ( UNSIGNED_INTEGER | SIGNED_INTEGER ) ( FRACTION ( EXPONENT )? | EXPONENT )
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:8: ( UNSIGNED_INTEGER | SIGNED_INTEGER )
            int alt32=2;
            int LA32_0 = input.LA(1);

            if ( ((LA32_0>='0' && LA32_0<='9')) ) {
                alt32=1;
            }
            else if ( (LA32_0=='+'||LA32_0=='-') ) {
                alt32=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 32, 0, input);

                throw nvae;
            }
            switch (alt32) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:9: UNSIGNED_INTEGER
                    {
                    mUNSIGNED_INTEGER(); 

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:28: SIGNED_INTEGER
                    {
                    mSIGNED_INTEGER(); 

                    }
                    break;

            }

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:44: ( FRACTION ( EXPONENT )? | EXPONENT )
            int alt34=2;
            int LA34_0 = input.LA(1);

            if ( (LA34_0=='.') ) {
                alt34=1;
            }
            else if ( (LA34_0=='E'||LA34_0=='e') ) {
                alt34=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 34, 0, input);

                throw nvae;
            }
            switch (alt34) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:45: FRACTION ( EXPONENT )?
                    {
                    mFRACTION(); 
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:54: ( EXPONENT )?
                    int alt33=2;
                    int LA33_0 = input.LA(1);

                    if ( (LA33_0=='E'||LA33_0=='e') ) {
                        alt33=1;
                    }
                    switch (alt33) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:54: EXPONENT
                            {
                            mEXPONENT(); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:101:66: EXPONENT
                    {
                    mEXPONENT(); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("FLOAT", 39);
        }
    }
    // $ANTLR end "FLOAT"

    // $ANTLR start "ASTERISK"
    public final void mASTERISK() throws RecognitionException {
    traceIn("ASTERISK", 40);
        try {
            int _type = ASTERISK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:102:9: ( '*' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:102:11: '*'
            {
            match('*'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("ASTERISK", 40);
        }
    }
    // $ANTLR end "ASTERISK"

    // $ANTLR start "EQUALS"
    public final void mEQUALS() throws RecognitionException {
    traceIn("EQUALS", 41);
        try {
            int _type = EQUALS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:103:7: ( '=' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:103:9: '='
            {
            match('='); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("EQUALS", 41);
        }
    }
    // $ANTLR end "EQUALS"

    // $ANTLR start "LT"
    public final void mLT() throws RecognitionException {
    traceIn("LT", 42);
        try {
            int _type = LT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:104:3: ( '<' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:104:5: '<'
            {
            match('<'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("LT", 42);
        }
    }
    // $ANTLR end "LT"

    // $ANTLR start "GT"
    public final void mGT() throws RecognitionException {
    traceIn("GT", 43);
        try {
            int _type = GT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:105:3: ( '>' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:105:5: '>'
            {
            match('>'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("GT", 43);
        }
    }
    // $ANTLR end "GT"

    // $ANTLR start "WHITESPACE"
    public final void mWHITESPACE() throws RecognitionException {
    traceIn("WHITESPACE", 44);
        try {
            int _type = WHITESPACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:110:11: ( ( SPACE )+ )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:110:13: ( SPACE )+
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:110:13: ( SPACE )+
            int cnt35=0;
            loop35:
            do {
                int alt35=2;
                int LA35_0 = input.LA(1);

                if ( (LA35_0=='\t'||LA35_0==' ') ) {
                    alt35=1;
                }


                switch (alt35) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:110:13: SPACE
            	    {
            	    mSPACE(); 

            	    }
            	    break;

            	default :
            	    if ( cnt35 >= 1 ) break loop35;
                        EarlyExitException eee =
                            new EarlyExitException(35, input);
                        throw eee;
                }
                cnt35++;
            } while (true);

             skip(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("WHITESPACE", 44);
        }
    }
    // $ANTLR end "WHITESPACE"

    // $ANTLR start "NEWLINE"
    public final void mNEWLINE() throws RecognitionException {
    traceIn("NEWLINE", 45);
        try {
            int _type = NEWLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:8: ( ( ( '\\r' )? '\\n' )+ )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:10: ( ( '\\r' )? '\\n' )+
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:10: ( ( '\\r' )? '\\n' )+
            int cnt37=0;
            loop37:
            do {
                int alt37=2;
                int LA37_0 = input.LA(1);

                if ( (LA37_0=='\n'||LA37_0=='\r') ) {
                    alt37=1;
                }


                switch (alt37) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:11: ( '\\r' )? '\\n'
            	    {
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:11: ( '\\r' )?
            	    int alt36=2;
            	    int LA36_0 = input.LA(1);

            	    if ( (LA36_0=='\r') ) {
            	        alt36=1;
            	    }
            	    switch (alt36) {
            	        case 1 :
            	            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:111:11: '\\r'
            	            {
            	            match('\r'); 

            	            }
            	            break;

            	    }

            	    match('\n'); 

            	    }
            	    break;

            	default :
            	    if ( cnt37 >= 1 ) break loop37;
                        EarlyExitException eee =
                            new EarlyExitException(37, input);
                        throw eee;
                }
                cnt37++;
            } while (true);

             skip(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("NEWLINE", 45);
        }
    }
    // $ANTLR end "NEWLINE"

    // $ANTLR start "MULTI_COMMENT"
    public final void mMULTI_COMMENT() throws RecognitionException {
    traceIn("MULTI_COMMENT", 46);
        try {
            int _type = MULTI_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:3: ( '/*' ( . )* '*/' ( NEWLINE )? )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:5: '/*' ( . )* '*/' ( NEWLINE )?
            {
            match("/*"); 

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:10: ( . )*
            loop38:
            do {
                int alt38=2;
                int LA38_0 = input.LA(1);

                if ( (LA38_0=='*') ) {
                    int LA38_1 = input.LA(2);

                    if ( (LA38_1=='/') ) {
                        alt38=2;
                    }
                    else if ( ((LA38_1>='\u0000' && LA38_1<='.')||(LA38_1>='0' && LA38_1<='\uFFFF')) ) {
                        alt38=1;
                    }


                }
                else if ( ((LA38_0>='\u0000' && LA38_0<=')')||(LA38_0>='+' && LA38_0<='\uFFFF')) ) {
                    alt38=1;
                }


                switch (alt38) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:10: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop38;
                }
            } while (true);

            match("*/"); 

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:18: ( NEWLINE )?
            int alt39=2;
            int LA39_0 = input.LA(1);

            if ( (LA39_0=='\n'||LA39_0=='\r') ) {
                alt39=1;
            }
            switch (alt39) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:113:18: NEWLINE
                    {
                    mNEWLINE(); 

                    }
                    break;

            }

             skip(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
    traceOut("MULTI_COMMENT", 46);
        }
    }
    // $ANTLR end "MULTI_COMMENT"

    // $ANTLR start "FRACTION"
    public final void mFRACTION() throws RecognitionException {
    traceIn("FRACTION", 47);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:118:18: ( DOT UNSIGNED_INTEGER )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:118:20: DOT UNSIGNED_INTEGER
            {
            mDOT(); 
            mUNSIGNED_INTEGER(); 

            }

        }
        finally {
    traceOut("FRACTION", 47);
        }
    }
    // $ANTLR end "FRACTION"

    // $ANTLR start "EXPONENT"
    public final void mEXPONENT() throws RecognitionException {
    traceIn("EXPONENT", 48);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:119:18: ( ( 'e' | 'E' ) ( UNSIGNED_INTEGER | SIGNED_INTEGER ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:119:20: ( 'e' | 'E' ) ( UNSIGNED_INTEGER | SIGNED_INTEGER )
            {
            if ( input.LA(1)=='E'||input.LA(1)=='e' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:119:32: ( UNSIGNED_INTEGER | SIGNED_INTEGER )
            int alt40=2;
            int LA40_0 = input.LA(1);

            if ( ((LA40_0>='0' && LA40_0<='9')) ) {
                alt40=1;
            }
            else if ( (LA40_0=='+'||LA40_0=='-') ) {
                alt40=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 40, 0, input);

                throw nvae;
            }
            switch (alt40) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:119:33: UNSIGNED_INTEGER
                    {
                    mUNSIGNED_INTEGER(); 

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:119:52: SIGNED_INTEGER
                    {
                    mSIGNED_INTEGER(); 

                    }
                    break;

            }


            }

        }
        finally {
    traceOut("EXPONENT", 48);
        }
    }
    // $ANTLR end "EXPONENT"

    // $ANTLR start "NONCONTROL_CHAR"
    public final void mNONCONTROL_CHAR() throws RecognitionException {
    traceIn("NONCONTROL_CHAR", 49);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:120:25: ( LETTER | DIGIT | SYMBOL | SPACE )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( input.LA(1)=='\t'||(input.LA(1)>=' ' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='~') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("NONCONTROL_CHAR", 49);
        }
    }
    // $ANTLR end "NONCONTROL_CHAR"

    // $ANTLR start "DIGIT"
    public final void mDIGIT() throws RecognitionException {
    traceIn("DIGIT", 50);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:121:15: ( '0' .. '9' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:121:17: '0' .. '9'
            {
            matchRange('0','9'); 

            }

        }
        finally {
    traceOut("DIGIT", 50);
        }
    }
    // $ANTLR end "DIGIT"

    // $ANTLR start "HEXDIGIT"
    public final void mHEXDIGIT() throws RecognitionException {
    traceIn("HEXDIGIT", 51);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:122:18: ( ( '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:122:20: ( '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' )
            {
            if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='F')||(input.LA(1)>='a' && input.LA(1)<='f') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("HEXDIGIT", 51);
        }
    }
    // $ANTLR end "HEXDIGIT"

    // $ANTLR start "LETTER"
    public final void mLETTER() throws RecognitionException {
    traceIn("LETTER", 52);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:123:16: ( LOWER | UPPER )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("LETTER", 52);
        }
    }
    // $ANTLR end "LETTER"

    // $ANTLR start "LOWER"
    public final void mLOWER() throws RecognitionException {
    traceIn("LOWER", 53);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:124:15: ( 'a' .. 'z' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:124:17: 'a' .. 'z'
            {
            matchRange('a','z'); 

            }

        }
        finally {
    traceOut("LOWER", 53);
        }
    }
    // $ANTLR end "LOWER"

    // $ANTLR start "UPPER"
    public final void mUPPER() throws RecognitionException {
    traceIn("UPPER", 54);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:125:15: ( 'A' .. 'Z' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:125:17: 'A' .. 'Z'
            {
            matchRange('A','Z'); 

            }

        }
        finally {
    traceOut("UPPER", 54);
        }
    }
    // $ANTLR end "UPPER"

    // $ANTLR start "SPACE"
    public final void mSPACE() throws RecognitionException {
    traceIn("SPACE", 55);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:126:15: ( ' ' | '\\t' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( input.LA(1)=='\t'||input.LA(1)==' ' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("SPACE", 55);
        }
    }
    // $ANTLR end "SPACE"

    // $ANTLR start "BREAK"
    public final void mBREAK() throws RecognitionException {
    traceIn("BREAK", 56);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:127:15: ( '_' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:127:17: '_'
            {
            match('_'); 

            }

        }
        finally {
    traceOut("BREAK", 56);
        }
    }
    // $ANTLR end "BREAK"

    // $ANTLR start "DOT"
    public final void mDOT() throws RecognitionException {
    traceIn("DOT", 57);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:128:13: ( '.' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:128:15: '.'
            {
            match('.'); 

            }

        }
        finally {
    traceOut("DOT", 57);
        }
    }
    // $ANTLR end "DOT"

    // $ANTLR start "STRING_DELIMITER"
    public final void mSTRING_DELIMITER() throws RecognitionException {
    traceIn("STRING_DELIMITER", 58);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:129:26: ( QUOTE | APOSTROPHE )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( input.LA(1)=='\"'||input.LA(1)=='\'' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("STRING_DELIMITER", 58);
        }
    }
    // $ANTLR end "STRING_DELIMITER"

    // $ANTLR start "QUOTE"
    public final void mQUOTE() throws RecognitionException {
    traceIn("QUOTE", 59);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:130:15: ( '\"' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:130:17: '\"'
            {
            match('\"'); 

            }

        }
        finally {
    traceOut("QUOTE", 59);
        }
    }
    // $ANTLR end "QUOTE"

    // $ANTLR start "APOSTROPHE"
    public final void mAPOSTROPHE() throws RecognitionException {
    traceIn("APOSTROPHE", 60);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:131:20: ( '\\'' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:131:22: '\\''
            {
            match('\''); 

            }

        }
        finally {
    traceOut("APOSTROPHE", 60);
        }
    }
    // $ANTLR end "APOSTROPHE"

    // $ANTLR start "SYMBOL"
    public final void mSYMBOL() throws RecognitionException {
    traceIn("SYMBOL", 61);
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:133:16: ( '!' | '#' .. '&' | '(' .. '/' | ':' .. '@' | '[' .. '`' | '{' .. '~' )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:
            {
            if ( input.LA(1)=='!'||(input.LA(1)>='#' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='/')||(input.LA(1)>=':' && input.LA(1)<='@')||(input.LA(1)>='[' && input.LA(1)<='`')||(input.LA(1)>='{' && input.LA(1)<='~') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
    traceOut("SYMBOL", 61);
        }
    }
    // $ANTLR end "SYMBOL"

    public void mTokens() throws RecognitionException {
        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:8: ( DECLARE_KEYWORD | REAL_KEYWORD | COMPLEX_KEYWORD | FIXED_KEYWORD | FLOAT_KEYWORD | PRECISION_KEYWORD | DECIMAL_KEYWORD | BINARY_KEYWORD | BIT_KEYWORD | SIGNED_KEYWORD | UNSIGNED_KEYWORD | CHARACTER_KEYWORD | WIDECHAR_KEYWORD | GRAPHIC_KEYWORD | REFER_KEYWORD | NONVARYING_KEYWORD | VARYING_KEYWORD | VARYINGZ_KEYWORD | PICTURE_KEYWORD | DIMENSION_KEYWORD | ALIGNED_KEYWORD | UNALIGNED_KEYWORD | INITIAL_KEYWORD | AUTOMATIC_KEYWORD | STATIC_KEYWORD | BASED_KEYWORD | CONTROLLED_KEYWORD | UNION_KEYWORD | LEFT_PAREN | RIGHT_PAREN | COMMA | SEMICOLON | COLUMN | SIGN | DATA_ITEM_NAME | STRING_LITERAL | SIGNED_INTEGER | UNSIGNED_INTEGER | FLOAT | ASTERISK | EQUALS | LT | GT | WHITESPACE | NEWLINE | MULTI_COMMENT )
        int alt41=46;
        alt41 = dfa41.predict(input);
        switch (alt41) {
            case 1 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:10: DECLARE_KEYWORD
                {
                mDECLARE_KEYWORD(); 

                }
                break;
            case 2 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:26: REAL_KEYWORD
                {
                mREAL_KEYWORD(); 

                }
                break;
            case 3 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:39: COMPLEX_KEYWORD
                {
                mCOMPLEX_KEYWORD(); 

                }
                break;
            case 4 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:55: FIXED_KEYWORD
                {
                mFIXED_KEYWORD(); 

                }
                break;
            case 5 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:69: FLOAT_KEYWORD
                {
                mFLOAT_KEYWORD(); 

                }
                break;
            case 6 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:83: PRECISION_KEYWORD
                {
                mPRECISION_KEYWORD(); 

                }
                break;
            case 7 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:101: DECIMAL_KEYWORD
                {
                mDECIMAL_KEYWORD(); 

                }
                break;
            case 8 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:117: BINARY_KEYWORD
                {
                mBINARY_KEYWORD(); 

                }
                break;
            case 9 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:132: BIT_KEYWORD
                {
                mBIT_KEYWORD(); 

                }
                break;
            case 10 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:144: SIGNED_KEYWORD
                {
                mSIGNED_KEYWORD(); 

                }
                break;
            case 11 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:159: UNSIGNED_KEYWORD
                {
                mUNSIGNED_KEYWORD(); 

                }
                break;
            case 12 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:176: CHARACTER_KEYWORD
                {
                mCHARACTER_KEYWORD(); 

                }
                break;
            case 13 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:194: WIDECHAR_KEYWORD
                {
                mWIDECHAR_KEYWORD(); 

                }
                break;
            case 14 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:211: GRAPHIC_KEYWORD
                {
                mGRAPHIC_KEYWORD(); 

                }
                break;
            case 15 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:227: REFER_KEYWORD
                {
                mREFER_KEYWORD(); 

                }
                break;
            case 16 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:241: NONVARYING_KEYWORD
                {
                mNONVARYING_KEYWORD(); 

                }
                break;
            case 17 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:260: VARYING_KEYWORD
                {
                mVARYING_KEYWORD(); 

                }
                break;
            case 18 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:276: VARYINGZ_KEYWORD
                {
                mVARYINGZ_KEYWORD(); 

                }
                break;
            case 19 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:293: PICTURE_KEYWORD
                {
                mPICTURE_KEYWORD(); 

                }
                break;
            case 20 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:309: DIMENSION_KEYWORD
                {
                mDIMENSION_KEYWORD(); 

                }
                break;
            case 21 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:327: ALIGNED_KEYWORD
                {
                mALIGNED_KEYWORD(); 

                }
                break;
            case 22 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:343: UNALIGNED_KEYWORD
                {
                mUNALIGNED_KEYWORD(); 

                }
                break;
            case 23 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:361: INITIAL_KEYWORD
                {
                mINITIAL_KEYWORD(); 

                }
                break;
            case 24 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:377: AUTOMATIC_KEYWORD
                {
                mAUTOMATIC_KEYWORD(); 

                }
                break;
            case 25 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:395: STATIC_KEYWORD
                {
                mSTATIC_KEYWORD(); 

                }
                break;
            case 26 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:410: BASED_KEYWORD
                {
                mBASED_KEYWORD(); 

                }
                break;
            case 27 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:424: CONTROLLED_KEYWORD
                {
                mCONTROLLED_KEYWORD(); 

                }
                break;
            case 28 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:443: UNION_KEYWORD
                {
                mUNION_KEYWORD(); 

                }
                break;
            case 29 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:457: LEFT_PAREN
                {
                mLEFT_PAREN(); 

                }
                break;
            case 30 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:468: RIGHT_PAREN
                {
                mRIGHT_PAREN(); 

                }
                break;
            case 31 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:480: COMMA
                {
                mCOMMA(); 

                }
                break;
            case 32 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:486: SEMICOLON
                {
                mSEMICOLON(); 

                }
                break;
            case 33 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:496: COLUMN
                {
                mCOLUMN(); 

                }
                break;
            case 34 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:503: SIGN
                {
                mSIGN(); 

                }
                break;
            case 35 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:508: DATA_ITEM_NAME
                {
                mDATA_ITEM_NAME(); 

                }
                break;
            case 36 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:523: STRING_LITERAL
                {
                mSTRING_LITERAL(); 

                }
                break;
            case 37 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:538: SIGNED_INTEGER
                {
                mSIGNED_INTEGER(); 

                }
                break;
            case 38 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:553: UNSIGNED_INTEGER
                {
                mUNSIGNED_INTEGER(); 

                }
                break;
            case 39 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:570: FLOAT
                {
                mFLOAT(); 

                }
                break;
            case 40 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:576: ASTERISK
                {
                mASTERISK(); 

                }
                break;
            case 41 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:585: EQUALS
                {
                mEQUALS(); 

                }
                break;
            case 42 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:592: LT
                {
                mLT(); 

                }
                break;
            case 43 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:595: GT
                {
                mGT(); 

                }
                break;
            case 44 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:598: WHITESPACE
                {
                mWHITESPACE(); 

                }
                break;
            case 45 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:609: NEWLINE
                {
                mNEWLINE(); 

                }
                break;
            case 46 :
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureLexer.g:1:617: MULTI_COMMENT
                {
                mMULTI_COMMENT(); 

                }
                break;

        }

    }


    protected DFA6 dfa6 = new DFA6(this);
    protected DFA7 dfa7 = new DFA7(this);
    protected DFA8 dfa8 = new DFA8(this);
    protected DFA12 dfa12 = new DFA12(this);
    protected DFA16 dfa16 = new DFA16(this);
    protected DFA17 dfa17 = new DFA17(this);
    protected DFA18 dfa18 = new DFA18(this);
    protected DFA19 dfa19 = new DFA19(this);
    protected DFA20 dfa20 = new DFA20(this);
    protected DFA23 dfa23 = new DFA23(this);
    protected DFA24 dfa24 = new DFA24(this);
    protected DFA41 dfa41 = new DFA41(this);
    static final String DFA6_eotS =
        "\11\uffff\1\15\1\17\1\21\6\uffff";
    static final String DFA6_eofS =
        "\22\uffff";
    static final String DFA6_minS =
        "\1\120\1\122\1\162\1\105\2\145\1\103\2\143\1\111\2\151\6\uffff";
    static final String DFA6_maxS =
        "\1\160\2\162\1\105\2\145\1\103\2\143\1\111\2\151\6\uffff";
    static final String DFA6_acceptS =
        "\14\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA6_specialS =
        "\22\uffff}>";
    static final String[] DFA6_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\12",
            "\1\13",
            "\1\14",
            "\1\16",
            "\1\20",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA6_eot = DFA.unpackEncodedString(DFA6_eotS);
    static final short[] DFA6_eof = DFA.unpackEncodedString(DFA6_eofS);
    static final char[] DFA6_min = DFA.unpackEncodedStringToUnsignedChars(DFA6_minS);
    static final char[] DFA6_max = DFA.unpackEncodedStringToUnsignedChars(DFA6_maxS);
    static final short[] DFA6_accept = DFA.unpackEncodedString(DFA6_acceptS);
    static final short[] DFA6_special = DFA.unpackEncodedString(DFA6_specialS);
    static final short[][] DFA6_transition;

    static {
        int numStates = DFA6_transitionS.length;
        DFA6_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA6_transition[i] = DFA.unpackEncodedString(DFA6_transitionS[i]);
        }
    }

    class DFA6 extends DFA {

        public DFA6(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 6;
            this.eot = DFA6_eot;
            this.eof = DFA6_eof;
            this.min = DFA6_min;
            this.max = DFA6_max;
            this.accept = DFA6_accept;
            this.special = DFA6_special;
            this.transition = DFA6_transition;
        }
        public String getDescription() {
            return "60:1: PRECISION_KEYWORD : ( 'PRECISION' | 'precision' | 'Precision' | 'PREC' | 'prec' | 'Prec' );";
        }
    }
    static final String DFA7_eotS =
        "\6\uffff\1\12\1\14\1\16\6\uffff";
    static final String DFA7_eofS =
        "\17\uffff";
    static final String DFA7_minS =
        "\1\104\1\105\1\145\1\103\2\143\1\111\2\151\6\uffff";
    static final String DFA7_maxS =
        "\1\144\2\145\1\103\2\143\1\111\2\151\6\uffff";
    static final String DFA7_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA7_specialS =
        "\17\uffff}>";
    static final String[] DFA7_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\13",
            "\1\15",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA7_eot = DFA.unpackEncodedString(DFA7_eotS);
    static final short[] DFA7_eof = DFA.unpackEncodedString(DFA7_eofS);
    static final char[] DFA7_min = DFA.unpackEncodedStringToUnsignedChars(DFA7_minS);
    static final char[] DFA7_max = DFA.unpackEncodedStringToUnsignedChars(DFA7_maxS);
    static final short[] DFA7_accept = DFA.unpackEncodedString(DFA7_acceptS);
    static final short[] DFA7_special = DFA.unpackEncodedString(DFA7_specialS);
    static final short[][] DFA7_transition;

    static {
        int numStates = DFA7_transitionS.length;
        DFA7_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA7_transition[i] = DFA.unpackEncodedString(DFA7_transitionS[i]);
        }
    }

    class DFA7 extends DFA {

        public DFA7(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 7;
            this.eot = DFA7_eot;
            this.eof = DFA7_eof;
            this.min = DFA7_min;
            this.max = DFA7_max;
            this.accept = DFA7_accept;
            this.special = DFA7_special;
            this.transition = DFA7_transition;
        }
        public String getDescription() {
            return "61:1: DECIMAL_KEYWORD : ( 'DECIMAL' | 'decimal' | 'Decimal' | 'DEC' | 'dec' | 'Dec' );";
        }
    }
    static final String DFA8_eotS =
        "\6\uffff\1\12\1\14\1\16\6\uffff";
    static final String DFA8_eofS =
        "\17\uffff";
    static final String DFA8_minS =
        "\1\102\1\111\1\151\1\116\2\156\1\101\2\141\6\uffff";
    static final String DFA8_maxS =
        "\1\142\2\151\1\116\2\156\1\101\2\141\6\uffff";
    static final String DFA8_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA8_specialS =
        "\17\uffff}>";
    static final String[] DFA8_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\13",
            "\1\15",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA8_eot = DFA.unpackEncodedString(DFA8_eotS);
    static final short[] DFA8_eof = DFA.unpackEncodedString(DFA8_eofS);
    static final char[] DFA8_min = DFA.unpackEncodedStringToUnsignedChars(DFA8_minS);
    static final char[] DFA8_max = DFA.unpackEncodedStringToUnsignedChars(DFA8_maxS);
    static final short[] DFA8_accept = DFA.unpackEncodedString(DFA8_acceptS);
    static final short[] DFA8_special = DFA.unpackEncodedString(DFA8_specialS);
    static final short[][] DFA8_transition;

    static {
        int numStates = DFA8_transitionS.length;
        DFA8_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA8_transition[i] = DFA.unpackEncodedString(DFA8_transitionS[i]);
        }
    }

    class DFA8 extends DFA {

        public DFA8(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 8;
            this.eot = DFA8_eot;
            this.eof = DFA8_eof;
            this.min = DFA8_min;
            this.max = DFA8_max;
            this.accept = DFA8_accept;
            this.special = DFA8_special;
            this.transition = DFA8_transition;
        }
        public String getDescription() {
            return "62:1: BINARY_KEYWORD : ( 'BINARY' | 'binary' | 'Binary' | 'BIN' | 'bin' | 'Bin' );";
        }
    }
    static final String DFA12_eotS =
        "\11\uffff\1\15\1\17\1\21\6\uffff";
    static final String DFA12_eofS =
        "\22\uffff";
    static final String DFA12_minS =
        "\1\103\1\110\1\150\1\101\2\141\1\122\2\162\1\101\2\141\6\uffff";
    static final String DFA12_maxS =
        "\1\143\2\150\1\101\2\141\1\122\2\162\1\101\2\141\6\uffff";
    static final String DFA12_acceptS =
        "\14\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA12_specialS =
        "\22\uffff}>";
    static final String[] DFA12_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\12",
            "\1\13",
            "\1\14",
            "\1\16",
            "\1\20",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA12_eot = DFA.unpackEncodedString(DFA12_eotS);
    static final short[] DFA12_eof = DFA.unpackEncodedString(DFA12_eofS);
    static final char[] DFA12_min = DFA.unpackEncodedStringToUnsignedChars(DFA12_minS);
    static final char[] DFA12_max = DFA.unpackEncodedStringToUnsignedChars(DFA12_maxS);
    static final short[] DFA12_accept = DFA.unpackEncodedString(DFA12_acceptS);
    static final short[] DFA12_special = DFA.unpackEncodedString(DFA12_specialS);
    static final short[][] DFA12_transition;

    static {
        int numStates = DFA12_transitionS.length;
        DFA12_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA12_transition[i] = DFA.unpackEncodedString(DFA12_transitionS[i]);
        }
    }

    class DFA12 extends DFA {

        public DFA12(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 12;
            this.eot = DFA12_eot;
            this.eof = DFA12_eof;
            this.min = DFA12_min;
            this.max = DFA12_max;
            this.accept = DFA12_accept;
            this.special = DFA12_special;
            this.transition = DFA12_transition;
        }
        public String getDescription() {
            return "66:1: CHARACTER_KEYWORD : ( 'CHARACTER' | 'character' | 'Character' | 'CHAR' | 'char' | 'Char' );";
        }
    }
    static final String DFA16_eotS =
        "\17\uffff\1\23\1\25\1\27\6\uffff";
    static final String DFA16_eofS =
        "\30\uffff";
    static final String DFA16_minS =
        "\1\116\1\117\1\157\1\116\2\156\1\126\2\166\1\101\2\141\1\122\2"+
        "\162\1\131\2\171\6\uffff";
    static final String DFA16_maxS =
        "\1\156\2\157\1\116\2\156\1\126\2\166\1\101\2\141\1\122\2\162\1"+
        "\131\2\171\6\uffff";
    static final String DFA16_acceptS =
        "\22\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA16_specialS =
        "\30\uffff}>";
    static final String[] DFA16_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\12",
            "\1\13",
            "\1\14",
            "\1\15",
            "\1\16",
            "\1\17",
            "\1\20",
            "\1\21",
            "\1\22",
            "\1\24",
            "\1\26",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA16_eot = DFA.unpackEncodedString(DFA16_eotS);
    static final short[] DFA16_eof = DFA.unpackEncodedString(DFA16_eofS);
    static final char[] DFA16_min = DFA.unpackEncodedStringToUnsignedChars(DFA16_minS);
    static final char[] DFA16_max = DFA.unpackEncodedStringToUnsignedChars(DFA16_maxS);
    static final short[] DFA16_accept = DFA.unpackEncodedString(DFA16_acceptS);
    static final short[] DFA16_special = DFA.unpackEncodedString(DFA16_specialS);
    static final short[][] DFA16_transition;

    static {
        int numStates = DFA16_transitionS.length;
        DFA16_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA16_transition[i] = DFA.unpackEncodedString(DFA16_transitionS[i]);
        }
    }

    class DFA16 extends DFA {

        public DFA16(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 16;
            this.eot = DFA16_eot;
            this.eof = DFA16_eof;
            this.min = DFA16_min;
            this.max = DFA16_max;
            this.accept = DFA16_accept;
            this.special = DFA16_special;
            this.transition = DFA16_transition;
        }
        public String getDescription() {
            return "70:1: NONVARYING_KEYWORD : ( 'NONVARYING' | 'nonvarying' | 'Nonvarying' | 'NONVAR' | 'nonvar' | 'Nonvar' );";
        }
    }
    static final String DFA17_eotS =
        "\6\uffff\1\12\1\14\1\16\6\uffff";
    static final String DFA17_eofS =
        "\17\uffff";
    static final String DFA17_minS =
        "\1\126\1\101\1\141\1\122\2\162\1\131\2\171\6\uffff";
    static final String DFA17_maxS =
        "\1\166\2\141\1\122\2\162\1\131\2\171\6\uffff";
    static final String DFA17_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA17_specialS =
        "\17\uffff}>";
    static final String[] DFA17_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\13",
            "\1\15",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA17_eot = DFA.unpackEncodedString(DFA17_eotS);
    static final short[] DFA17_eof = DFA.unpackEncodedString(DFA17_eofS);
    static final char[] DFA17_min = DFA.unpackEncodedStringToUnsignedChars(DFA17_minS);
    static final char[] DFA17_max = DFA.unpackEncodedStringToUnsignedChars(DFA17_maxS);
    static final short[] DFA17_accept = DFA.unpackEncodedString(DFA17_acceptS);
    static final short[] DFA17_special = DFA.unpackEncodedString(DFA17_specialS);
    static final short[][] DFA17_transition;

    static {
        int numStates = DFA17_transitionS.length;
        DFA17_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA17_transition[i] = DFA.unpackEncodedString(DFA17_transitionS[i]);
        }
    }

    class DFA17 extends DFA {

        public DFA17(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 17;
            this.eot = DFA17_eot;
            this.eof = DFA17_eof;
            this.min = DFA17_min;
            this.max = DFA17_max;
            this.accept = DFA17_accept;
            this.special = DFA17_special;
            this.transition = DFA17_transition;
        }
        public String getDescription() {
            return "71:1: VARYING_KEYWORD : ( 'VARYING' | 'varying' | 'Varying' | 'VAR' | 'var' | 'Var' );";
        }
    }
    static final String DFA18_eotS =
        "\17\uffff";
    static final String DFA18_eofS =
        "\17\uffff";
    static final String DFA18_minS =
        "\1\126\1\101\1\141\1\122\2\162\1\131\2\171\6\uffff";
    static final String DFA18_maxS =
        "\1\166\2\141\1\122\2\162\1\132\2\172\6\uffff";
    static final String DFA18_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA18_specialS =
        "\17\uffff}>";
    static final String[] DFA18_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11\1\12",
            "\1\13\1\14",
            "\1\15\1\16",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA18_eot = DFA.unpackEncodedString(DFA18_eotS);
    static final short[] DFA18_eof = DFA.unpackEncodedString(DFA18_eofS);
    static final char[] DFA18_min = DFA.unpackEncodedStringToUnsignedChars(DFA18_minS);
    static final char[] DFA18_max = DFA.unpackEncodedStringToUnsignedChars(DFA18_maxS);
    static final short[] DFA18_accept = DFA.unpackEncodedString(DFA18_acceptS);
    static final short[] DFA18_special = DFA.unpackEncodedString(DFA18_specialS);
    static final short[][] DFA18_transition;

    static {
        int numStates = DFA18_transitionS.length;
        DFA18_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA18_transition[i] = DFA.unpackEncodedString(DFA18_transitionS[i]);
        }
    }

    class DFA18 extends DFA {

        public DFA18(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 18;
            this.eot = DFA18_eot;
            this.eof = DFA18_eof;
            this.min = DFA18_min;
            this.max = DFA18_max;
            this.accept = DFA18_accept;
            this.special = DFA18_special;
            this.transition = DFA18_transition;
        }
        public String getDescription() {
            return "72:1: VARYINGZ_KEYWORD : ( 'VARYINGZ' | 'varyingz' | 'Varyingz' | 'VARZ' | 'varz' | 'Varz' );";
        }
    }
    static final String DFA19_eotS =
        "\6\uffff\1\12\1\14\1\16\6\uffff";
    static final String DFA19_eofS =
        "\17\uffff";
    static final String DFA19_minS =
        "\1\120\1\111\1\151\1\103\2\143\1\124\2\164\6\uffff";
    static final String DFA19_maxS =
        "\1\160\2\151\1\103\2\143\1\124\2\164\6\uffff";
    static final String DFA19_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA19_specialS =
        "\17\uffff}>";
    static final String[] DFA19_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\13",
            "\1\15",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA19_eot = DFA.unpackEncodedString(DFA19_eotS);
    static final short[] DFA19_eof = DFA.unpackEncodedString(DFA19_eofS);
    static final char[] DFA19_min = DFA.unpackEncodedStringToUnsignedChars(DFA19_minS);
    static final char[] DFA19_max = DFA.unpackEncodedStringToUnsignedChars(DFA19_maxS);
    static final short[] DFA19_accept = DFA.unpackEncodedString(DFA19_acceptS);
    static final short[] DFA19_special = DFA.unpackEncodedString(DFA19_specialS);
    static final short[][] DFA19_transition;

    static {
        int numStates = DFA19_transitionS.length;
        DFA19_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA19_transition[i] = DFA.unpackEncodedString(DFA19_transitionS[i]);
        }
    }

    class DFA19 extends DFA {

        public DFA19(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 19;
            this.eot = DFA19_eot;
            this.eof = DFA19_eof;
            this.min = DFA19_min;
            this.max = DFA19_max;
            this.accept = DFA19_accept;
            this.special = DFA19_special;
            this.transition = DFA19_transition;
        }
        public String getDescription() {
            return "73:1: PICTURE_KEYWORD : ( 'PICTURE' | 'picture' | 'Picture' | 'PIC' | 'pic' | 'Pic' );";
        }
    }
    static final String DFA20_eotS =
        "\6\uffff\1\12\1\14\1\16\6\uffff";
    static final String DFA20_eofS =
        "\17\uffff";
    static final String DFA20_minS =
        "\1\104\1\111\1\151\1\115\2\155\1\105\2\145\6\uffff";
    static final String DFA20_maxS =
        "\1\144\2\151\1\115\2\155\1\105\2\145\6\uffff";
    static final String DFA20_acceptS =
        "\11\uffff\1\1\1\4\1\3\1\6\1\2\1\5";
    static final String DFA20_specialS =
        "\17\uffff}>";
    static final String[] DFA20_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\13",
            "\1\15",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA20_eot = DFA.unpackEncodedString(DFA20_eotS);
    static final short[] DFA20_eof = DFA.unpackEncodedString(DFA20_eofS);
    static final char[] DFA20_min = DFA.unpackEncodedStringToUnsignedChars(DFA20_minS);
    static final char[] DFA20_max = DFA.unpackEncodedStringToUnsignedChars(DFA20_maxS);
    static final short[] DFA20_accept = DFA.unpackEncodedString(DFA20_acceptS);
    static final short[] DFA20_special = DFA.unpackEncodedString(DFA20_specialS);
    static final short[][] DFA20_transition;

    static {
        int numStates = DFA20_transitionS.length;
        DFA20_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA20_transition[i] = DFA.unpackEncodedString(DFA20_transitionS[i]);
        }
    }

    class DFA20 extends DFA {

        public DFA20(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 20;
            this.eot = DFA20_eot;
            this.eof = DFA20_eof;
            this.min = DFA20_min;
            this.max = DFA20_max;
            this.accept = DFA20_accept;
            this.special = DFA20_special;
            this.transition = DFA20_transition;
        }
        public String getDescription() {
            return "74:1: DIMENSION_KEYWORD : ( 'DIMENSION' | 'dimension' | 'Dimension' | 'DIM' | 'dim' | 'Dim' );";
        }
    }
    static final String DFA23_eotS =
        "\11\uffff\1\15\1\17\1\21\6\uffff";
    static final String DFA23_eofS =
        "\22\uffff";
    static final String DFA23_minS =
        "\1\111\1\116\1\156\1\111\2\151\1\124\2\164\1\111\2\151\6\uffff";
    static final String DFA23_maxS =
        "\1\151\2\156\1\111\2\151\1\124\2\164\1\111\2\151\6\uffff";
    static final String DFA23_acceptS =
        "\14\uffff\1\1\1\4\1\3\1\5\1\2\1\6";
    static final String DFA23_specialS =
        "\22\uffff}>";
    static final String[] DFA23_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\12",
            "\1\13",
            "\1\14",
            "\1\16",
            "\1\20",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA23_eot = DFA.unpackEncodedString(DFA23_eotS);
    static final short[] DFA23_eof = DFA.unpackEncodedString(DFA23_eofS);
    static final char[] DFA23_min = DFA.unpackEncodedStringToUnsignedChars(DFA23_minS);
    static final char[] DFA23_max = DFA.unpackEncodedStringToUnsignedChars(DFA23_maxS);
    static final short[] DFA23_accept = DFA.unpackEncodedString(DFA23_acceptS);
    static final short[] DFA23_special = DFA.unpackEncodedString(DFA23_specialS);
    static final short[][] DFA23_transition;

    static {
        int numStates = DFA23_transitionS.length;
        DFA23_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA23_transition[i] = DFA.unpackEncodedString(DFA23_transitionS[i]);
        }
    }

    class DFA23 extends DFA {

        public DFA23(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 23;
            this.eot = DFA23_eot;
            this.eof = DFA23_eof;
            this.min = DFA23_min;
            this.max = DFA23_max;
            this.accept = DFA23_accept;
            this.special = DFA23_special;
            this.transition = DFA23_transition;
        }
        public String getDescription() {
            return "77:1: INITIAL_KEYWORD : ( 'INITIAL' | 'initial' | 'Initial' | 'INIT' | 'Init' | 'init' );";
        }
    }
    static final String DFA24_eotS =
        "\11\uffff\1\15\1\17\1\21\6\uffff";
    static final String DFA24_eofS =
        "\22\uffff";
    static final String DFA24_minS =
        "\1\101\1\125\1\165\1\124\2\164\1\117\2\157\1\115\2\155\6\uffff";
    static final String DFA24_maxS =
        "\1\141\2\165\1\124\2\164\1\117\2\157\1\115\2\155\6\uffff";
    static final String DFA24_acceptS =
        "\14\uffff\1\1\1\4\1\3\1\5\1\2\1\6";
    static final String DFA24_specialS =
        "\22\uffff}>";
    static final String[] DFA24_transitionS = {
            "\1\1\37\uffff\1\2",
            "\1\3\37\uffff\1\4",
            "\1\5",
            "\1\6",
            "\1\7",
            "\1\10",
            "\1\11",
            "\1\12",
            "\1\13",
            "\1\14",
            "\1\16",
            "\1\20",
            "",
            "",
            "",
            "",
            "",
            ""
    };

    static final short[] DFA24_eot = DFA.unpackEncodedString(DFA24_eotS);
    static final short[] DFA24_eof = DFA.unpackEncodedString(DFA24_eofS);
    static final char[] DFA24_min = DFA.unpackEncodedStringToUnsignedChars(DFA24_minS);
    static final char[] DFA24_max = DFA.unpackEncodedStringToUnsignedChars(DFA24_maxS);
    static final short[] DFA24_accept = DFA.unpackEncodedString(DFA24_acceptS);
    static final short[] DFA24_special = DFA.unpackEncodedString(DFA24_specialS);
    static final short[][] DFA24_transition;

    static {
        int numStates = DFA24_transitionS.length;
        DFA24_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA24_transition[i] = DFA.unpackEncodedString(DFA24_transitionS[i]);
        }
    }

    class DFA24 extends DFA {

        public DFA24(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 24;
            this.eot = DFA24_eot;
            this.eof = DFA24_eof;
            this.min = DFA24_min;
            this.max = DFA24_max;
            this.accept = DFA24_accept;
            this.special = DFA24_special;
            this.transition = DFA24_transition;
        }
        public String getDescription() {
            return "78:1: AUTOMATIC_KEYWORD : ( 'AUTOMATIC' | 'automatic' | 'Automatic' | 'AUTO' | 'Auto' | 'auto' );";
        }
    }
    static final String DFA41_eotS =
        "\1\uffff\22\43\2\150\10\43\5\uffff\1\171\2\uffff\1\173\7\uffff"+
        "\73\43\1\uffff\20\43\1\uffff\1\u00d7\2\uffff\2\u00da\2\u00dd\2\u00df"+
        "\1\u00da\1\u00dd\1\u00df\16\43\2\u00f2\4\43\1\u00f2\10\43\2\u0100"+
        "\1\43\1\u0100\1\u0105\1\u0106\1\u0105\1\u0106\2\43\1\u0105\1\u0106"+
        "\34\43\3\u0129\11\43\1\uffff\2\43\1\uffff\2\43\1\uffff\1\43\1\uffff"+
        "\4\43\1\u0140\1\43\1\u0140\1\43\1\u0140\5\43\2\u0148\2\u014a\1\uffff"+
        "\2\43\1\u0148\1\u014a\6\43\2\u0156\1\43\1\uffff\1\43\1\u0156\2\43"+
        "\2\uffff\41\43\1\u017e\1\uffff\1\43\1\u017e\1\43\1\u017e\2\43\2"+
        "\u0184\1\43\1\u0184\3\u0189\11\43\1\uffff\3\u0195\4\43\1\uffff\1"+
        "\43\1\uffff\4\43\2\u019f\2\u01a0\1\u019f\1\u01a0\1\43\1\uffff\7"+
        "\43\2\u01a9\1\43\1\u01a9\10\43\1\u01b3\2\43\1\u01b3\2\43\1\u01b3"+
        "\2\43\2\u01ba\1\43\1\u01ba\7\43\1\uffff\5\43\1\uffff\4\43\1\uffff"+
        "\13\43\1\uffff\11\43\2\uffff\6\43\2\u0105\1\uffff\1\u0105\2\u01e6"+
        "\2\u01e7\1\u01e6\1\u01e7\2\43\1\uffff\6\43\1\uffff\4\43\3\u01f5"+
        "\14\43\1\u00dd\1\u00da\1\u00dd\1\u00da\2\43\1\u00dd\1\u00da\1\43"+
        "\1\u0148\1\43\1\u0148\3\43\1\u0148\4\43\2\u0100\1\43\1\u0100\2\uffff"+
        "\11\43\3\150\1\43\1\uffff\2\43\3\u0129\2\u021f\2\43\1\u021f\1\43"+
        "\3\u0189\14\43\1\u022f\1\43\1\u022f\1\43\1\u022f\1\43\3\u01ba\3"+
        "\43\3\u017e\1\uffff\3\43\3\u00df\2\43\2\u014a\1\43\1\u014a\3\u0156"+
        "\1\uffff\3\u023c\3\43\3\u0184\3\u00f2\1\uffff\3\u01f5";
    static final String DFA41_eofS =
        "\u0240\uffff";
    static final String DFA41_minS =
        "\1\11\1\103\1\143\1\105\1\145\1\110\1\150\1\111\1\151\1\111\1\151"+
        "\1\101\1\141\1\111\1\151\1\116\1\156\1\103\1\143\2\60\1\117\1\157"+
        "\1\101\1\141\1\114\1\154\1\116\1\156\5\uffff\1\60\2\uffff\1\56\7"+
        "\uffff\1\103\1\143\1\114\1\154\1\115\1\155\1\143\1\154\1\155\1\101"+
        "\2\141\1\115\1\155\1\114\1\154\1\101\1\141\1\114\1\154\1\155\1\154"+
        "\1\141\1\154\1\130\1\170\1\117\1\157\1\170\1\157\1\105\1\145\1\103"+
        "\1\143\1\145\1\143\1\116\1\156\1\123\1\163\1\156\1\163\1\107\1\147"+
        "\1\101\1\141\1\147\1\141\1\101\2\141\1\104\1\144\1\110\1\150\1\144"+
        "\1\150\1\101\1\141\1\uffff\1\141\1\116\2\156\1\122\2\162\1\111\1"+
        "\151\1\124\1\164\1\151\1\164\1\111\2\151\1\uffff\1\56\2\uffff\11"+
        "\60\1\114\1\105\1\154\1\145\1\154\1\145\1\120\1\124\1\160\1\164"+
        "\1\130\1\170\1\122\1\162\2\60\1\160\1\164\1\170\1\162\1\60\1\105"+
        "\1\145\1\101\1\141\1\145\1\141\1\103\1\143\2\60\1\143\5\60\1\105"+
        "\1\145\2\60\1\145\1\116\1\156\1\124\1\164\1\156\1\164\1\111\1\114"+
        "\1\117\1\151\1\154\1\157\1\151\1\154\1\157\1\105\1\145\1\101\1\141"+
        "\1\145\1\141\1\120\2\160\1\126\2\166\3\60\1\107\1\147\1\117\1\157"+
        "\1\147\1\157\1\124\2\164\1\uffff\1\101\1\115\1\uffff\1\141\1\155"+
        "\1\uffff\1\116\1\uffff\1\156\1\141\1\155\1\156\1\60\1\122\1\60\1"+
        "\162\1\60\1\162\1\114\1\122\1\154\1\162\4\60\1\uffff\1\154\1\162"+
        "\2\60\1\104\1\144\1\124\1\164\1\144\1\164\2\60\1\125\1\uffff\1\165"+
        "\1\60\1\165\1\122\2\uffff\1\162\1\104\1\144\1\162\1\144\1\105\1"+
        "\145\1\111\1\151\1\145\1\151\1\107\1\111\1\116\1\147\1\151\1\156"+
        "\1\147\1\151\1\156\1\103\1\143\1\122\1\162\1\143\1\162\1\110\2\150"+
        "\1\101\2\141\1\111\1\60\1\uffff\1\151\1\60\1\151\1\60\1\116\1\156"+
        "\2\60\1\156\4\60\1\122\1\101\1\162\1\141\1\123\1\163\1\162\1\141"+
        "\1\163\1\uffff\3\60\1\105\1\117\1\145\1\157\1\uffff\1\103\1\uffff"+
        "\1\143\1\145\1\157\1\143\6\60\1\123\1\uffff\1\163\1\122\1\162\1"+
        "\163\1\162\1\131\1\171\2\60\1\171\1\60\1\104\1\144\1\103\1\143\1"+
        "\144\1\143\1\116\1\107\1\60\1\156\1\147\1\60\1\156\1\147\1\60\1"+
        "\110\1\150\2\60\1\150\1\60\1\111\2\151\1\122\2\162\1\116\1\uffff"+
        "\2\156\1\105\1\145\1\101\1\uffff\1\141\1\145\1\141\1\101\1\uffff"+
        "\2\141\1\105\1\114\1\145\1\154\1\111\1\151\1\145\1\154\1\151\1\uffff"+
        "\1\130\1\114\1\170\1\154\1\124\1\164\1\170\1\154\1\164\2\uffff\1"+
        "\111\1\151\1\105\1\145\1\151\1\145\2\60\1\uffff\7\60\1\105\1\116"+
        "\1\uffff\1\145\1\156\1\145\1\156\1\101\1\141\1\uffff\1\141\1\103"+
        "\2\143\3\60\1\107\2\147\1\104\1\144\1\124\1\164\1\144\1\164\1\114"+
        "\2\154\4\60\1\117\1\157\2\60\1\157\1\60\1\114\1\60\1\154\1\105\1"+
        "\145\1\60\1\154\1\145\1\117\1\157\2\60\1\157\1\60\2\uffff\1\104"+
        "\1\105\1\144\1\145\1\144\1\145\1\122\2\162\3\60\1\111\1\uffff\2"+
        "\151\5\60\1\111\1\151\1\60\1\151\3\60\1\116\2\156\1\105\1\145\1"+
        "\122\1\162\1\145\1\162\1\116\2\156\1\60\1\104\1\60\1\144\1\60\1"+
        "\144\3\60\1\116\2\156\3\60\1\uffff\1\103\2\143\3\60\1\104\1\144"+
        "\2\60\1\144\4\60\1\uffff\3\60\1\107\2\147\6\60\1\uffff\3\60";
    static final String DFA41_maxS =
        "\1\172\2\151\2\145\2\164\2\154\2\162\2\151\2\164\2\156\2\151\2"+
        "\172\2\157\2\141\2\165\2\156\5\uffff\1\71\2\uffff\1\145\7\uffff"+
        "\1\103\1\143\1\114\1\154\1\115\1\155\1\143\1\154\1\155\1\106\2\146"+
        "\1\116\1\156\1\114\1\154\1\101\1\141\1\114\1\154\1\156\1\154\1\141"+
        "\1\154\1\130\1\170\1\117\1\157\1\170\1\157\1\105\1\145\1\103\1\143"+
        "\1\145\1\143\1\124\1\164\1\123\1\163\1\164\1\163\1\107\1\147\1\101"+
        "\1\141\1\147\1\141\1\123\2\163\1\104\1\144\1\110\1\150\1\144\1\150"+
        "\1\101\1\141\1\uffff\1\141\1\116\2\156\1\122\2\162\1\111\1\151\1"+
        "\124\1\164\1\151\1\164\1\111\2\151\1\uffff\1\145\2\uffff\11\172"+
        "\1\114\1\105\1\154\1\145\1\154\1\145\1\120\1\124\1\160\1\164\1\130"+
        "\1\170\1\122\1\162\2\172\1\160\1\164\1\170\1\162\1\172\1\105\1\145"+
        "\1\101\1\141\1\145\1\141\1\103\1\143\2\172\1\143\5\172\1\105\1\145"+
        "\2\172\1\145\1\116\1\156\1\124\1\164\1\156\1\164\1\111\1\114\1\117"+
        "\1\151\1\154\1\157\1\151\1\154\1\157\1\105\1\145\1\101\1\141\1\145"+
        "\1\141\1\120\2\160\1\126\2\166\3\172\1\107\1\147\1\117\1\157\1\147"+
        "\1\157\1\124\2\164\1\uffff\1\101\1\115\1\uffff\1\141\1\155\1\uffff"+
        "\1\116\1\uffff\1\156\1\141\1\155\1\156\1\172\1\122\1\172\1\162\1"+
        "\172\1\162\1\114\1\122\1\154\1\162\4\172\1\uffff\1\154\1\162\2\172"+
        "\1\104\1\144\1\124\1\164\1\144\1\164\2\172\1\125\1\uffff\1\165\1"+
        "\172\1\165\1\122\2\uffff\1\162\1\104\1\144\1\162\1\144\1\105\1\145"+
        "\1\111\1\151\1\145\1\151\1\107\1\111\1\116\1\147\1\151\1\156\1\147"+
        "\1\151\1\156\1\103\1\143\1\122\1\162\1\143\1\162\1\110\2\150\1\101"+
        "\2\141\1\111\1\172\1\uffff\1\151\1\172\1\151\1\172\1\116\1\156\2"+
        "\172\1\156\4\172\1\122\1\101\1\162\1\141\1\123\1\163\1\162\1\141"+
        "\1\163\1\uffff\3\172\1\105\1\117\1\145\1\157\1\uffff\1\103\1\uffff"+
        "\1\143\1\145\1\157\1\143\6\172\1\123\1\uffff\1\163\1\122\1\162\1"+
        "\163\1\162\1\131\1\171\2\172\1\171\1\172\1\104\1\144\1\103\1\143"+
        "\1\144\1\143\1\116\1\107\1\172\1\156\1\147\1\172\1\156\1\147\1\172"+
        "\1\110\1\150\2\172\1\150\1\172\1\111\2\151\1\122\2\162\1\116\1\uffff"+
        "\2\156\1\105\1\145\1\101\1\uffff\1\141\1\145\1\141\1\101\1\uffff"+
        "\2\141\1\105\1\114\1\145\1\154\1\111\1\151\1\145\1\154\1\151\1\uffff"+
        "\1\130\1\114\1\170\1\154\1\124\1\164\1\170\1\154\1\164\2\uffff\1"+
        "\111\1\151\1\105\1\145\1\151\1\145\2\172\1\uffff\7\172\1\105\1\116"+
        "\1\uffff\1\145\1\156\1\145\1\156\1\101\1\141\1\uffff\1\141\1\103"+
        "\2\143\3\172\1\107\2\147\1\104\1\144\1\124\1\164\1\144\1\164\1\114"+
        "\2\154\4\172\1\117\1\157\2\172\1\157\1\172\1\114\1\172\1\154\1\105"+
        "\1\145\1\172\1\154\1\145\1\117\1\157\2\172\1\157\1\172\2\uffff\1"+
        "\104\1\105\1\144\1\145\1\144\1\145\1\122\2\162\3\172\1\111\1\uffff"+
        "\2\151\5\172\1\111\1\151\1\172\1\151\3\172\1\116\2\156\1\105\1\145"+
        "\1\122\1\162\1\145\1\162\1\116\2\156\1\172\1\104\1\172\1\144\1\172"+
        "\1\144\3\172\1\116\2\156\3\172\1\uffff\1\103\2\143\3\172\1\104\1"+
        "\144\2\172\1\144\4\172\1\uffff\3\172\1\107\2\147\6\172\1\uffff\3"+
        "\172";
    static final String DFA41_acceptS =
        "\35\uffff\1\35\1\36\1\37\1\40\1\41\1\uffff\1\43\1\44\1\uffff\1"+
        "\50\1\51\1\52\1\53\1\54\1\55\1\56\73\uffff\1\16\20\uffff\1\42\1"+
        "\uffff\1\46\1\47\132\uffff\1\45\2\uffff\1\7\2\uffff\1\1\1\uffff"+
        "\1\24\22\uffff\1\33\15\uffff\1\23\4\uffff\1\10\1\11\42\uffff\1\21"+
        "\26\uffff\1\2\7\uffff\1\3\1\uffff\1\14\13\uffff\1\6\47\uffff\1\22"+
        "\5\uffff\1\30\4\uffff\1\27\13\uffff\1\17\11\uffff\1\4\1\5\10\uffff"+
        "\1\32\11\uffff\1\34\6\uffff\1\15\53\uffff\1\12\1\31\15\uffff\1\20"+
        "\51\uffff\1\25\17\uffff\1\13\14\uffff\1\26\3\uffff";
    static final String DFA41_specialS =
        "\u0240\uffff}>";
    static final String[] DFA41_transitionS = {
            "\1\52\1\53\2\uffff\1\53\22\uffff\1\52\1\uffff\1\44\4\uffff"+
            "\1\44\1\35\1\36\1\46\1\42\1\37\1\42\1\uffff\1\54\12\45\1\41"+
            "\1\40\1\50\1\47\1\51\2\uffff\1\31\1\13\1\5\1\1\1\43\1\7\1\23"+
            "\1\43\1\33\4\43\1\25\1\43\1\11\1\43\1\3\1\15\1\43\1\17\1\27"+
            "\1\21\3\43\6\uffff\1\32\1\14\1\6\1\2\1\43\1\10\1\24\1\43\1\34"+
            "\4\43\1\26\1\43\1\12\1\43\1\4\1\16\1\43\1\20\1\30\1\22\3\43",
            "\1\57\1\uffff\1\55\3\uffff\1\61\31\uffff\1\60\1\uffff\1\56"+
            "\3\uffff\1\62",
            "\1\64\1\uffff\1\63\3\uffff\1\65",
            "\1\66\37\uffff\1\67",
            "\1\70",
            "\1\75\6\uffff\1\71\1\73\3\uffff\1\77\23\uffff\1\76\6\uffff"+
            "\1\72\1\74\3\uffff\1\100",
            "\1\103\6\uffff\1\101\1\102\3\uffff\1\104",
            "\1\105\2\uffff\1\107\34\uffff\1\106\2\uffff\1\110",
            "\1\111\2\uffff\1\112",
            "\1\115\10\uffff\1\113\26\uffff\1\116\10\uffff\1\114",
            "\1\120\10\uffff\1\117",
            "\1\123\7\uffff\1\121\27\uffff\1\124\7\uffff\1\122",
            "\1\126\7\uffff\1\125",
            "\1\127\12\uffff\1\131\24\uffff\1\130\12\uffff\1\132",
            "\1\133\12\uffff\1\134",
            "\1\135\37\uffff\1\136",
            "\1\137",
            "\1\142\5\uffff\1\140\31\uffff\1\143\5\uffff\1\141",
            "\1\145\5\uffff\1\144",
            "\12\43\7\uffff\21\43\1\146\10\43\4\uffff\1\43\1\uffff\21\43"+
            "\1\147\10\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\21\43\1\151\10\43",
            "\1\152\37\uffff\1\153",
            "\1\154",
            "\1\155\37\uffff\1\156",
            "\1\157",
            "\1\160\10\uffff\1\162\26\uffff\1\161\10\uffff\1\163",
            "\1\164\10\uffff\1\165",
            "\1\166\37\uffff\1\167",
            "\1\170",
            "",
            "",
            "",
            "",
            "",
            "\12\172",
            "",
            "",
            "\1\174\1\uffff\12\45\13\uffff\1\174\37\uffff\1\174",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\175",
            "\1\176",
            "\1\177",
            "\1\u0080",
            "\1\u0081",
            "\1\u0082",
            "\1\u0083",
            "\1\u0084",
            "\1\u0085",
            "\1\u0086\4\uffff\1\u0087",
            "\1\u0088\4\uffff\1\u0089",
            "\1\u008a\4\uffff\1\u008b",
            "\1\u008c\1\u008d",
            "\1\u008e\1\u008f",
            "\1\u0090",
            "\1\u0091",
            "\1\u0092",
            "\1\u0093",
            "\1\u0094",
            "\1\u0095",
            "\1\u0096\1\u0097",
            "\1\u0098",
            "\1\u0099",
            "\1\u009a",
            "\1\u009b",
            "\1\u009c",
            "\1\u009d",
            "\1\u009e",
            "\1\u009f",
            "\1\u00a0",
            "\1\u00a1",
            "\1\u00a2",
            "\1\u00a3",
            "\1\u00a4",
            "\1\u00a5",
            "\1\u00a6",
            "\1\u00a7\5\uffff\1\u00a8",
            "\1\u00a9\5\uffff\1\u00aa",
            "\1\u00ab",
            "\1\u00ac",
            "\1\u00ad\5\uffff\1\u00ae",
            "\1\u00af",
            "\1\u00b0",
            "\1\u00b1",
            "\1\u00b2",
            "\1\u00b3",
            "\1\u00b4",
            "\1\u00b5",
            "\1\u00b7\7\uffff\1\u00b8\11\uffff\1\u00b6",
            "\1\u00ba\7\uffff\1\u00bb\11\uffff\1\u00b9",
            "\1\u00bd\7\uffff\1\u00be\11\uffff\1\u00bc",
            "\1\u00bf",
            "\1\u00c0",
            "\1\u00c1",
            "\1\u00c2",
            "\1\u00c3",
            "\1\u00c4",
            "\1\u00c5",
            "\1\u00c6",
            "",
            "\1\u00c7",
            "\1\u00c8",
            "\1\u00c9",
            "\1\u00ca",
            "\1\u00cb",
            "\1\u00cc",
            "\1\u00cd",
            "\1\u00ce",
            "\1\u00cf",
            "\1\u00d0",
            "\1\u00d1",
            "\1\u00d2",
            "\1\u00d3",
            "\1\u00d4",
            "\1\u00d5",
            "\1\u00d6",
            "",
            "\1\174\1\uffff\12\172\13\uffff\1\174\37\uffff\1\174",
            "",
            "",
            "\12\43\7\uffff\10\43\1\u00d9\2\43\1\u00d8\16\43\4\uffff\1"+
            "\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u00dc\2"+
            "\43\1\u00db\16\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\4\43\1\u00de\25\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\4\43\1\u00e0\25"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u00e2\2"+
            "\43\1\u00e1\16\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\4\43\1\u00e3\25"+
            "\43",
            "\1\u00e4",
            "\1\u00e5",
            "\1\u00e6",
            "\1\u00e7",
            "\1\u00e8",
            "\1\u00e9",
            "\1\u00ea",
            "\1\u00eb",
            "\1\u00ec",
            "\1\u00ed",
            "\1\u00ee",
            "\1\u00ef",
            "\1\u00f0",
            "\1\u00f1",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u00f3",
            "\1\u00f4",
            "\1\u00f5",
            "\1\u00f6",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u00f7",
            "\1\u00f8",
            "\1\u00f9",
            "\1\u00fa",
            "\1\u00fb",
            "\1\u00fc",
            "\1\u00fd",
            "\1\u00fe",
            "\12\43\7\uffff\23\43\1\u00ff\6\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\23\43\1\u0101\6"+
            "\43",
            "\1\u0102",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\23\43\1\u0103\6"+
            "\43",
            "\12\43\7\uffff\1\u0104\31\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\1\u0107\31\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0108",
            "\1\u0109",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\1\u010a\31\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u010b",
            "\1\u010c",
            "\1\u010d",
            "\1\u010e",
            "\1\u010f",
            "\1\u0110",
            "\1\u0111",
            "\1\u0112",
            "\1\u0113",
            "\1\u0114",
            "\1\u0115",
            "\1\u0116",
            "\1\u0117",
            "\1\u0118",
            "\1\u0119",
            "\1\u011a",
            "\1\u011b",
            "\1\u011c",
            "\1\u011d",
            "\1\u011e",
            "\1\u011f",
            "\1\u0120",
            "\1\u0121",
            "\1\u0122",
            "\1\u0123",
            "\1\u0124",
            "\1\u0125",
            "\1\u0126",
            "\12\43\7\uffff\30\43\1\u0127\1\u0128\4\uffff\1\43\1\uffff"+
            "\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\30\43\1\u012a\1"+
            "\u012b",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\30\43\1\u012c\1"+
            "\u012d",
            "\1\u012e",
            "\1\u012f",
            "\1\u0130",
            "\1\u0131",
            "\1\u0132",
            "\1\u0133",
            "\1\u0134",
            "\1\u0135",
            "\1\u0136",
            "",
            "\1\u0137",
            "\1\u0138",
            "",
            "\1\u0139",
            "\1\u013a",
            "",
            "\1\u013b",
            "",
            "\1\u013c",
            "\1\u013d",
            "\1\u013e",
            "\1\u013f",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0141",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0142",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0143",
            "\1\u0144",
            "\1\u0145",
            "\1\u0146",
            "\1\u0147",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\1\u0149\31\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\1\u014b\31\43",
            "",
            "\1\u014c",
            "\1\u014d",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\1\u014e\31\43",
            "\1\u014f",
            "\1\u0150",
            "\1\u0151",
            "\1\u0152",
            "\1\u0153",
            "\1\u0154",
            "\12\43\7\uffff\10\43\1\u0155\21\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u0157\21"+
            "\43",
            "\1\u0158",
            "",
            "\1\u0159",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u015a\21"+
            "\43",
            "\1\u015b",
            "\1\u015c",
            "",
            "",
            "\1\u015d",
            "\1\u015e",
            "\1\u015f",
            "\1\u0160",
            "\1\u0161",
            "\1\u0162",
            "\1\u0163",
            "\1\u0164",
            "\1\u0165",
            "\1\u0166",
            "\1\u0167",
            "\1\u0168",
            "\1\u0169",
            "\1\u016a",
            "\1\u016b",
            "\1\u016c",
            "\1\u016d",
            "\1\u016e",
            "\1\u016f",
            "\1\u0170",
            "\1\u0171",
            "\1\u0172",
            "\1\u0173",
            "\1\u0174",
            "\1\u0175",
            "\1\u0176",
            "\1\u0177",
            "\1\u0178",
            "\1\u0179",
            "\1\u017a",
            "\1\u017b",
            "\1\u017c",
            "\1\u017d",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "\1\u017f",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0180",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0181",
            "\1\u0182",
            "\12\43\7\uffff\14\43\1\u0183\15\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\14\43\1\u0185\15"+
            "\43",
            "\1\u0186",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\14\43\1\u0187\15"+
            "\43",
            "\12\43\7\uffff\10\43\1\u0188\21\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u018a\21"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\u018b\21"+
            "\43",
            "\1\u018c",
            "\1\u018d",
            "\1\u018e",
            "\1\u018f",
            "\1\u0190",
            "\1\u0191",
            "\1\u0192",
            "\1\u0193",
            "\1\u0194",
            "",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0196",
            "\1\u0197",
            "\1\u0198",
            "\1\u0199",
            "",
            "\1\u019a",
            "",
            "\1\u019b",
            "\1\u019c",
            "\1\u019d",
            "\1\u019e",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01a1",
            "",
            "\1\u01a2",
            "\1\u01a3",
            "\1\u01a4",
            "\1\u01a5",
            "\1\u01a6",
            "\1\u01a7",
            "\1\u01a8",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01aa",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01ab",
            "\1\u01ac",
            "\1\u01ad",
            "\1\u01ae",
            "\1\u01af",
            "\1\u01b0",
            "\1\u01b1",
            "\1\u01b2",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01b4",
            "\1\u01b5",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01b6",
            "\1\u01b7",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01b8",
            "\1\u01b9",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01bb",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01bc",
            "\1\u01bd",
            "\1\u01be",
            "\1\u01bf",
            "\1\u01c0",
            "\1\u01c1",
            "\1\u01c2",
            "",
            "\1\u01c3",
            "\1\u01c4",
            "\1\u01c5",
            "\1\u01c6",
            "\1\u01c7",
            "",
            "\1\u01c8",
            "\1\u01c9",
            "\1\u01ca",
            "\1\u01cb",
            "",
            "\1\u01cc",
            "\1\u01cd",
            "\1\u01ce",
            "\1\u01cf",
            "\1\u01d0",
            "\1\u01d1",
            "\1\u01d2",
            "\1\u01d3",
            "\1\u01d4",
            "\1\u01d5",
            "\1\u01d6",
            "",
            "\1\u01d7",
            "\1\u01d8",
            "\1\u01d9",
            "\1\u01da",
            "\1\u01db",
            "\1\u01dc",
            "\1\u01dd",
            "\1\u01de",
            "\1\u01df",
            "",
            "",
            "\1\u01e0",
            "\1\u01e1",
            "\1\u01e2",
            "\1\u01e3",
            "\1\u01e4",
            "\1\u01e5",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u01e8",
            "\1\u01e9",
            "",
            "\1\u01ea",
            "\1\u01eb",
            "\1\u01ec",
            "\1\u01ed",
            "\1\u01ee",
            "\1\u01ef",
            "",
            "\1\u01f0",
            "\1\u01f1",
            "\1\u01f2",
            "\1\u01f3",
            "\12\43\7\uffff\30\43\1\u01f4\1\43\4\uffff\1\43\1\uffff\32"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\30\43\1\u01f6\1"+
            "\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\30\43\1\u01f7\1"+
            "\43",
            "\1\u01f8",
            "\1\u01f9",
            "\1\u01fa",
            "\1\u01fb",
            "\1\u01fc",
            "\1\u01fd",
            "\1\u01fe",
            "\1\u01ff",
            "\1\u0200",
            "\1\u0201",
            "\1\u0202",
            "\1\u0203",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0204",
            "\1\u0205",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0206",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0207",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0208",
            "\1\u0209",
            "\1\u020a",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u020b",
            "\1\u020c",
            "\1\u020d",
            "\1\u020e",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u020f",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "",
            "\1\u0210",
            "\1\u0211",
            "\1\u0212",
            "\1\u0213",
            "\1\u0214",
            "\1\u0215",
            "\1\u0216",
            "\1\u0217",
            "\1\u0218",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0219",
            "",
            "\1\u021a",
            "\1\u021b",
            "\12\43\7\uffff\31\43\1\u021c\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\31\43\1\u021d",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\31\43\1\u021e",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0220",
            "\1\u0221",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0222",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0223",
            "\1\u0224",
            "\1\u0225",
            "\1\u0226",
            "\1\u0227",
            "\1\u0228",
            "\1\u0229",
            "\1\u022a",
            "\1\u022b",
            "\1\u022c",
            "\1\u022d",
            "\1\u022e",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0230",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0231",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0232",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0233",
            "\1\u0234",
            "\1\u0235",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "\1\u0236",
            "\1\u0237",
            "\1\u0238",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u0239",
            "\1\u023a",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u023b",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\1\u023d",
            "\1\u023e",
            "\1\u023f",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\12\43\7\uffff\32\43\4\uffff\1\43\1\uffff\32\43"
    };

    static final short[] DFA41_eot = DFA.unpackEncodedString(DFA41_eotS);
    static final short[] DFA41_eof = DFA.unpackEncodedString(DFA41_eofS);
    static final char[] DFA41_min = DFA.unpackEncodedStringToUnsignedChars(DFA41_minS);
    static final char[] DFA41_max = DFA.unpackEncodedStringToUnsignedChars(DFA41_maxS);
    static final short[] DFA41_accept = DFA.unpackEncodedString(DFA41_acceptS);
    static final short[] DFA41_special = DFA.unpackEncodedString(DFA41_specialS);
    static final short[][] DFA41_transition;

    static {
        int numStates = DFA41_transitionS.length;
        DFA41_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA41_transition[i] = DFA.unpackEncodedString(DFA41_transitionS[i]);
        }
    }

    class DFA41 extends DFA {

        public DFA41(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 41;
            this.eot = DFA41_eot;
            this.eof = DFA41_eof;
            this.min = DFA41_min;
            this.max = DFA41_max;
            this.accept = DFA41_accept;
            this.special = DFA41_special;
            this.transition = DFA41_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( DECLARE_KEYWORD | REAL_KEYWORD | COMPLEX_KEYWORD | FIXED_KEYWORD | FLOAT_KEYWORD | PRECISION_KEYWORD | DECIMAL_KEYWORD | BINARY_KEYWORD | BIT_KEYWORD | SIGNED_KEYWORD | UNSIGNED_KEYWORD | CHARACTER_KEYWORD | WIDECHAR_KEYWORD | GRAPHIC_KEYWORD | REFER_KEYWORD | NONVARYING_KEYWORD | VARYING_KEYWORD | VARYINGZ_KEYWORD | PICTURE_KEYWORD | DIMENSION_KEYWORD | ALIGNED_KEYWORD | UNALIGNED_KEYWORD | INITIAL_KEYWORD | AUTOMATIC_KEYWORD | STATIC_KEYWORD | BASED_KEYWORD | CONTROLLED_KEYWORD | UNION_KEYWORD | LEFT_PAREN | RIGHT_PAREN | COMMA | SEMICOLON | COLUMN | SIGN | DATA_ITEM_NAME | STRING_LITERAL | SIGNED_INTEGER | UNSIGNED_INTEGER | FLOAT | ASTERISK | EQUALS | LT | GT | WHITESPACE | NEWLINE | MULTI_COMMENT );";
        }
    }
 

}
