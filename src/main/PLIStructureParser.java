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
// $ANTLR 3.1.3 Mar 17, 2009 19:23:44 D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g 2009-10-09 08:51:48

package com.legstar.pli2cob;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import org.antlr.runtime.tree.*;

public class PLIStructureParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "DATA_ITEM", "LEVEL", "NAME", "ARITHMETIC", "REAL", "COMPLEX", "FIXED", "FLOAT", "DECIMAL", "BINARY", "PRECISION", "SCALING_FACTOR", "SIGNED", "UNSIGNED", "STRING", "BIT", "CHARACTER", "GRAPHIC", "WIDECHAR", "LENGTH", "REFER", "STORAGE", "NONVARYING", "VARYING", "VARYINGZ", "PICTURE", "FILLER", "DIMENSIONS", "DIMENSION", "HBOUND", "LBOUND", "INITIAL", "AUTOMATIC", "STATIC", "BASED", "CONTROLLED", "ALIGNMENT", "ALIGNED", "UNALIGNED", "UNION", "REDEFINES", "DECLARE_KEYWORD", "SEMICOLON", "COMMA", "UNSIGNED_INTEGER", "DATA_ITEM_NAME", "ASTERISK", "REAL_KEYWORD", "COMPLEX_KEYWORD", "FIXED_KEYWORD", "FLOAT_KEYWORD", "PRECISION_KEYWORD", "DECIMAL_KEYWORD", "BINARY_KEYWORD", "BIT_KEYWORD", "SIGNED_KEYWORD", "UNSIGNED_KEYWORD", "CHARACTER_KEYWORD", "WIDECHAR_KEYWORD", "GRAPHIC_KEYWORD", "REFER_KEYWORD", "NONVARYING_KEYWORD", "VARYING_KEYWORD", "VARYINGZ_KEYWORD", "PICTURE_KEYWORD", "DIMENSION_KEYWORD", "ALIGNED_KEYWORD", "UNALIGNED_KEYWORD", "INITIAL_KEYWORD", "AUTOMATIC_KEYWORD", "STATIC_KEYWORD", "BASED_KEYWORD", "CONTROLLED_KEYWORD", "UNION_KEYWORD", "LEFT_PAREN", "RIGHT_PAREN", "STRING_LITERAL", "SIGNED_INTEGER", "COLUMN"
    };
    public static final int DATA_ITEM_NAME=49;
    public static final int DATA_ITEM=4;
    public static final int UNALIGNED=42;
    public static final int DIMENSION_KEYWORD=69;
    public static final int HBOUND=33;
    public static final int SIGNED=16;
    public static final int DECIMAL_KEYWORD=56;
    public static final int CONTROLLED_KEYWORD=76;
    public static final int REFER=24;
    public static final int WIDECHAR_KEYWORD=62;
    public static final int REDEFINES=44;
    public static final int PRECISION=14;
    public static final int VARYING=27;
    public static final int FIXED=10;
    public static final int VARYINGZ_KEYWORD=67;
    public static final int SIGNED_KEYWORD=59;
    public static final int AUTOMATIC_KEYWORD=73;
    public static final int VARYING_KEYWORD=66;
    public static final int GRAPHIC_KEYWORD=63;
    public static final int REAL_KEYWORD=51;
    public static final int COMPLEX=9;
    public static final int CHARACTER_KEYWORD=61;
    public static final int FLOAT=11;
    public static final int UNALIGNED_KEYWORD=71;
    public static final int BASED=38;
    public static final int EOF=-1;
    public static final int BINARY_KEYWORD=57;
    public static final int FLOAT_KEYWORD=54;
    public static final int CHARACTER=20;
    public static final int ASTERISK=50;
    public static final int LENGTH=23;
    public static final int UNION_KEYWORD=77;
    public static final int WIDECHAR=22;
    public static final int STORAGE=25;
    public static final int DIMENSIONS=31;
    public static final int RIGHT_PAREN=79;
    public static final int UNSIGNED_INTEGER=48;
    public static final int NAME=6;
    public static final int STRING_LITERAL=80;
    public static final int SCALING_FACTOR=15;
    public static final int VARYINGZ=28;
    public static final int ALIGNMENT=40;
    public static final int COMMA=47;
    public static final int NONVARYING=26;
    public static final int SIGNED_INTEGER=81;
    public static final int ARITHMETIC=7;
    public static final int COLUMN=82;
    public static final int ALIGNED=41;
    public static final int DIMENSION=32;
    public static final int ALIGNED_KEYWORD=70;
    public static final int INITIAL_KEYWORD=72;
    public static final int FILLER=30;
    public static final int CONTROLLED=39;
    public static final int FIXED_KEYWORD=53;
    public static final int PICTURE_KEYWORD=68;
    public static final int GRAPHIC=21;
    public static final int STATIC=37;
    public static final int INITIAL=35;
    public static final int REFER_KEYWORD=64;
    public static final int SEMICOLON=46;
    public static final int NONVARYING_KEYWORD=65;
    public static final int UNSIGNED_KEYWORD=60;
    public static final int LBOUND=34;
    public static final int STATIC_KEYWORD=74;
    public static final int UNION=43;
    public static final int PICTURE=29;
    public static final int REAL=8;
    public static final int UNSIGNED=17;
    public static final int DECIMAL=12;
    public static final int LEVEL=5;
    public static final int COMPLEX_KEYWORD=52;
    public static final int LEFT_PAREN=78;
    public static final int BASED_KEYWORD=75;
    public static final int BIT=19;
    public static final int DECLARE_KEYWORD=45;
    public static final int BINARY=13;
    public static final int PRECISION_KEYWORD=55;
    public static final int BIT_KEYWORD=58;
    public static final int STRING=18;
    public static final int AUTOMATIC=36;

    // delegates
    // delegators


        public PLIStructureParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public PLIStructureParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return PLIStructureParser.tokenNames; }
    public String getGrammarFileName() { return "D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g"; }


        /** Logger. */
        private final Log _log = LogFactory.getLog(getClass());
        
        /** Used to store data item nodes while we are searching for a parent. */
        private Stack < Object > stack = new Stack < Object >();

        /** Helper holding levels for data items stored in the previous stack. */
        private Stack < Integer > levelStack = new Stack < Integer >();

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



    public static class pl1code_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "pl1code"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:132:1: pl1code : ( statement )* ;
    public final PLIStructureParser.pl1code_return pl1code() throws RecognitionException {
        PLIStructureParser.pl1code_return retval = new PLIStructureParser.pl1code_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.statement_return statement1 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:133:5: ( ( statement )* )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:133:7: ( statement )*
            {
            root_0 = (Object)adaptor.nil();

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:133:7: ( statement )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>=DATA_ITEM && LA1_0<=COLUMN)) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:133:8: statement
            	    {
            	    pushFollow(FOLLOW_statement_in_pl1code278);
            	    statement1=statement();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, statement1.getTree());

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "pl1code"

    public static class statement_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "statement"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:136:1: statement : ( ( declare )=> declare | non_declare );
    public final PLIStructureParser.statement_return statement() throws RecognitionException {
        PLIStructureParser.statement_return retval = new PLIStructureParser.statement_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.declare_return declare2 = null;

        PLIStructureParser.non_declare_return non_declare3 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:137:5: ( ( declare )=> declare | non_declare )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==DECLARE_KEYWORD) ) {
                int LA2_1 = input.LA(2);

                if ( (synpred1_PLIStructureParser()) ) {
                    alt2=1;
                }
                else if ( (true) ) {
                    alt2=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 2, 1, input);

                    throw nvae;
                }
            }
            else if ( ((LA2_0>=DATA_ITEM && LA2_0<=REDEFINES)||(LA2_0>=SEMICOLON && LA2_0<=COLUMN)) ) {
                alt2=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:137:7: ( declare )=> declare
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_declare_in_statement301);
                    declare2=declare();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, declare2.getTree());

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:138:7: non_declare
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_non_declare_in_statement309);
                    non_declare3=non_declare();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "statement"

    public static class declare_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "declare"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:145:1: declare : DECLARE_KEYWORD data_items SEMICOLON ;
    public final PLIStructureParser.declare_return declare() throws RecognitionException {
        PLIStructureParser.declare_return retval = new PLIStructureParser.declare_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token DECLARE_KEYWORD4=null;
        Token SEMICOLON6=null;
        PLIStructureParser.data_items_return data_items5 = null;


        Object DECLARE_KEYWORD4_tree=null;
        Object SEMICOLON6_tree=null;

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:146:5: ( DECLARE_KEYWORD data_items SEMICOLON )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:146:7: DECLARE_KEYWORD data_items SEMICOLON
            {
            root_0 = (Object)adaptor.nil();

            DECLARE_KEYWORD4=(Token)match(input,DECLARE_KEYWORD,FOLLOW_DECLARE_KEYWORD_in_declare329); if (state.failed) return retval;
            pushFollow(FOLLOW_data_items_in_declare332);
            data_items5=data_items();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, data_items5.getTree());
            SEMICOLON6=(Token)match(input,SEMICOLON,FOLLOW_SEMICOLON_in_declare334); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "declare"

    public static class non_declare_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "non_declare"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:153:1: non_declare options {greedy=false; } : ( . )* SEMICOLON ;
    public final PLIStructureParser.non_declare_return non_declare() throws RecognitionException {
        PLIStructureParser.non_declare_return retval = new PLIStructureParser.non_declare_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token wildcard7=null;
        Token SEMICOLON8=null;

        Object wildcard7_tree=null;
        Object SEMICOLON8_tree=null;

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:154:5: ( ( . )* SEMICOLON )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:154:8: ( . )* SEMICOLON
            {
            root_0 = (Object)adaptor.nil();

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:154:8: ( . )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==SEMICOLON) ) {
                    alt3=2;
                }
                else if ( ((LA3_0>=DATA_ITEM && LA3_0<=DECLARE_KEYWORD)||(LA3_0>=COMMA && LA3_0<=COLUMN)) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:154:8: .
            	    {
            	    wildcard7=(Token)input.LT(1);
            	    matchAny(input); if (state.failed) return retval;
            	    if ( state.backtracking==0 ) {
            	    wildcard7_tree = (Object)adaptor.create(wildcard7);
            	    adaptor.addChild(root_0, wildcard7_tree);
            	    }

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);

            SEMICOLON8=(Token)match(input,SEMICOLON,FOLLOW_SEMICOLON_in_non_declare365); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            SEMICOLON8_tree = (Object)adaptor.create(SEMICOLON8);
            adaptor.addChild(root_0, SEMICOLON8_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "non_declare"

    public static class data_items_return extends ParserRuleReturnScope {
        public int level;
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "data_items"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:160:1: data_items returns [int level] : ( ( data_item_with_children )=> data_item_with_children | data_item );
    public final PLIStructureParser.data_items_return data_items() throws RecognitionException {
        PLIStructureParser.data_items_return retval = new PLIStructureParser.data_items_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.data_item_with_children_return data_item_with_children9 = null;

        PLIStructureParser.data_item_return data_item10 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:161:5: ( ( data_item_with_children )=> data_item_with_children | data_item )
            int alt4=2;
            alt4 = dfa4.predict(input);
            switch (alt4) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:161:9: ( data_item_with_children )=> data_item_with_children
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_data_item_with_children_in_data_items393);
                    data_item_with_children9=data_item_with_children();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, data_item_with_children9.getTree());
                    if ( state.backtracking==0 ) {
                      retval.level = (data_item_with_children9!=null?data_item_with_children9.level:0);
                    }

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:162:9: data_item
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_data_item_in_data_items405);
                    data_item10=data_item();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, data_item10.getTree());
                    if ( state.backtracking==0 ) {
                      retval.level = (data_item10!=null?data_item10.level:0);
                    }

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "data_items"

    public static class data_item_with_children_return extends ParserRuleReturnScope {
        public int level;
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "data_item_with_children"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:172:1: data_item_with_children returns [int level] : data_item COMMA data_items -> data_item ;
    public final PLIStructureParser.data_item_with_children_return data_item_with_children() throws RecognitionException {
        PLIStructureParser.data_item_with_children_return retval = new PLIStructureParser.data_item_with_children_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token COMMA12=null;
        PLIStructureParser.data_item_return data_item11 = null;

        PLIStructureParser.data_items_return data_items13 = null;


        Object COMMA12_tree=null;
        RewriteRuleTokenStream stream_COMMA=new RewriteRuleTokenStream(adaptor,"token COMMA");
        RewriteRuleSubtreeStream stream_data_item=new RewriteRuleSubtreeStream(adaptor,"rule data_item");
        RewriteRuleSubtreeStream stream_data_items=new RewriteRuleSubtreeStream(adaptor,"rule data_items");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:180:5: ( data_item COMMA data_items -> data_item )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:180:7: data_item COMMA data_items
            {
            pushFollow(FOLLOW_data_item_in_data_item_with_children435);
            data_item11=data_item();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_data_item.add(data_item11.getTree());
            COMMA12=(Token)match(input,COMMA,FOLLOW_COMMA_in_data_item_with_children437); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_COMMA.add(COMMA12);

            pushFollow(FOLLOW_data_items_in_data_item_with_children439);
            data_items13=data_items();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_data_items.add(data_items13.getTree());
            if ( state.backtracking==0 ) {

                      retval.level = (data_item11!=null?data_item11.level:0);
                      /* push children to stack (delayed addChild) */
                      stack.push((data_items13!=null?((Object)data_items13.tree):null));
                      levelStack.push((data_items13!=null?data_items13.level:0));
                  
            }


            // AST REWRITE
            // elements: data_item
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 187:7: -> data_item
            {
                adaptor.addChild(root_0, stream_data_item.nextTree());

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
            if ( state.backtracking==0 ) {

                  /* add stacked children with a higher level */
                  while(!levelStack.isEmpty() && levelStack.peek() > retval.level) {
                      adaptor.addChild(((Object)retval.tree), stack.pop());
                      levelStack.pop();
                  }

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "data_item_with_children"

    public static class data_item_return extends ParserRuleReturnScope {
        public int level;
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "data_item"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:193:1: data_item returns [int level] : ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* -> ^( DATA_ITEM ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* ) ;
    public final PLIStructureParser.data_item_return data_item() throws RecognitionException {
        PLIStructureParser.data_item_return retval = new PLIStructureParser.data_item_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.level_return level14 = null;

        PLIStructureParser.data_item_name_return data_item_name15 = null;

        PLIStructureParser.implicit_dimension_attribute_return implicit_dimension_attribute16 = null;

        PLIStructureParser.elementary_data_item_attribute_return elementary_data_item_attribute17 = null;

        PLIStructureParser.misc_attribute_return misc_attribute18 = null;


        RewriteRuleSubtreeStream stream_implicit_dimension_attribute=new RewriteRuleSubtreeStream(adaptor,"rule implicit_dimension_attribute");
        RewriteRuleSubtreeStream stream_level=new RewriteRuleSubtreeStream(adaptor,"rule level");
        RewriteRuleSubtreeStream stream_data_item_name=new RewriteRuleSubtreeStream(adaptor,"rule data_item_name");
        RewriteRuleSubtreeStream stream_elementary_data_item_attribute=new RewriteRuleSubtreeStream(adaptor,"rule elementary_data_item_attribute");
        RewriteRuleSubtreeStream stream_misc_attribute=new RewriteRuleSubtreeStream(adaptor,"rule misc_attribute");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:5: ( ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* -> ^( DATA_ITEM ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:7: ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )*
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:7: ( level )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==UNSIGNED_INTEGER) ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:7: level
                    {
                    pushFollow(FOLLOW_level_in_data_item478);
                    level14=level();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_level.add(level14.getTree());

                    }
                    break;

            }

            pushFollow(FOLLOW_data_item_name_in_data_item481);
            data_item_name15=data_item_name();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_data_item_name.add(data_item_name15.getTree());
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:29: ( implicit_dimension_attribute )?
            int alt6=2;
            int LA6_0 = input.LA(1);

            if ( (LA6_0==LEFT_PAREN) ) {
                alt6=1;
            }
            switch (alt6) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:29: implicit_dimension_attribute
                    {
                    pushFollow(FOLLOW_implicit_dimension_attribute_in_data_item483);
                    implicit_dimension_attribute16=implicit_dimension_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_implicit_dimension_attribute.add(implicit_dimension_attribute16.getTree());

                    }
                    break;

            }

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:59: ( elementary_data_item_attribute )?
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( ((LA7_0>=REAL_KEYWORD && LA7_0<=GRAPHIC_KEYWORD)||(LA7_0>=NONVARYING_KEYWORD && LA7_0<=PICTURE_KEYWORD)) ) {
                alt7=1;
            }
            switch (alt7) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:59: elementary_data_item_attribute
                    {
                    pushFollow(FOLLOW_elementary_data_item_attribute_in_data_item486);
                    elementary_data_item_attribute17=elementary_data_item_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_elementary_data_item_attribute.add(elementary_data_item_attribute17.getTree());

                    }
                    break;

            }

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:91: ( misc_attribute )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( ((LA8_0>=DIMENSION_KEYWORD && LA8_0<=UNION_KEYWORD)) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:194:91: misc_attribute
            	    {
            	    pushFollow(FOLLOW_misc_attribute_in_data_item489);
            	    misc_attribute18=misc_attribute();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_misc_attribute.add(misc_attribute18.getTree());

            	    }
            	    break;

            	default :
            	    break loop8;
                }
            } while (true);

            if ( state.backtracking==0 ) {
              retval.level = (retval.level == null) ? 1 : Integer.parseInt((level14!=null?input.toString(level14.start,level14.stop):null));
            }


            // AST REWRITE
            // elements: data_item_name, level, elementary_data_item_attribute, implicit_dimension_attribute, misc_attribute
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 196:7: -> ^( DATA_ITEM ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:196:9: ^( DATA_ITEM ( level )? data_item_name ( implicit_dimension_attribute )? ( elementary_data_item_attribute )? ( misc_attribute )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(DATA_ITEM, "DATA_ITEM"), root_1);

                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:196:21: ( level )?
                if ( stream_level.hasNext() ) {
                    adaptor.addChild(root_1, stream_level.nextTree());

                }
                stream_level.reset();
                adaptor.addChild(root_1, stream_data_item_name.nextTree());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:196:43: ( implicit_dimension_attribute )?
                if ( stream_implicit_dimension_attribute.hasNext() ) {
                    adaptor.addChild(root_1, stream_implicit_dimension_attribute.nextTree());

                }
                stream_implicit_dimension_attribute.reset();
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:196:73: ( elementary_data_item_attribute )?
                if ( stream_elementary_data_item_attribute.hasNext() ) {
                    adaptor.addChild(root_1, stream_elementary_data_item_attribute.nextTree());

                }
                stream_elementary_data_item_attribute.reset();
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:196:105: ( misc_attribute )*
                while ( stream_misc_attribute.hasNext() ) {
                    adaptor.addChild(root_1, stream_misc_attribute.nextTree());

                }
                stream_misc_attribute.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "data_item"

    public static class level_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "level"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:199:1: level : UNSIGNED_INTEGER -> ^( LEVEL UNSIGNED_INTEGER ) ;
    public final PLIStructureParser.level_return level() throws RecognitionException {
        PLIStructureParser.level_return retval = new PLIStructureParser.level_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token UNSIGNED_INTEGER19=null;

        Object UNSIGNED_INTEGER19_tree=null;
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:199:6: ( UNSIGNED_INTEGER -> ^( LEVEL UNSIGNED_INTEGER ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:200:5: UNSIGNED_INTEGER
            {
            UNSIGNED_INTEGER19=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_level539); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER19);



            // AST REWRITE
            // elements: UNSIGNED_INTEGER
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 200:22: -> ^( LEVEL UNSIGNED_INTEGER )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:200:24: ^( LEVEL UNSIGNED_INTEGER )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(LEVEL, "LEVEL"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "level"

    public static class data_item_name_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "data_item_name"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:207:1: data_item_name : ( DATA_ITEM_NAME -> ^( NAME DATA_ITEM_NAME ) | ASTERISK -> ^( NAME ASTERISK ) | DECLARE_KEYWORD -> ^( NAME DECLARE_KEYWORD ) | REAL_KEYWORD -> ^( NAME REAL_KEYWORD ) | COMPLEX_KEYWORD -> ^( NAME COMPLEX_KEYWORD ) | FIXED_KEYWORD -> ^( NAME FIXED_KEYWORD ) | FLOAT_KEYWORD -> ^( NAME FLOAT_KEYWORD ) | PRECISION_KEYWORD -> ^( NAME PRECISION_KEYWORD ) | DECIMAL_KEYWORD -> ^( NAME DECIMAL_KEYWORD ) | BINARY_KEYWORD -> ^( NAME BINARY_KEYWORD ) | BIT_KEYWORD -> ^( NAME BIT_KEYWORD ) | SIGNED_KEYWORD -> ^( NAME SIGNED_KEYWORD ) | UNSIGNED_KEYWORD -> ^( NAME UNSIGNED_KEYWORD ) | CHARACTER_KEYWORD -> ^( NAME CHARACTER_KEYWORD ) | WIDECHAR_KEYWORD -> ^( NAME WIDECHAR_KEYWORD ) | GRAPHIC_KEYWORD -> ^( NAME GRAPHIC_KEYWORD ) | REFER_KEYWORD -> ^( NAME REFER_KEYWORD ) | NONVARYING_KEYWORD -> ^( NAME NONVARYING_KEYWORD ) | VARYING_KEYWORD -> ^( NAME VARYING_KEYWORD ) | VARYINGZ_KEYWORD -> ^( NAME VARYINGZ_KEYWORD ) | PICTURE_KEYWORD -> ^( NAME PICTURE_KEYWORD ) | DIMENSION_KEYWORD -> ^( NAME DIMENSION_KEYWORD ) | ALIGNED_KEYWORD -> ^( NAME ALIGNED_KEYWORD ) | UNALIGNED_KEYWORD -> ^( NAME UNALIGNED_KEYWORD ) | INITIAL_KEYWORD -> ^( NAME INITIAL_KEYWORD ) | AUTOMATIC_KEYWORD -> ^( NAME AUTOMATIC_KEYWORD ) | STATIC_KEYWORD -> ^( NAME STATIC_KEYWORD ) | BASED_KEYWORD -> ^( NAME BASED_KEYWORD ) | CONTROLLED_KEYWORD -> ^( NAME CONTROLLED_KEYWORD ) | UNION_KEYWORD -> ^( NAME UNION_KEYWORD ) );
    public final PLIStructureParser.data_item_name_return data_item_name() throws RecognitionException {
        PLIStructureParser.data_item_name_return retval = new PLIStructureParser.data_item_name_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token DATA_ITEM_NAME20=null;
        Token ASTERISK21=null;
        Token DECLARE_KEYWORD22=null;
        Token REAL_KEYWORD23=null;
        Token COMPLEX_KEYWORD24=null;
        Token FIXED_KEYWORD25=null;
        Token FLOAT_KEYWORD26=null;
        Token PRECISION_KEYWORD27=null;
        Token DECIMAL_KEYWORD28=null;
        Token BINARY_KEYWORD29=null;
        Token BIT_KEYWORD30=null;
        Token SIGNED_KEYWORD31=null;
        Token UNSIGNED_KEYWORD32=null;
        Token CHARACTER_KEYWORD33=null;
        Token WIDECHAR_KEYWORD34=null;
        Token GRAPHIC_KEYWORD35=null;
        Token REFER_KEYWORD36=null;
        Token NONVARYING_KEYWORD37=null;
        Token VARYING_KEYWORD38=null;
        Token VARYINGZ_KEYWORD39=null;
        Token PICTURE_KEYWORD40=null;
        Token DIMENSION_KEYWORD41=null;
        Token ALIGNED_KEYWORD42=null;
        Token UNALIGNED_KEYWORD43=null;
        Token INITIAL_KEYWORD44=null;
        Token AUTOMATIC_KEYWORD45=null;
        Token STATIC_KEYWORD46=null;
        Token BASED_KEYWORD47=null;
        Token CONTROLLED_KEYWORD48=null;
        Token UNION_KEYWORD49=null;

        Object DATA_ITEM_NAME20_tree=null;
        Object ASTERISK21_tree=null;
        Object DECLARE_KEYWORD22_tree=null;
        Object REAL_KEYWORD23_tree=null;
        Object COMPLEX_KEYWORD24_tree=null;
        Object FIXED_KEYWORD25_tree=null;
        Object FLOAT_KEYWORD26_tree=null;
        Object PRECISION_KEYWORD27_tree=null;
        Object DECIMAL_KEYWORD28_tree=null;
        Object BINARY_KEYWORD29_tree=null;
        Object BIT_KEYWORD30_tree=null;
        Object SIGNED_KEYWORD31_tree=null;
        Object UNSIGNED_KEYWORD32_tree=null;
        Object CHARACTER_KEYWORD33_tree=null;
        Object WIDECHAR_KEYWORD34_tree=null;
        Object GRAPHIC_KEYWORD35_tree=null;
        Object REFER_KEYWORD36_tree=null;
        Object NONVARYING_KEYWORD37_tree=null;
        Object VARYING_KEYWORD38_tree=null;
        Object VARYINGZ_KEYWORD39_tree=null;
        Object PICTURE_KEYWORD40_tree=null;
        Object DIMENSION_KEYWORD41_tree=null;
        Object ALIGNED_KEYWORD42_tree=null;
        Object UNALIGNED_KEYWORD43_tree=null;
        Object INITIAL_KEYWORD44_tree=null;
        Object AUTOMATIC_KEYWORD45_tree=null;
        Object STATIC_KEYWORD46_tree=null;
        Object BASED_KEYWORD47_tree=null;
        Object CONTROLLED_KEYWORD48_tree=null;
        Object UNION_KEYWORD49_tree=null;
        RewriteRuleTokenStream stream_DATA_ITEM_NAME=new RewriteRuleTokenStream(adaptor,"token DATA_ITEM_NAME");
        RewriteRuleTokenStream stream_DIMENSION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token DIMENSION_KEYWORD");
        RewriteRuleTokenStream stream_DECIMAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token DECIMAL_KEYWORD");
        RewriteRuleTokenStream stream_CONTROLLED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token CONTROLLED_KEYWORD");
        RewriteRuleTokenStream stream_WIDECHAR_KEYWORD=new RewriteRuleTokenStream(adaptor,"token WIDECHAR_KEYWORD");
        RewriteRuleTokenStream stream_SIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token SIGNED_KEYWORD");
        RewriteRuleTokenStream stream_VARYINGZ_KEYWORD=new RewriteRuleTokenStream(adaptor,"token VARYINGZ_KEYWORD");
        RewriteRuleTokenStream stream_AUTOMATIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token AUTOMATIC_KEYWORD");
        RewriteRuleTokenStream stream_REAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token REAL_KEYWORD");
        RewriteRuleTokenStream stream_GRAPHIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token GRAPHIC_KEYWORD");
        RewriteRuleTokenStream stream_VARYING_KEYWORD=new RewriteRuleTokenStream(adaptor,"token VARYING_KEYWORD");
        RewriteRuleTokenStream stream_REFER_KEYWORD=new RewriteRuleTokenStream(adaptor,"token REFER_KEYWORD");
        RewriteRuleTokenStream stream_CHARACTER_KEYWORD=new RewriteRuleTokenStream(adaptor,"token CHARACTER_KEYWORD");
        RewriteRuleTokenStream stream_UNALIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNALIGNED_KEYWORD");
        RewriteRuleTokenStream stream_NONVARYING_KEYWORD=new RewriteRuleTokenStream(adaptor,"token NONVARYING_KEYWORD");
        RewriteRuleTokenStream stream_UNSIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_KEYWORD");
        RewriteRuleTokenStream stream_STATIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token STATIC_KEYWORD");
        RewriteRuleTokenStream stream_BINARY_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BINARY_KEYWORD");
        RewriteRuleTokenStream stream_FLOAT_KEYWORD=new RewriteRuleTokenStream(adaptor,"token FLOAT_KEYWORD");
        RewriteRuleTokenStream stream_ASTERISK=new RewriteRuleTokenStream(adaptor,"token ASTERISK");
        RewriteRuleTokenStream stream_UNION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNION_KEYWORD");
        RewriteRuleTokenStream stream_COMPLEX_KEYWORD=new RewriteRuleTokenStream(adaptor,"token COMPLEX_KEYWORD");
        RewriteRuleTokenStream stream_BASED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BASED_KEYWORD");
        RewriteRuleTokenStream stream_DECLARE_KEYWORD=new RewriteRuleTokenStream(adaptor,"token DECLARE_KEYWORD");
        RewriteRuleTokenStream stream_ALIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token ALIGNED_KEYWORD");
        RewriteRuleTokenStream stream_INITIAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token INITIAL_KEYWORD");
        RewriteRuleTokenStream stream_PRECISION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token PRECISION_KEYWORD");
        RewriteRuleTokenStream stream_BIT_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BIT_KEYWORD");
        RewriteRuleTokenStream stream_FIXED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token FIXED_KEYWORD");
        RewriteRuleTokenStream stream_PICTURE_KEYWORD=new RewriteRuleTokenStream(adaptor,"token PICTURE_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:208:5: ( DATA_ITEM_NAME -> ^( NAME DATA_ITEM_NAME ) | ASTERISK -> ^( NAME ASTERISK ) | DECLARE_KEYWORD -> ^( NAME DECLARE_KEYWORD ) | REAL_KEYWORD -> ^( NAME REAL_KEYWORD ) | COMPLEX_KEYWORD -> ^( NAME COMPLEX_KEYWORD ) | FIXED_KEYWORD -> ^( NAME FIXED_KEYWORD ) | FLOAT_KEYWORD -> ^( NAME FLOAT_KEYWORD ) | PRECISION_KEYWORD -> ^( NAME PRECISION_KEYWORD ) | DECIMAL_KEYWORD -> ^( NAME DECIMAL_KEYWORD ) | BINARY_KEYWORD -> ^( NAME BINARY_KEYWORD ) | BIT_KEYWORD -> ^( NAME BIT_KEYWORD ) | SIGNED_KEYWORD -> ^( NAME SIGNED_KEYWORD ) | UNSIGNED_KEYWORD -> ^( NAME UNSIGNED_KEYWORD ) | CHARACTER_KEYWORD -> ^( NAME CHARACTER_KEYWORD ) | WIDECHAR_KEYWORD -> ^( NAME WIDECHAR_KEYWORD ) | GRAPHIC_KEYWORD -> ^( NAME GRAPHIC_KEYWORD ) | REFER_KEYWORD -> ^( NAME REFER_KEYWORD ) | NONVARYING_KEYWORD -> ^( NAME NONVARYING_KEYWORD ) | VARYING_KEYWORD -> ^( NAME VARYING_KEYWORD ) | VARYINGZ_KEYWORD -> ^( NAME VARYINGZ_KEYWORD ) | PICTURE_KEYWORD -> ^( NAME PICTURE_KEYWORD ) | DIMENSION_KEYWORD -> ^( NAME DIMENSION_KEYWORD ) | ALIGNED_KEYWORD -> ^( NAME ALIGNED_KEYWORD ) | UNALIGNED_KEYWORD -> ^( NAME UNALIGNED_KEYWORD ) | INITIAL_KEYWORD -> ^( NAME INITIAL_KEYWORD ) | AUTOMATIC_KEYWORD -> ^( NAME AUTOMATIC_KEYWORD ) | STATIC_KEYWORD -> ^( NAME STATIC_KEYWORD ) | BASED_KEYWORD -> ^( NAME BASED_KEYWORD ) | CONTROLLED_KEYWORD -> ^( NAME CONTROLLED_KEYWORD ) | UNION_KEYWORD -> ^( NAME UNION_KEYWORD ) )
            int alt9=30;
            switch ( input.LA(1) ) {
            case DATA_ITEM_NAME:
                {
                alt9=1;
                }
                break;
            case ASTERISK:
                {
                alt9=2;
                }
                break;
            case DECLARE_KEYWORD:
                {
                alt9=3;
                }
                break;
            case REAL_KEYWORD:
                {
                alt9=4;
                }
                break;
            case COMPLEX_KEYWORD:
                {
                alt9=5;
                }
                break;
            case FIXED_KEYWORD:
                {
                alt9=6;
                }
                break;
            case FLOAT_KEYWORD:
                {
                alt9=7;
                }
                break;
            case PRECISION_KEYWORD:
                {
                alt9=8;
                }
                break;
            case DECIMAL_KEYWORD:
                {
                alt9=9;
                }
                break;
            case BINARY_KEYWORD:
                {
                alt9=10;
                }
                break;
            case BIT_KEYWORD:
                {
                alt9=11;
                }
                break;
            case SIGNED_KEYWORD:
                {
                alt9=12;
                }
                break;
            case UNSIGNED_KEYWORD:
                {
                alt9=13;
                }
                break;
            case CHARACTER_KEYWORD:
                {
                alt9=14;
                }
                break;
            case WIDECHAR_KEYWORD:
                {
                alt9=15;
                }
                break;
            case GRAPHIC_KEYWORD:
                {
                alt9=16;
                }
                break;
            case REFER_KEYWORD:
                {
                alt9=17;
                }
                break;
            case NONVARYING_KEYWORD:
                {
                alt9=18;
                }
                break;
            case VARYING_KEYWORD:
                {
                alt9=19;
                }
                break;
            case VARYINGZ_KEYWORD:
                {
                alt9=20;
                }
                break;
            case PICTURE_KEYWORD:
                {
                alt9=21;
                }
                break;
            case DIMENSION_KEYWORD:
                {
                alt9=22;
                }
                break;
            case ALIGNED_KEYWORD:
                {
                alt9=23;
                }
                break;
            case UNALIGNED_KEYWORD:
                {
                alt9=24;
                }
                break;
            case INITIAL_KEYWORD:
                {
                alt9=25;
                }
                break;
            case AUTOMATIC_KEYWORD:
                {
                alt9=26;
                }
                break;
            case STATIC_KEYWORD:
                {
                alt9=27;
                }
                break;
            case BASED_KEYWORD:
                {
                alt9=28;
                }
                break;
            case CONTROLLED_KEYWORD:
                {
                alt9=29;
                }
                break;
            case UNION_KEYWORD:
                {
                alt9=30;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 9, 0, input);

                throw nvae;
            }

            switch (alt9) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:208:7: DATA_ITEM_NAME
                    {
                    DATA_ITEM_NAME20=(Token)match(input,DATA_ITEM_NAME,FOLLOW_DATA_ITEM_NAME_in_data_item_name565); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_DATA_ITEM_NAME.add(DATA_ITEM_NAME20);



                    // AST REWRITE
                    // elements: DATA_ITEM_NAME
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 208:22: -> ^( NAME DATA_ITEM_NAME )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:208:24: ^( NAME DATA_ITEM_NAME )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_DATA_ITEM_NAME.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:209:7: ASTERISK
                    {
                    ASTERISK21=(Token)match(input,ASTERISK,FOLLOW_ASTERISK_in_data_item_name580); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ASTERISK.add(ASTERISK21);



                    // AST REWRITE
                    // elements: ASTERISK
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 209:16: -> ^( NAME ASTERISK )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:209:18: ^( NAME ASTERISK )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_ASTERISK.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:210:7: DECLARE_KEYWORD
                    {
                    DECLARE_KEYWORD22=(Token)match(input,DECLARE_KEYWORD,FOLLOW_DECLARE_KEYWORD_in_data_item_name595); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_DECLARE_KEYWORD.add(DECLARE_KEYWORD22);



                    // AST REWRITE
                    // elements: DECLARE_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 210:23: -> ^( NAME DECLARE_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:210:25: ^( NAME DECLARE_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_DECLARE_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:211:7: REAL_KEYWORD
                    {
                    REAL_KEYWORD23=(Token)match(input,REAL_KEYWORD,FOLLOW_REAL_KEYWORD_in_data_item_name610); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_REAL_KEYWORD.add(REAL_KEYWORD23);



                    // AST REWRITE
                    // elements: REAL_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 211:20: -> ^( NAME REAL_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:211:22: ^( NAME REAL_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_REAL_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:212:7: COMPLEX_KEYWORD
                    {
                    COMPLEX_KEYWORD24=(Token)match(input,COMPLEX_KEYWORD,FOLLOW_COMPLEX_KEYWORD_in_data_item_name625); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_COMPLEX_KEYWORD.add(COMPLEX_KEYWORD24);



                    // AST REWRITE
                    // elements: COMPLEX_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 212:23: -> ^( NAME COMPLEX_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:212:25: ^( NAME COMPLEX_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_COMPLEX_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:213:7: FIXED_KEYWORD
                    {
                    FIXED_KEYWORD25=(Token)match(input,FIXED_KEYWORD,FOLLOW_FIXED_KEYWORD_in_data_item_name640); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_FIXED_KEYWORD.add(FIXED_KEYWORD25);



                    // AST REWRITE
                    // elements: FIXED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 213:21: -> ^( NAME FIXED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:213:23: ^( NAME FIXED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_FIXED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 7 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:214:7: FLOAT_KEYWORD
                    {
                    FLOAT_KEYWORD26=(Token)match(input,FLOAT_KEYWORD,FOLLOW_FLOAT_KEYWORD_in_data_item_name655); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_FLOAT_KEYWORD.add(FLOAT_KEYWORD26);



                    // AST REWRITE
                    // elements: FLOAT_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 214:21: -> ^( NAME FLOAT_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:214:23: ^( NAME FLOAT_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_FLOAT_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 8 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:215:7: PRECISION_KEYWORD
                    {
                    PRECISION_KEYWORD27=(Token)match(input,PRECISION_KEYWORD,FOLLOW_PRECISION_KEYWORD_in_data_item_name670); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_PRECISION_KEYWORD.add(PRECISION_KEYWORD27);



                    // AST REWRITE
                    // elements: PRECISION_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 215:25: -> ^( NAME PRECISION_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:215:27: ^( NAME PRECISION_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_PRECISION_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 9 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:216:7: DECIMAL_KEYWORD
                    {
                    DECIMAL_KEYWORD28=(Token)match(input,DECIMAL_KEYWORD,FOLLOW_DECIMAL_KEYWORD_in_data_item_name685); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_DECIMAL_KEYWORD.add(DECIMAL_KEYWORD28);



                    // AST REWRITE
                    // elements: DECIMAL_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 216:23: -> ^( NAME DECIMAL_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:216:25: ^( NAME DECIMAL_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_DECIMAL_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 10 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:217:7: BINARY_KEYWORD
                    {
                    BINARY_KEYWORD29=(Token)match(input,BINARY_KEYWORD,FOLLOW_BINARY_KEYWORD_in_data_item_name700); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BINARY_KEYWORD.add(BINARY_KEYWORD29);



                    // AST REWRITE
                    // elements: BINARY_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 217:22: -> ^( NAME BINARY_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:217:24: ^( NAME BINARY_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_BINARY_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 11 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:218:7: BIT_KEYWORD
                    {
                    BIT_KEYWORD30=(Token)match(input,BIT_KEYWORD,FOLLOW_BIT_KEYWORD_in_data_item_name715); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BIT_KEYWORD.add(BIT_KEYWORD30);



                    // AST REWRITE
                    // elements: BIT_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 218:19: -> ^( NAME BIT_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:218:21: ^( NAME BIT_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_BIT_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 12 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:219:7: SIGNED_KEYWORD
                    {
                    SIGNED_KEYWORD31=(Token)match(input,SIGNED_KEYWORD,FOLLOW_SIGNED_KEYWORD_in_data_item_name731); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SIGNED_KEYWORD.add(SIGNED_KEYWORD31);



                    // AST REWRITE
                    // elements: SIGNED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 219:22: -> ^( NAME SIGNED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:219:24: ^( NAME SIGNED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_SIGNED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 13 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:220:7: UNSIGNED_KEYWORD
                    {
                    UNSIGNED_KEYWORD32=(Token)match(input,UNSIGNED_KEYWORD,FOLLOW_UNSIGNED_KEYWORD_in_data_item_name746); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNSIGNED_KEYWORD.add(UNSIGNED_KEYWORD32);



                    // AST REWRITE
                    // elements: UNSIGNED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 220:24: -> ^( NAME UNSIGNED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:220:26: ^( NAME UNSIGNED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_UNSIGNED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 14 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:221:7: CHARACTER_KEYWORD
                    {
                    CHARACTER_KEYWORD33=(Token)match(input,CHARACTER_KEYWORD,FOLLOW_CHARACTER_KEYWORD_in_data_item_name761); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_CHARACTER_KEYWORD.add(CHARACTER_KEYWORD33);



                    // AST REWRITE
                    // elements: CHARACTER_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 221:25: -> ^( NAME CHARACTER_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:221:27: ^( NAME CHARACTER_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_CHARACTER_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 15 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:222:7: WIDECHAR_KEYWORD
                    {
                    WIDECHAR_KEYWORD34=(Token)match(input,WIDECHAR_KEYWORD,FOLLOW_WIDECHAR_KEYWORD_in_data_item_name777); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_WIDECHAR_KEYWORD.add(WIDECHAR_KEYWORD34);



                    // AST REWRITE
                    // elements: WIDECHAR_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 222:24: -> ^( NAME WIDECHAR_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:222:26: ^( NAME WIDECHAR_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_WIDECHAR_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 16 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:223:7: GRAPHIC_KEYWORD
                    {
                    GRAPHIC_KEYWORD35=(Token)match(input,GRAPHIC_KEYWORD,FOLLOW_GRAPHIC_KEYWORD_in_data_item_name793); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_GRAPHIC_KEYWORD.add(GRAPHIC_KEYWORD35);



                    // AST REWRITE
                    // elements: GRAPHIC_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 223:23: -> ^( NAME GRAPHIC_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:223:25: ^( NAME GRAPHIC_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_GRAPHIC_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 17 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:224:7: REFER_KEYWORD
                    {
                    REFER_KEYWORD36=(Token)match(input,REFER_KEYWORD,FOLLOW_REFER_KEYWORD_in_data_item_name809); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_REFER_KEYWORD.add(REFER_KEYWORD36);



                    // AST REWRITE
                    // elements: REFER_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 224:21: -> ^( NAME REFER_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:224:23: ^( NAME REFER_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_REFER_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 18 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:225:7: NONVARYING_KEYWORD
                    {
                    NONVARYING_KEYWORD37=(Token)match(input,NONVARYING_KEYWORD,FOLLOW_NONVARYING_KEYWORD_in_data_item_name825); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_NONVARYING_KEYWORD.add(NONVARYING_KEYWORD37);



                    // AST REWRITE
                    // elements: NONVARYING_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 225:26: -> ^( NAME NONVARYING_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:225:28: ^( NAME NONVARYING_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_NONVARYING_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 19 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:226:7: VARYING_KEYWORD
                    {
                    VARYING_KEYWORD38=(Token)match(input,VARYING_KEYWORD,FOLLOW_VARYING_KEYWORD_in_data_item_name841); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_VARYING_KEYWORD.add(VARYING_KEYWORD38);



                    // AST REWRITE
                    // elements: VARYING_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 226:23: -> ^( NAME VARYING_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:226:25: ^( NAME VARYING_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_VARYING_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 20 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:227:7: VARYINGZ_KEYWORD
                    {
                    VARYINGZ_KEYWORD39=(Token)match(input,VARYINGZ_KEYWORD,FOLLOW_VARYINGZ_KEYWORD_in_data_item_name857); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_VARYINGZ_KEYWORD.add(VARYINGZ_KEYWORD39);



                    // AST REWRITE
                    // elements: VARYINGZ_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 227:24: -> ^( NAME VARYINGZ_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:227:26: ^( NAME VARYINGZ_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_VARYINGZ_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 21 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:228:7: PICTURE_KEYWORD
                    {
                    PICTURE_KEYWORD40=(Token)match(input,PICTURE_KEYWORD,FOLLOW_PICTURE_KEYWORD_in_data_item_name873); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_PICTURE_KEYWORD.add(PICTURE_KEYWORD40);



                    // AST REWRITE
                    // elements: PICTURE_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 228:23: -> ^( NAME PICTURE_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:228:25: ^( NAME PICTURE_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_PICTURE_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 22 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:229:7: DIMENSION_KEYWORD
                    {
                    DIMENSION_KEYWORD41=(Token)match(input,DIMENSION_KEYWORD,FOLLOW_DIMENSION_KEYWORD_in_data_item_name889); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_DIMENSION_KEYWORD.add(DIMENSION_KEYWORD41);



                    // AST REWRITE
                    // elements: DIMENSION_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 229:25: -> ^( NAME DIMENSION_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:229:27: ^( NAME DIMENSION_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_DIMENSION_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 23 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:230:7: ALIGNED_KEYWORD
                    {
                    ALIGNED_KEYWORD42=(Token)match(input,ALIGNED_KEYWORD,FOLLOW_ALIGNED_KEYWORD_in_data_item_name904); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ALIGNED_KEYWORD.add(ALIGNED_KEYWORD42);



                    // AST REWRITE
                    // elements: ALIGNED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 230:23: -> ^( NAME ALIGNED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:230:25: ^( NAME ALIGNED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_ALIGNED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 24 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:231:7: UNALIGNED_KEYWORD
                    {
                    UNALIGNED_KEYWORD43=(Token)match(input,UNALIGNED_KEYWORD,FOLLOW_UNALIGNED_KEYWORD_in_data_item_name919); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNALIGNED_KEYWORD.add(UNALIGNED_KEYWORD43);



                    // AST REWRITE
                    // elements: UNALIGNED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 231:25: -> ^( NAME UNALIGNED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:231:27: ^( NAME UNALIGNED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_UNALIGNED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 25 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:232:7: INITIAL_KEYWORD
                    {
                    INITIAL_KEYWORD44=(Token)match(input,INITIAL_KEYWORD,FOLLOW_INITIAL_KEYWORD_in_data_item_name934); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_INITIAL_KEYWORD.add(INITIAL_KEYWORD44);



                    // AST REWRITE
                    // elements: INITIAL_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 232:23: -> ^( NAME INITIAL_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:232:25: ^( NAME INITIAL_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_INITIAL_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 26 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:233:7: AUTOMATIC_KEYWORD
                    {
                    AUTOMATIC_KEYWORD45=(Token)match(input,AUTOMATIC_KEYWORD,FOLLOW_AUTOMATIC_KEYWORD_in_data_item_name949); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_AUTOMATIC_KEYWORD.add(AUTOMATIC_KEYWORD45);



                    // AST REWRITE
                    // elements: AUTOMATIC_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 233:25: -> ^( NAME AUTOMATIC_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:233:27: ^( NAME AUTOMATIC_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_AUTOMATIC_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 27 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:234:7: STATIC_KEYWORD
                    {
                    STATIC_KEYWORD46=(Token)match(input,STATIC_KEYWORD,FOLLOW_STATIC_KEYWORD_in_data_item_name964); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_STATIC_KEYWORD.add(STATIC_KEYWORD46);



                    // AST REWRITE
                    // elements: STATIC_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 234:22: -> ^( NAME STATIC_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:234:24: ^( NAME STATIC_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_STATIC_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 28 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:235:7: BASED_KEYWORD
                    {
                    BASED_KEYWORD47=(Token)match(input,BASED_KEYWORD,FOLLOW_BASED_KEYWORD_in_data_item_name979); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BASED_KEYWORD.add(BASED_KEYWORD47);



                    // AST REWRITE
                    // elements: BASED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 235:21: -> ^( NAME BASED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:235:23: ^( NAME BASED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_BASED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 29 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:236:7: CONTROLLED_KEYWORD
                    {
                    CONTROLLED_KEYWORD48=(Token)match(input,CONTROLLED_KEYWORD,FOLLOW_CONTROLLED_KEYWORD_in_data_item_name994); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_CONTROLLED_KEYWORD.add(CONTROLLED_KEYWORD48);



                    // AST REWRITE
                    // elements: CONTROLLED_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 236:26: -> ^( NAME CONTROLLED_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:236:28: ^( NAME CONTROLLED_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_CONTROLLED_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 30 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:237:7: UNION_KEYWORD
                    {
                    UNION_KEYWORD49=(Token)match(input,UNION_KEYWORD,FOLLOW_UNION_KEYWORD_in_data_item_name1009); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNION_KEYWORD.add(UNION_KEYWORD49);



                    // AST REWRITE
                    // elements: UNION_KEYWORD
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 237:21: -> ^( NAME UNION_KEYWORD )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:237:23: ^( NAME UNION_KEYWORD )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(NAME, "NAME"), root_1);

                        adaptor.addChild(root_1, stream_UNION_KEYWORD.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "data_item_name"

    public static class elementary_data_item_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "elementary_data_item_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:240:1: elementary_data_item_attribute : ( string_attribute | picture_attribute | arithmetic_attribute );
    public final PLIStructureParser.elementary_data_item_attribute_return elementary_data_item_attribute() throws RecognitionException {
        PLIStructureParser.elementary_data_item_attribute_return retval = new PLIStructureParser.elementary_data_item_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.string_attribute_return string_attribute50 = null;

        PLIStructureParser.picture_attribute_return picture_attribute51 = null;

        PLIStructureParser.arithmetic_attribute_return arithmetic_attribute52 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:241:5: ( string_attribute | picture_attribute | arithmetic_attribute )
            int alt10=3;
            switch ( input.LA(1) ) {
            case BIT_KEYWORD:
            case CHARACTER_KEYWORD:
            case WIDECHAR_KEYWORD:
            case GRAPHIC_KEYWORD:
            case NONVARYING_KEYWORD:
            case VARYING_KEYWORD:
            case VARYINGZ_KEYWORD:
                {
                alt10=1;
                }
                break;
            case PICTURE_KEYWORD:
                {
                alt10=2;
                }
                break;
            case REAL_KEYWORD:
            case COMPLEX_KEYWORD:
            case FIXED_KEYWORD:
            case FLOAT_KEYWORD:
            case PRECISION_KEYWORD:
            case DECIMAL_KEYWORD:
            case BINARY_KEYWORD:
            case SIGNED_KEYWORD:
            case UNSIGNED_KEYWORD:
                {
                alt10=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 10, 0, input);

                throw nvae;
            }

            switch (alt10) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:241:7: string_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_string_attribute_in_elementary_data_item_attribute1033);
                    string_attribute50=string_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, string_attribute50.getTree());

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:242:7: picture_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_picture_attribute_in_elementary_data_item_attribute1041);
                    picture_attribute51=picture_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, picture_attribute51.getTree());

                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:243:7: arithmetic_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_arithmetic_attribute_in_elementary_data_item_attribute1049);
                    arithmetic_attribute52=arithmetic_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, arithmetic_attribute52.getTree());

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "elementary_data_item_attribute"

    public static class misc_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "misc_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:246:1: misc_attribute : ( dimension_attribute | alignment_attribute | initial_attribute | storage_attribute | union_attribute );
    public final PLIStructureParser.misc_attribute_return misc_attribute() throws RecognitionException {
        PLIStructureParser.misc_attribute_return retval = new PLIStructureParser.misc_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.dimension_attribute_return dimension_attribute53 = null;

        PLIStructureParser.alignment_attribute_return alignment_attribute54 = null;

        PLIStructureParser.initial_attribute_return initial_attribute55 = null;

        PLIStructureParser.storage_attribute_return storage_attribute56 = null;

        PLIStructureParser.union_attribute_return union_attribute57 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:247:5: ( dimension_attribute | alignment_attribute | initial_attribute | storage_attribute | union_attribute )
            int alt11=5;
            switch ( input.LA(1) ) {
            case DIMENSION_KEYWORD:
                {
                alt11=1;
                }
                break;
            case ALIGNED_KEYWORD:
            case UNALIGNED_KEYWORD:
                {
                alt11=2;
                }
                break;
            case INITIAL_KEYWORD:
                {
                alt11=3;
                }
                break;
            case AUTOMATIC_KEYWORD:
            case STATIC_KEYWORD:
            case BASED_KEYWORD:
            case CONTROLLED_KEYWORD:
                {
                alt11=4;
                }
                break;
            case UNION_KEYWORD:
                {
                alt11=5;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 11, 0, input);

                throw nvae;
            }

            switch (alt11) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:247:7: dimension_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_dimension_attribute_in_misc_attribute1066);
                    dimension_attribute53=dimension_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, dimension_attribute53.getTree());

                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:248:7: alignment_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_alignment_attribute_in_misc_attribute1075);
                    alignment_attribute54=alignment_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, alignment_attribute54.getTree());

                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:249:7: initial_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_initial_attribute_in_misc_attribute1083);
                    initial_attribute55=initial_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, initial_attribute55.getTree());

                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:250:7: storage_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_storage_attribute_in_misc_attribute1091);
                    storage_attribute56=storage_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, storage_attribute56.getTree());

                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:251:7: union_attribute
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_union_attribute_in_misc_attribute1099);
                    union_attribute57=union_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, union_attribute57.getTree());

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "misc_attribute"

    public static class string_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "string_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:258:1: string_attribute : ( string_keyword ( string_length_specification )? ( varying_attribute )? -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? ) | varying_attribute string_keyword ( string_length_specification )? -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? ) );
    public final PLIStructureParser.string_attribute_return string_attribute() throws RecognitionException {
        PLIStructureParser.string_attribute_return retval = new PLIStructureParser.string_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.string_keyword_return string_keyword58 = null;

        PLIStructureParser.string_length_specification_return string_length_specification59 = null;

        PLIStructureParser.varying_attribute_return varying_attribute60 = null;

        PLIStructureParser.varying_attribute_return varying_attribute61 = null;

        PLIStructureParser.string_keyword_return string_keyword62 = null;

        PLIStructureParser.string_length_specification_return string_length_specification63 = null;


        RewriteRuleSubtreeStream stream_string_length_specification=new RewriteRuleSubtreeStream(adaptor,"rule string_length_specification");
        RewriteRuleSubtreeStream stream_varying_attribute=new RewriteRuleSubtreeStream(adaptor,"rule varying_attribute");
        RewriteRuleSubtreeStream stream_string_keyword=new RewriteRuleSubtreeStream(adaptor,"rule string_keyword");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:5: ( string_keyword ( string_length_specification )? ( varying_attribute )? -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? ) | varying_attribute string_keyword ( string_length_specification )? -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? ) )
            int alt15=2;
            int LA15_0 = input.LA(1);

            if ( (LA15_0==BIT_KEYWORD||(LA15_0>=CHARACTER_KEYWORD && LA15_0<=GRAPHIC_KEYWORD)) ) {
                alt15=1;
            }
            else if ( ((LA15_0>=NONVARYING_KEYWORD && LA15_0<=VARYINGZ_KEYWORD)) ) {
                alt15=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 15, 0, input);

                throw nvae;
            }
            switch (alt15) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:7: string_keyword ( string_length_specification )? ( varying_attribute )?
                    {
                    pushFollow(FOLLOW_string_keyword_in_string_attribute1118);
                    string_keyword58=string_keyword();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_string_keyword.add(string_keyword58.getTree());
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:22: ( string_length_specification )?
                    int alt12=2;
                    int LA12_0 = input.LA(1);

                    if ( (LA12_0==LEFT_PAREN) ) {
                        alt12=1;
                    }
                    switch (alt12) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:22: string_length_specification
                            {
                            pushFollow(FOLLOW_string_length_specification_in_string_attribute1120);
                            string_length_specification59=string_length_specification();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_string_length_specification.add(string_length_specification59.getTree());

                            }
                            break;

                    }

                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:51: ( varying_attribute )?
                    int alt13=2;
                    int LA13_0 = input.LA(1);

                    if ( ((LA13_0>=NONVARYING_KEYWORD && LA13_0<=VARYINGZ_KEYWORD)) ) {
                        alt13=1;
                    }
                    switch (alt13) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:259:51: varying_attribute
                            {
                            pushFollow(FOLLOW_varying_attribute_in_string_attribute1123);
                            varying_attribute60=varying_attribute();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_varying_attribute.add(varying_attribute60.getTree());

                            }
                            break;

                    }



                    // AST REWRITE
                    // elements: varying_attribute, string_length_specification, string_keyword
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 260:7: -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:260:9: ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STRING, "STRING"), root_1);

                        adaptor.addChild(root_1, stream_string_keyword.nextTree());
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:260:33: ( string_length_specification )?
                        if ( stream_string_length_specification.hasNext() ) {
                            adaptor.addChild(root_1, stream_string_length_specification.nextTree());

                        }
                        stream_string_length_specification.reset();
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:260:62: ( varying_attribute )?
                        if ( stream_varying_attribute.hasNext() ) {
                            adaptor.addChild(root_1, stream_varying_attribute.nextTree());

                        }
                        stream_varying_attribute.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:261:7: varying_attribute string_keyword ( string_length_specification )?
                    {
                    pushFollow(FOLLOW_varying_attribute_in_string_attribute1151);
                    varying_attribute61=varying_attribute();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_varying_attribute.add(varying_attribute61.getTree());
                    pushFollow(FOLLOW_string_keyword_in_string_attribute1153);
                    string_keyword62=string_keyword();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_string_keyword.add(string_keyword62.getTree());
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:261:40: ( string_length_specification )?
                    int alt14=2;
                    int LA14_0 = input.LA(1);

                    if ( (LA14_0==LEFT_PAREN) ) {
                        alt14=1;
                    }
                    switch (alt14) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:261:40: string_length_specification
                            {
                            pushFollow(FOLLOW_string_length_specification_in_string_attribute1155);
                            string_length_specification63=string_length_specification();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_string_length_specification.add(string_length_specification63.getTree());

                            }
                            break;

                    }



                    // AST REWRITE
                    // elements: string_keyword, varying_attribute, string_length_specification
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 262:7: -> ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:262:9: ^( STRING string_keyword ( string_length_specification )? ( varying_attribute )? )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STRING, "STRING"), root_1);

                        adaptor.addChild(root_1, stream_string_keyword.nextTree());
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:262:33: ( string_length_specification )?
                        if ( stream_string_length_specification.hasNext() ) {
                            adaptor.addChild(root_1, stream_string_length_specification.nextTree());

                        }
                        stream_string_length_specification.reset();
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:262:62: ( varying_attribute )?
                        if ( stream_varying_attribute.hasNext() ) {
                            adaptor.addChild(root_1, stream_varying_attribute.nextTree());

                        }
                        stream_varying_attribute.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "string_attribute"

    public static class string_keyword_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "string_keyword"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:265:1: string_keyword : ( BIT_KEYWORD -> ^( BIT ) | CHARACTER_KEYWORD -> ^( CHARACTER ) | GRAPHIC_KEYWORD -> ^( GRAPHIC ) | WIDECHAR_KEYWORD -> ^( WIDECHAR ) );
    public final PLIStructureParser.string_keyword_return string_keyword() throws RecognitionException {
        PLIStructureParser.string_keyword_return retval = new PLIStructureParser.string_keyword_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token BIT_KEYWORD64=null;
        Token CHARACTER_KEYWORD65=null;
        Token GRAPHIC_KEYWORD66=null;
        Token WIDECHAR_KEYWORD67=null;

        Object BIT_KEYWORD64_tree=null;
        Object CHARACTER_KEYWORD65_tree=null;
        Object GRAPHIC_KEYWORD66_tree=null;
        Object WIDECHAR_KEYWORD67_tree=null;
        RewriteRuleTokenStream stream_GRAPHIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token GRAPHIC_KEYWORD");
        RewriteRuleTokenStream stream_CHARACTER_KEYWORD=new RewriteRuleTokenStream(adaptor,"token CHARACTER_KEYWORD");
        RewriteRuleTokenStream stream_WIDECHAR_KEYWORD=new RewriteRuleTokenStream(adaptor,"token WIDECHAR_KEYWORD");
        RewriteRuleTokenStream stream_BIT_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BIT_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:265:15: ( BIT_KEYWORD -> ^( BIT ) | CHARACTER_KEYWORD -> ^( CHARACTER ) | GRAPHIC_KEYWORD -> ^( GRAPHIC ) | WIDECHAR_KEYWORD -> ^( WIDECHAR ) )
            int alt16=4;
            switch ( input.LA(1) ) {
            case BIT_KEYWORD:
                {
                alt16=1;
                }
                break;
            case CHARACTER_KEYWORD:
                {
                alt16=2;
                }
                break;
            case GRAPHIC_KEYWORD:
                {
                alt16=3;
                }
                break;
            case WIDECHAR_KEYWORD:
                {
                alt16=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 16, 0, input);

                throw nvae;
            }

            switch (alt16) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:266:5: BIT_KEYWORD
                    {
                    BIT_KEYWORD64=(Token)match(input,BIT_KEYWORD,FOLLOW_BIT_KEYWORD_in_string_keyword1192); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BIT_KEYWORD.add(BIT_KEYWORD64);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 266:17: -> ^( BIT )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:266:19: ^( BIT )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(BIT, "BIT"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:267:7: CHARACTER_KEYWORD
                    {
                    CHARACTER_KEYWORD65=(Token)match(input,CHARACTER_KEYWORD,FOLLOW_CHARACTER_KEYWORD_in_string_keyword1205); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_CHARACTER_KEYWORD.add(CHARACTER_KEYWORD65);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 267:25: -> ^( CHARACTER )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:267:27: ^( CHARACTER )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(CHARACTER, "CHARACTER"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:268:7: GRAPHIC_KEYWORD
                    {
                    GRAPHIC_KEYWORD66=(Token)match(input,GRAPHIC_KEYWORD,FOLLOW_GRAPHIC_KEYWORD_in_string_keyword1218); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_GRAPHIC_KEYWORD.add(GRAPHIC_KEYWORD66);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 268:23: -> ^( GRAPHIC )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:268:25: ^( GRAPHIC )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(GRAPHIC, "GRAPHIC"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:269:7: WIDECHAR_KEYWORD
                    {
                    WIDECHAR_KEYWORD67=(Token)match(input,WIDECHAR_KEYWORD,FOLLOW_WIDECHAR_KEYWORD_in_string_keyword1231); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_WIDECHAR_KEYWORD.add(WIDECHAR_KEYWORD67);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 269:24: -> ^( WIDECHAR )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:269:26: ^( WIDECHAR )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(WIDECHAR, "WIDECHAR"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "string_keyword"

    public static class string_length_specification_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "string_length_specification"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:272:1: string_length_specification : LEFT_PAREN UNSIGNED_INTEGER ( refer_specification )? RIGHT_PAREN -> ^( LENGTH UNSIGNED_INTEGER ( refer_specification )? ) ;
    public final PLIStructureParser.string_length_specification_return string_length_specification() throws RecognitionException {
        PLIStructureParser.string_length_specification_return retval = new PLIStructureParser.string_length_specification_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token LEFT_PAREN68=null;
        Token UNSIGNED_INTEGER69=null;
        Token RIGHT_PAREN71=null;
        PLIStructureParser.refer_specification_return refer_specification70 = null;


        Object LEFT_PAREN68_tree=null;
        Object UNSIGNED_INTEGER69_tree=null;
        Object RIGHT_PAREN71_tree=null;
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleSubtreeStream stream_refer_specification=new RewriteRuleSubtreeStream(adaptor,"rule refer_specification");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:272:28: ( LEFT_PAREN UNSIGNED_INTEGER ( refer_specification )? RIGHT_PAREN -> ^( LENGTH UNSIGNED_INTEGER ( refer_specification )? ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:273:5: LEFT_PAREN UNSIGNED_INTEGER ( refer_specification )? RIGHT_PAREN
            {
            LEFT_PAREN68=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_string_length_specification1253); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN68);

            UNSIGNED_INTEGER69=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_string_length_specification1255); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER69);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:273:33: ( refer_specification )?
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( (LA17_0==REFER_KEYWORD) ) {
                alt17=1;
            }
            switch (alt17) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:273:33: refer_specification
                    {
                    pushFollow(FOLLOW_refer_specification_in_string_length_specification1257);
                    refer_specification70=refer_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_refer_specification.add(refer_specification70.getTree());

                    }
                    break;

            }

            RIGHT_PAREN71=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_string_length_specification1260); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN71);



            // AST REWRITE
            // elements: UNSIGNED_INTEGER, refer_specification
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 274:5: -> ^( LENGTH UNSIGNED_INTEGER ( refer_specification )? )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:274:7: ^( LENGTH UNSIGNED_INTEGER ( refer_specification )? )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(LENGTH, "LENGTH"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:274:33: ( refer_specification )?
                if ( stream_refer_specification.hasNext() ) {
                    adaptor.addChild(root_1, stream_refer_specification.nextTree());

                }
                stream_refer_specification.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "string_length_specification"

    public static class refer_specification_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "refer_specification"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:277:1: refer_specification : REFER_KEYWORD LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN -> ^( REFER DATA_ITEM_NAME ) ;
    public final PLIStructureParser.refer_specification_return refer_specification() throws RecognitionException {
        PLIStructureParser.refer_specification_return retval = new PLIStructureParser.refer_specification_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token REFER_KEYWORD72=null;
        Token LEFT_PAREN73=null;
        Token DATA_ITEM_NAME74=null;
        Token RIGHT_PAREN75=null;

        Object REFER_KEYWORD72_tree=null;
        Object LEFT_PAREN73_tree=null;
        Object DATA_ITEM_NAME74_tree=null;
        Object RIGHT_PAREN75_tree=null;
        RewriteRuleTokenStream stream_DATA_ITEM_NAME=new RewriteRuleTokenStream(adaptor,"token DATA_ITEM_NAME");
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_REFER_KEYWORD=new RewriteRuleTokenStream(adaptor,"token REFER_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:277:20: ( REFER_KEYWORD LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN -> ^( REFER DATA_ITEM_NAME ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:278:5: REFER_KEYWORD LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN
            {
            REFER_KEYWORD72=(Token)match(input,REFER_KEYWORD,FOLLOW_REFER_KEYWORD_in_refer_specification1290); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_REFER_KEYWORD.add(REFER_KEYWORD72);

            LEFT_PAREN73=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_refer_specification1292); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN73);

            DATA_ITEM_NAME74=(Token)match(input,DATA_ITEM_NAME,FOLLOW_DATA_ITEM_NAME_in_refer_specification1294); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_DATA_ITEM_NAME.add(DATA_ITEM_NAME74);

            RIGHT_PAREN75=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_refer_specification1296); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN75);



            // AST REWRITE
            // elements: DATA_ITEM_NAME
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 279:5: -> ^( REFER DATA_ITEM_NAME )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:279:7: ^( REFER DATA_ITEM_NAME )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(REFER, "REFER"), root_1);

                adaptor.addChild(root_1, stream_DATA_ITEM_NAME.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "refer_specification"

    public static class varying_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "varying_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:282:1: varying_attribute : ( NONVARYING_KEYWORD -> ^( VARYING NONVARYING ) | VARYING_KEYWORD -> ^( VARYING VARYING ) | VARYINGZ_KEYWORD -> ^( VARYING VARYINGZ ) );
    public final PLIStructureParser.varying_attribute_return varying_attribute() throws RecognitionException {
        PLIStructureParser.varying_attribute_return retval = new PLIStructureParser.varying_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token NONVARYING_KEYWORD76=null;
        Token VARYING_KEYWORD77=null;
        Token VARYINGZ_KEYWORD78=null;

        Object NONVARYING_KEYWORD76_tree=null;
        Object VARYING_KEYWORD77_tree=null;
        Object VARYINGZ_KEYWORD78_tree=null;
        RewriteRuleTokenStream stream_VARYING_KEYWORD=new RewriteRuleTokenStream(adaptor,"token VARYING_KEYWORD");
        RewriteRuleTokenStream stream_NONVARYING_KEYWORD=new RewriteRuleTokenStream(adaptor,"token NONVARYING_KEYWORD");
        RewriteRuleTokenStream stream_VARYINGZ_KEYWORD=new RewriteRuleTokenStream(adaptor,"token VARYINGZ_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:282:18: ( NONVARYING_KEYWORD -> ^( VARYING NONVARYING ) | VARYING_KEYWORD -> ^( VARYING VARYING ) | VARYINGZ_KEYWORD -> ^( VARYING VARYINGZ ) )
            int alt18=3;
            switch ( input.LA(1) ) {
            case NONVARYING_KEYWORD:
                {
                alt18=1;
                }
                break;
            case VARYING_KEYWORD:
                {
                alt18=2;
                }
                break;
            case VARYINGZ_KEYWORD:
                {
                alt18=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 18, 0, input);

                throw nvae;
            }

            switch (alt18) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:283:5: NONVARYING_KEYWORD
                    {
                    NONVARYING_KEYWORD76=(Token)match(input,NONVARYING_KEYWORD,FOLLOW_NONVARYING_KEYWORD_in_varying_attribute1327); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_NONVARYING_KEYWORD.add(NONVARYING_KEYWORD76);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 283:24: -> ^( VARYING NONVARYING )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:283:27: ^( VARYING NONVARYING )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(VARYING, "VARYING"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(NONVARYING, "NONVARYING"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:284:7: VARYING_KEYWORD
                    {
                    VARYING_KEYWORD77=(Token)match(input,VARYING_KEYWORD,FOLLOW_VARYING_KEYWORD_in_varying_attribute1343); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_VARYING_KEYWORD.add(VARYING_KEYWORD77);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 284:23: -> ^( VARYING VARYING )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:284:26: ^( VARYING VARYING )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(VARYING, "VARYING"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(VARYING, "VARYING"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:285:7: VARYINGZ_KEYWORD
                    {
                    VARYINGZ_KEYWORD78=(Token)match(input,VARYINGZ_KEYWORD,FOLLOW_VARYINGZ_KEYWORD_in_varying_attribute1359); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_VARYINGZ_KEYWORD.add(VARYINGZ_KEYWORD78);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 285:24: -> ^( VARYING VARYINGZ )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:285:27: ^( VARYING VARYINGZ )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(VARYING, "VARYING"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(VARYINGZ, "VARYINGZ"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "varying_attribute"

    public static class picture_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "picture_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:291:1: picture_attribute : PICTURE_KEYWORD picture_value ;
    public final PLIStructureParser.picture_attribute_return picture_attribute() throws RecognitionException {
        PLIStructureParser.picture_attribute_return retval = new PLIStructureParser.picture_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token PICTURE_KEYWORD79=null;
        PLIStructureParser.picture_value_return picture_value80 = null;


        Object PICTURE_KEYWORD79_tree=null;

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:291:18: ( PICTURE_KEYWORD picture_value )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:292:5: PICTURE_KEYWORD picture_value
            {
            root_0 = (Object)adaptor.nil();

            PICTURE_KEYWORD79=(Token)match(input,PICTURE_KEYWORD,FOLLOW_PICTURE_KEYWORD_in_picture_attribute1385); if (state.failed) return retval;
            pushFollow(FOLLOW_picture_value_in_picture_attribute1388);
            picture_value80=picture_value();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, picture_value80.getTree());

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "picture_attribute"

    public static class picture_value_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "picture_value"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:295:1: picture_value : STRING_LITERAL -> ^( PICTURE STRING_LITERAL ) ;
    public final PLIStructureParser.picture_value_return picture_value() throws RecognitionException {
        PLIStructureParser.picture_value_return retval = new PLIStructureParser.picture_value_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token STRING_LITERAL81=null;

        Object STRING_LITERAL81_tree=null;
        RewriteRuleTokenStream stream_STRING_LITERAL=new RewriteRuleTokenStream(adaptor,"token STRING_LITERAL");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:295:14: ( STRING_LITERAL -> ^( PICTURE STRING_LITERAL ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:296:5: STRING_LITERAL
            {
            STRING_LITERAL81=(Token)match(input,STRING_LITERAL,FOLLOW_STRING_LITERAL_in_picture_value1404); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_STRING_LITERAL.add(STRING_LITERAL81);



            // AST REWRITE
            // elements: STRING_LITERAL
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 296:20: -> ^( PICTURE STRING_LITERAL )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:296:23: ^( PICTURE STRING_LITERAL )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(PICTURE, "PICTURE"), root_1);

                adaptor.addChild(root_1, stream_STRING_LITERAL.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "picture_value"

    public static class arithmetic_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "arithmetic_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:304:1: arithmetic_attribute : ( arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* -> ^( ARITHMETIC arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* ) | PRECISION_KEYWORD implicit_precision_specification ( other_arithmetic_attribute )* -> ^( ARITHMETIC ^( implicit_precision_specification ) ( other_arithmetic_attribute )* ) );
    public final PLIStructureParser.arithmetic_attribute_return arithmetic_attribute() throws RecognitionException {
        PLIStructureParser.arithmetic_attribute_return retval = new PLIStructureParser.arithmetic_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token PRECISION_KEYWORD85=null;
        PLIStructureParser.arithmetic_keyword_return arithmetic_keyword82 = null;

        PLIStructureParser.precision_specification_return precision_specification83 = null;

        PLIStructureParser.other_arithmetic_attribute_return other_arithmetic_attribute84 = null;

        PLIStructureParser.implicit_precision_specification_return implicit_precision_specification86 = null;

        PLIStructureParser.other_arithmetic_attribute_return other_arithmetic_attribute87 = null;


        Object PRECISION_KEYWORD85_tree=null;
        RewriteRuleTokenStream stream_PRECISION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token PRECISION_KEYWORD");
        RewriteRuleSubtreeStream stream_implicit_precision_specification=new RewriteRuleSubtreeStream(adaptor,"rule implicit_precision_specification");
        RewriteRuleSubtreeStream stream_arithmetic_keyword=new RewriteRuleSubtreeStream(adaptor,"rule arithmetic_keyword");
        RewriteRuleSubtreeStream stream_other_arithmetic_attribute=new RewriteRuleSubtreeStream(adaptor,"rule other_arithmetic_attribute");
        RewriteRuleSubtreeStream stream_precision_specification=new RewriteRuleSubtreeStream(adaptor,"rule precision_specification");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:304:21: ( arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* -> ^( ARITHMETIC arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* ) | PRECISION_KEYWORD implicit_precision_specification ( other_arithmetic_attribute )* -> ^( ARITHMETIC ^( implicit_precision_specification ) ( other_arithmetic_attribute )* ) )
            int alt22=2;
            int LA22_0 = input.LA(1);

            if ( ((LA22_0>=REAL_KEYWORD && LA22_0<=FLOAT_KEYWORD)||(LA22_0>=DECIMAL_KEYWORD && LA22_0<=BINARY_KEYWORD)||(LA22_0>=SIGNED_KEYWORD && LA22_0<=UNSIGNED_KEYWORD)) ) {
                alt22=1;
            }
            else if ( (LA22_0==PRECISION_KEYWORD) ) {
                alt22=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 22, 0, input);

                throw nvae;
            }
            switch (alt22) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:305:5: arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )*
                    {
                    pushFollow(FOLLOW_arithmetic_keyword_in_arithmetic_attribute1430);
                    arithmetic_keyword82=arithmetic_keyword();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_arithmetic_keyword.add(arithmetic_keyword82.getTree());
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:305:24: ( precision_specification )?
                    int alt19=2;
                    int LA19_0 = input.LA(1);

                    if ( (LA19_0==PRECISION_KEYWORD||LA19_0==LEFT_PAREN) ) {
                        alt19=1;
                    }
                    switch (alt19) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:305:24: precision_specification
                            {
                            pushFollow(FOLLOW_precision_specification_in_arithmetic_attribute1432);
                            precision_specification83=precision_specification();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_precision_specification.add(precision_specification83.getTree());

                            }
                            break;

                    }

                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:305:49: ( other_arithmetic_attribute )*
                    loop20:
                    do {
                        int alt20=2;
                        int LA20_0 = input.LA(1);

                        if ( ((LA20_0>=REAL_KEYWORD && LA20_0<=FLOAT_KEYWORD)||(LA20_0>=DECIMAL_KEYWORD && LA20_0<=BINARY_KEYWORD)||(LA20_0>=SIGNED_KEYWORD && LA20_0<=UNSIGNED_KEYWORD)) ) {
                            alt20=1;
                        }


                        switch (alt20) {
                    	case 1 :
                    	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:305:49: other_arithmetic_attribute
                    	    {
                    	    pushFollow(FOLLOW_other_arithmetic_attribute_in_arithmetic_attribute1435);
                    	    other_arithmetic_attribute84=other_arithmetic_attribute();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_other_arithmetic_attribute.add(other_arithmetic_attribute84.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop20;
                        }
                    } while (true);



                    // AST REWRITE
                    // elements: other_arithmetic_attribute, arithmetic_keyword, precision_specification
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 306:5: -> ^( ARITHMETIC arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:306:7: ^( ARITHMETIC arithmetic_keyword ( precision_specification )? ( other_arithmetic_attribute )* )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ARITHMETIC, "ARITHMETIC"), root_1);

                        adaptor.addChild(root_1, stream_arithmetic_keyword.nextTree());
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:306:40: ( precision_specification )?
                        if ( stream_precision_specification.hasNext() ) {
                            adaptor.addChild(root_1, stream_precision_specification.nextTree());

                        }
                        stream_precision_specification.reset();
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:306:65: ( other_arithmetic_attribute )*
                        while ( stream_other_arithmetic_attribute.hasNext() ) {
                            adaptor.addChild(root_1, stream_other_arithmetic_attribute.nextTree());

                        }
                        stream_other_arithmetic_attribute.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:307:7: PRECISION_KEYWORD implicit_precision_specification ( other_arithmetic_attribute )*
                    {
                    PRECISION_KEYWORD85=(Token)match(input,PRECISION_KEYWORD,FOLLOW_PRECISION_KEYWORD_in_arithmetic_attribute1462); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_PRECISION_KEYWORD.add(PRECISION_KEYWORD85);

                    pushFollow(FOLLOW_implicit_precision_specification_in_arithmetic_attribute1464);
                    implicit_precision_specification86=implicit_precision_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_implicit_precision_specification.add(implicit_precision_specification86.getTree());
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:307:58: ( other_arithmetic_attribute )*
                    loop21:
                    do {
                        int alt21=2;
                        int LA21_0 = input.LA(1);

                        if ( ((LA21_0>=REAL_KEYWORD && LA21_0<=FLOAT_KEYWORD)||(LA21_0>=DECIMAL_KEYWORD && LA21_0<=BINARY_KEYWORD)||(LA21_0>=SIGNED_KEYWORD && LA21_0<=UNSIGNED_KEYWORD)) ) {
                            alt21=1;
                        }


                        switch (alt21) {
                    	case 1 :
                    	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:307:58: other_arithmetic_attribute
                    	    {
                    	    pushFollow(FOLLOW_other_arithmetic_attribute_in_arithmetic_attribute1466);
                    	    other_arithmetic_attribute87=other_arithmetic_attribute();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_other_arithmetic_attribute.add(other_arithmetic_attribute87.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop21;
                        }
                    } while (true);



                    // AST REWRITE
                    // elements: other_arithmetic_attribute, implicit_precision_specification
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 308:5: -> ^( ARITHMETIC ^( implicit_precision_specification ) ( other_arithmetic_attribute )* )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:308:7: ^( ARITHMETIC ^( implicit_precision_specification ) ( other_arithmetic_attribute )* )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ARITHMETIC, "ARITHMETIC"), root_1);

                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:308:20: ^( implicit_precision_specification )
                        {
                        Object root_2 = (Object)adaptor.nil();
                        root_2 = (Object)adaptor.becomeRoot(stream_implicit_precision_specification.nextNode(), root_2);

                        adaptor.addChild(root_1, root_2);
                        }
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:308:56: ( other_arithmetic_attribute )*
                        while ( stream_other_arithmetic_attribute.hasNext() ) {
                            adaptor.addChild(root_1, stream_other_arithmetic_attribute.nextTree());

                        }
                        stream_other_arithmetic_attribute.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "arithmetic_attribute"

    public static class arithmetic_keyword_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "arithmetic_keyword"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:311:1: arithmetic_keyword : ( FLOAT_KEYWORD -> ^( FLOAT ) | FIXED_KEYWORD -> ^( FIXED ) | BINARY_KEYWORD -> ^( BINARY ) | DECIMAL_KEYWORD -> ^( DECIMAL ) | SIGNED_KEYWORD -> ^( SIGNED ) | UNSIGNED_KEYWORD -> ^( UNSIGNED ) | REAL_KEYWORD -> ^( REAL ) | COMPLEX_KEYWORD -> ^( COMPLEX ) );
    public final PLIStructureParser.arithmetic_keyword_return arithmetic_keyword() throws RecognitionException {
        PLIStructureParser.arithmetic_keyword_return retval = new PLIStructureParser.arithmetic_keyword_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token FLOAT_KEYWORD88=null;
        Token FIXED_KEYWORD89=null;
        Token BINARY_KEYWORD90=null;
        Token DECIMAL_KEYWORD91=null;
        Token SIGNED_KEYWORD92=null;
        Token UNSIGNED_KEYWORD93=null;
        Token REAL_KEYWORD94=null;
        Token COMPLEX_KEYWORD95=null;

        Object FLOAT_KEYWORD88_tree=null;
        Object FIXED_KEYWORD89_tree=null;
        Object BINARY_KEYWORD90_tree=null;
        Object DECIMAL_KEYWORD91_tree=null;
        Object SIGNED_KEYWORD92_tree=null;
        Object UNSIGNED_KEYWORD93_tree=null;
        Object REAL_KEYWORD94_tree=null;
        Object COMPLEX_KEYWORD95_tree=null;
        RewriteRuleTokenStream stream_REAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token REAL_KEYWORD");
        RewriteRuleTokenStream stream_DECIMAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token DECIMAL_KEYWORD");
        RewriteRuleTokenStream stream_UNSIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_KEYWORD");
        RewriteRuleTokenStream stream_BINARY_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BINARY_KEYWORD");
        RewriteRuleTokenStream stream_FLOAT_KEYWORD=new RewriteRuleTokenStream(adaptor,"token FLOAT_KEYWORD");
        RewriteRuleTokenStream stream_FIXED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token FIXED_KEYWORD");
        RewriteRuleTokenStream stream_COMPLEX_KEYWORD=new RewriteRuleTokenStream(adaptor,"token COMPLEX_KEYWORD");
        RewriteRuleTokenStream stream_SIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token SIGNED_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:311:19: ( FLOAT_KEYWORD -> ^( FLOAT ) | FIXED_KEYWORD -> ^( FIXED ) | BINARY_KEYWORD -> ^( BINARY ) | DECIMAL_KEYWORD -> ^( DECIMAL ) | SIGNED_KEYWORD -> ^( SIGNED ) | UNSIGNED_KEYWORD -> ^( UNSIGNED ) | REAL_KEYWORD -> ^( REAL ) | COMPLEX_KEYWORD -> ^( COMPLEX ) )
            int alt23=8;
            switch ( input.LA(1) ) {
            case FLOAT_KEYWORD:
                {
                alt23=1;
                }
                break;
            case FIXED_KEYWORD:
                {
                alt23=2;
                }
                break;
            case BINARY_KEYWORD:
                {
                alt23=3;
                }
                break;
            case DECIMAL_KEYWORD:
                {
                alt23=4;
                }
                break;
            case SIGNED_KEYWORD:
                {
                alt23=5;
                }
                break;
            case UNSIGNED_KEYWORD:
                {
                alt23=6;
                }
                break;
            case REAL_KEYWORD:
                {
                alt23=7;
                }
                break;
            case COMPLEX_KEYWORD:
                {
                alt23=8;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 23, 0, input);

                throw nvae;
            }

            switch (alt23) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:312:5: FLOAT_KEYWORD
                    {
                    FLOAT_KEYWORD88=(Token)match(input,FLOAT_KEYWORD,FOLLOW_FLOAT_KEYWORD_in_arithmetic_keyword1500); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_FLOAT_KEYWORD.add(FLOAT_KEYWORD88);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 313:5: -> ^( FLOAT )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:313:7: ^( FLOAT )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(FLOAT, "FLOAT"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:314:7: FIXED_KEYWORD
                    {
                    FIXED_KEYWORD89=(Token)match(input,FIXED_KEYWORD,FOLLOW_FIXED_KEYWORD_in_arithmetic_keyword1517); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_FIXED_KEYWORD.add(FIXED_KEYWORD89);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 315:5: -> ^( FIXED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:315:7: ^( FIXED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(FIXED, "FIXED"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:316:7: BINARY_KEYWORD
                    {
                    BINARY_KEYWORD90=(Token)match(input,BINARY_KEYWORD,FOLLOW_BINARY_KEYWORD_in_arithmetic_keyword1534); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BINARY_KEYWORD.add(BINARY_KEYWORD90);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 317:5: -> ^( BINARY )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:317:7: ^( BINARY )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(BINARY, "BINARY"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:318:7: DECIMAL_KEYWORD
                    {
                    DECIMAL_KEYWORD91=(Token)match(input,DECIMAL_KEYWORD,FOLLOW_DECIMAL_KEYWORD_in_arithmetic_keyword1551); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_DECIMAL_KEYWORD.add(DECIMAL_KEYWORD91);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 319:5: -> ^( DECIMAL )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:319:7: ^( DECIMAL )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(DECIMAL, "DECIMAL"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 5 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:320:7: SIGNED_KEYWORD
                    {
                    SIGNED_KEYWORD92=(Token)match(input,SIGNED_KEYWORD,FOLLOW_SIGNED_KEYWORD_in_arithmetic_keyword1568); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SIGNED_KEYWORD.add(SIGNED_KEYWORD92);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 321:5: -> ^( SIGNED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:321:7: ^( SIGNED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(SIGNED, "SIGNED"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 6 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:322:7: UNSIGNED_KEYWORD
                    {
                    UNSIGNED_KEYWORD93=(Token)match(input,UNSIGNED_KEYWORD,FOLLOW_UNSIGNED_KEYWORD_in_arithmetic_keyword1585); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNSIGNED_KEYWORD.add(UNSIGNED_KEYWORD93);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 323:5: -> ^( UNSIGNED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:323:7: ^( UNSIGNED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(UNSIGNED, "UNSIGNED"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 7 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:324:7: REAL_KEYWORD
                    {
                    REAL_KEYWORD94=(Token)match(input,REAL_KEYWORD,FOLLOW_REAL_KEYWORD_in_arithmetic_keyword1602); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_REAL_KEYWORD.add(REAL_KEYWORD94);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 325:5: -> ^( REAL )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:325:7: ^( REAL )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(REAL, "REAL"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 8 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:326:7: COMPLEX_KEYWORD
                    {
                    COMPLEX_KEYWORD95=(Token)match(input,COMPLEX_KEYWORD,FOLLOW_COMPLEX_KEYWORD_in_arithmetic_keyword1619); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_COMPLEX_KEYWORD.add(COMPLEX_KEYWORD95);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 327:5: -> ^( COMPLEX )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:327:7: ^( COMPLEX )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(COMPLEX, "COMPLEX"), root_1);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "arithmetic_keyword"

    public static class other_arithmetic_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "other_arithmetic_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:330:1: other_arithmetic_attribute : arithmetic_keyword ( precision_specification )? ;
    public final PLIStructureParser.other_arithmetic_attribute_return other_arithmetic_attribute() throws RecognitionException {
        PLIStructureParser.other_arithmetic_attribute_return retval = new PLIStructureParser.other_arithmetic_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.arithmetic_keyword_return arithmetic_keyword96 = null;

        PLIStructureParser.precision_specification_return precision_specification97 = null;



        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:330:27: ( arithmetic_keyword ( precision_specification )? )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:331:5: arithmetic_keyword ( precision_specification )?
            {
            root_0 = (Object)adaptor.nil();

            pushFollow(FOLLOW_arithmetic_keyword_in_other_arithmetic_attribute1644);
            arithmetic_keyword96=arithmetic_keyword();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, arithmetic_keyword96.getTree());
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:331:24: ( precision_specification )?
            int alt24=2;
            int LA24_0 = input.LA(1);

            if ( (LA24_0==PRECISION_KEYWORD||LA24_0==LEFT_PAREN) ) {
                alt24=1;
            }
            switch (alt24) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:331:24: precision_specification
                    {
                    pushFollow(FOLLOW_precision_specification_in_other_arithmetic_attribute1646);
                    precision_specification97=precision_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, precision_specification97.getTree());

                    }
                    break;

            }


            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "other_arithmetic_attribute"

    public static class precision_specification_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "precision_specification"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:334:1: precision_specification : ( PRECISION_KEYWORD )? implicit_precision_specification -> ^( implicit_precision_specification ) ;
    public final PLIStructureParser.precision_specification_return precision_specification() throws RecognitionException {
        PLIStructureParser.precision_specification_return retval = new PLIStructureParser.precision_specification_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token PRECISION_KEYWORD98=null;
        PLIStructureParser.implicit_precision_specification_return implicit_precision_specification99 = null;


        Object PRECISION_KEYWORD98_tree=null;
        RewriteRuleTokenStream stream_PRECISION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token PRECISION_KEYWORD");
        RewriteRuleSubtreeStream stream_implicit_precision_specification=new RewriteRuleSubtreeStream(adaptor,"rule implicit_precision_specification");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:334:24: ( ( PRECISION_KEYWORD )? implicit_precision_specification -> ^( implicit_precision_specification ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:335:5: ( PRECISION_KEYWORD )? implicit_precision_specification
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:335:5: ( PRECISION_KEYWORD )?
            int alt25=2;
            int LA25_0 = input.LA(1);

            if ( (LA25_0==PRECISION_KEYWORD) ) {
                alt25=1;
            }
            switch (alt25) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:335:5: PRECISION_KEYWORD
                    {
                    PRECISION_KEYWORD98=(Token)match(input,PRECISION_KEYWORD,FOLLOW_PRECISION_KEYWORD_in_precision_specification1663); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_PRECISION_KEYWORD.add(PRECISION_KEYWORD98);


                    }
                    break;

            }

            pushFollow(FOLLOW_implicit_precision_specification_in_precision_specification1666);
            implicit_precision_specification99=implicit_precision_specification();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_implicit_precision_specification.add(implicit_precision_specification99.getTree());


            // AST REWRITE
            // elements: implicit_precision_specification
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 336:5: -> ^( implicit_precision_specification )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:336:7: ^( implicit_precision_specification )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_implicit_precision_specification.nextNode(), root_1);

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "precision_specification"

    public static class implicit_precision_specification_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "implicit_precision_specification"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:339:1: implicit_precision_specification : LEFT_PAREN UNSIGNED_INTEGER ( scaling_factor )? RIGHT_PAREN -> ^( PRECISION UNSIGNED_INTEGER ( scaling_factor )? ) ;
    public final PLIStructureParser.implicit_precision_specification_return implicit_precision_specification() throws RecognitionException {
        PLIStructureParser.implicit_precision_specification_return retval = new PLIStructureParser.implicit_precision_specification_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token LEFT_PAREN100=null;
        Token UNSIGNED_INTEGER101=null;
        Token RIGHT_PAREN103=null;
        PLIStructureParser.scaling_factor_return scaling_factor102 = null;


        Object LEFT_PAREN100_tree=null;
        Object UNSIGNED_INTEGER101_tree=null;
        Object RIGHT_PAREN103_tree=null;
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleSubtreeStream stream_scaling_factor=new RewriteRuleSubtreeStream(adaptor,"rule scaling_factor");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:339:33: ( LEFT_PAREN UNSIGNED_INTEGER ( scaling_factor )? RIGHT_PAREN -> ^( PRECISION UNSIGNED_INTEGER ( scaling_factor )? ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:340:5: LEFT_PAREN UNSIGNED_INTEGER ( scaling_factor )? RIGHT_PAREN
            {
            LEFT_PAREN100=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_implicit_precision_specification1691); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN100);

            UNSIGNED_INTEGER101=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_implicit_precision_specification1693); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER101);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:340:33: ( scaling_factor )?
            int alt26=2;
            int LA26_0 = input.LA(1);

            if ( (LA26_0==COMMA) ) {
                alt26=1;
            }
            switch (alt26) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:340:33: scaling_factor
                    {
                    pushFollow(FOLLOW_scaling_factor_in_implicit_precision_specification1695);
                    scaling_factor102=scaling_factor();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_scaling_factor.add(scaling_factor102.getTree());

                    }
                    break;

            }

            RIGHT_PAREN103=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_implicit_precision_specification1698); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN103);



            // AST REWRITE
            // elements: UNSIGNED_INTEGER, scaling_factor
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 341:5: -> ^( PRECISION UNSIGNED_INTEGER ( scaling_factor )? )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:341:7: ^( PRECISION UNSIGNED_INTEGER ( scaling_factor )? )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(PRECISION, "PRECISION"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:341:36: ( scaling_factor )?
                if ( stream_scaling_factor.hasNext() ) {
                    adaptor.addChild(root_1, stream_scaling_factor.nextTree());

                }
                stream_scaling_factor.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "implicit_precision_specification"

    public static class scaling_factor_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "scaling_factor"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:344:1: scaling_factor : COMMA (v= UNSIGNED_INTEGER | v= SIGNED_INTEGER ) -> ^( SCALING_FACTOR $v) ;
    public final PLIStructureParser.scaling_factor_return scaling_factor() throws RecognitionException {
        PLIStructureParser.scaling_factor_return retval = new PLIStructureParser.scaling_factor_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token v=null;
        Token COMMA104=null;

        Object v_tree=null;
        Object COMMA104_tree=null;
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleTokenStream stream_COMMA=new RewriteRuleTokenStream(adaptor,"token COMMA");
        RewriteRuleTokenStream stream_SIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token SIGNED_INTEGER");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:344:15: ( COMMA (v= UNSIGNED_INTEGER | v= SIGNED_INTEGER ) -> ^( SCALING_FACTOR $v) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:345:5: COMMA (v= UNSIGNED_INTEGER | v= SIGNED_INTEGER )
            {
            COMMA104=(Token)match(input,COMMA,FOLLOW_COMMA_in_scaling_factor1728); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_COMMA.add(COMMA104);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:345:11: (v= UNSIGNED_INTEGER | v= SIGNED_INTEGER )
            int alt27=2;
            int LA27_0 = input.LA(1);

            if ( (LA27_0==UNSIGNED_INTEGER) ) {
                alt27=1;
            }
            else if ( (LA27_0==SIGNED_INTEGER) ) {
                alt27=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 27, 0, input);

                throw nvae;
            }
            switch (alt27) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:345:12: v= UNSIGNED_INTEGER
                    {
                    v=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_scaling_factor1733); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(v);


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:345:33: v= SIGNED_INTEGER
                    {
                    v=(Token)match(input,SIGNED_INTEGER,FOLLOW_SIGNED_INTEGER_in_scaling_factor1739); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SIGNED_INTEGER.add(v);


                    }
                    break;

            }



            // AST REWRITE
            // elements: v
            // token labels: v
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleTokenStream stream_v=new RewriteRuleTokenStream(adaptor,"token v",v);
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 346:5: -> ^( SCALING_FACTOR $v)
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:346:7: ^( SCALING_FACTOR $v)
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(SCALING_FACTOR, "SCALING_FACTOR"), root_1);

                adaptor.addChild(root_1, stream_v.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "scaling_factor"

    public static class sign_sequence_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "sign_sequence"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:349:1: sign_sequence : (v= SIGNED_KEYWORD | v= UNSIGNED_KEYWORD ) -> ^( SIGNED $v) ;
    public final PLIStructureParser.sign_sequence_return sign_sequence() throws RecognitionException {
        PLIStructureParser.sign_sequence_return retval = new PLIStructureParser.sign_sequence_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token v=null;

        Object v_tree=null;
        RewriteRuleTokenStream stream_UNSIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_KEYWORD");
        RewriteRuleTokenStream stream_SIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token SIGNED_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:349:14: ( (v= SIGNED_KEYWORD | v= UNSIGNED_KEYWORD ) -> ^( SIGNED $v) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:350:5: (v= SIGNED_KEYWORD | v= UNSIGNED_KEYWORD )
            {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:350:5: (v= SIGNED_KEYWORD | v= UNSIGNED_KEYWORD )
            int alt28=2;
            int LA28_0 = input.LA(1);

            if ( (LA28_0==SIGNED_KEYWORD) ) {
                alt28=1;
            }
            else if ( (LA28_0==UNSIGNED_KEYWORD) ) {
                alt28=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 28, 0, input);

                throw nvae;
            }
            switch (alt28) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:350:6: v= SIGNED_KEYWORD
                    {
                    v=(Token)match(input,SIGNED_KEYWORD,FOLLOW_SIGNED_KEYWORD_in_sign_sequence1771); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SIGNED_KEYWORD.add(v);


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:350:25: v= UNSIGNED_KEYWORD
                    {
                    v=(Token)match(input,UNSIGNED_KEYWORD,FOLLOW_UNSIGNED_KEYWORD_in_sign_sequence1777); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNSIGNED_KEYWORD.add(v);


                    }
                    break;

            }



            // AST REWRITE
            // elements: v
            // token labels: v
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleTokenStream stream_v=new RewriteRuleTokenStream(adaptor,"token v",v);
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 351:5: -> ^( SIGNED $v)
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:351:7: ^( SIGNED $v)
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(SIGNED, "SIGNED"), root_1);

                adaptor.addChild(root_1, stream_v.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "sign_sequence"

    public static class dimension_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "dimension_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:357:1: dimension_attribute : DIMENSION_KEYWORD implicit_dimension_attribute ;
    public final PLIStructureParser.dimension_attribute_return dimension_attribute() throws RecognitionException {
        PLIStructureParser.dimension_attribute_return retval = new PLIStructureParser.dimension_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token DIMENSION_KEYWORD105=null;
        PLIStructureParser.implicit_dimension_attribute_return implicit_dimension_attribute106 = null;


        Object DIMENSION_KEYWORD105_tree=null;

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:357:20: ( DIMENSION_KEYWORD implicit_dimension_attribute )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:358:5: DIMENSION_KEYWORD implicit_dimension_attribute
            {
            root_0 = (Object)adaptor.nil();

            DIMENSION_KEYWORD105=(Token)match(input,DIMENSION_KEYWORD,FOLLOW_DIMENSION_KEYWORD_in_dimension_attribute1808); if (state.failed) return retval;
            pushFollow(FOLLOW_implicit_dimension_attribute_in_dimension_attribute1811);
            implicit_dimension_attribute106=implicit_dimension_attribute();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, implicit_dimension_attribute106.getTree());

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "dimension_attribute"

    public static class implicit_dimension_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "implicit_dimension_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:361:1: implicit_dimension_attribute : LEFT_PAREN bound_attribute ( COMMA bound_attribute )* RIGHT_PAREN -> ^( DIMENSIONS ( bound_attribute )+ ) ;
    public final PLIStructureParser.implicit_dimension_attribute_return implicit_dimension_attribute() throws RecognitionException {
        PLIStructureParser.implicit_dimension_attribute_return retval = new PLIStructureParser.implicit_dimension_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token LEFT_PAREN107=null;
        Token COMMA109=null;
        Token RIGHT_PAREN111=null;
        PLIStructureParser.bound_attribute_return bound_attribute108 = null;

        PLIStructureParser.bound_attribute_return bound_attribute110 = null;


        Object LEFT_PAREN107_tree=null;
        Object COMMA109_tree=null;
        Object RIGHT_PAREN111_tree=null;
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_COMMA=new RewriteRuleTokenStream(adaptor,"token COMMA");
        RewriteRuleSubtreeStream stream_bound_attribute=new RewriteRuleSubtreeStream(adaptor,"rule bound_attribute");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:361:29: ( LEFT_PAREN bound_attribute ( COMMA bound_attribute )* RIGHT_PAREN -> ^( DIMENSIONS ( bound_attribute )+ ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:362:5: LEFT_PAREN bound_attribute ( COMMA bound_attribute )* RIGHT_PAREN
            {
            LEFT_PAREN107=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_implicit_dimension_attribute1831); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN107);

            pushFollow(FOLLOW_bound_attribute_in_implicit_dimension_attribute1833);
            bound_attribute108=bound_attribute();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_bound_attribute.add(bound_attribute108.getTree());
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:362:32: ( COMMA bound_attribute )*
            loop29:
            do {
                int alt29=2;
                int LA29_0 = input.LA(1);

                if ( (LA29_0==COMMA) ) {
                    alt29=1;
                }


                switch (alt29) {
            	case 1 :
            	    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:362:33: COMMA bound_attribute
            	    {
            	    COMMA109=(Token)match(input,COMMA,FOLLOW_COMMA_in_implicit_dimension_attribute1836); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_COMMA.add(COMMA109);

            	    pushFollow(FOLLOW_bound_attribute_in_implicit_dimension_attribute1838);
            	    bound_attribute110=bound_attribute();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_bound_attribute.add(bound_attribute110.getTree());

            	    }
            	    break;

            	default :
            	    break loop29;
                }
            } while (true);

            RIGHT_PAREN111=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_implicit_dimension_attribute1842); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN111);



            // AST REWRITE
            // elements: bound_attribute
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 363:5: -> ^( DIMENSIONS ( bound_attribute )+ )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:363:7: ^( DIMENSIONS ( bound_attribute )+ )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(DIMENSIONS, "DIMENSIONS"), root_1);

                if ( !(stream_bound_attribute.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_bound_attribute.hasNext() ) {
                    adaptor.addChild(root_1, stream_bound_attribute.nextTree());

                }
                stream_bound_attribute.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "implicit_dimension_attribute"

    public static class bound_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "bound_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:366:1: bound_attribute : ( double_bound_expression -> ^( DIMENSION double_bound_expression ) | single_bound_expression -> ^( DIMENSION single_bound_expression ) );
    public final PLIStructureParser.bound_attribute_return bound_attribute() throws RecognitionException {
        PLIStructureParser.bound_attribute_return retval = new PLIStructureParser.bound_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        PLIStructureParser.double_bound_expression_return double_bound_expression112 = null;

        PLIStructureParser.single_bound_expression_return single_bound_expression113 = null;


        RewriteRuleSubtreeStream stream_single_bound_expression=new RewriteRuleSubtreeStream(adaptor,"rule single_bound_expression");
        RewriteRuleSubtreeStream stream_double_bound_expression=new RewriteRuleSubtreeStream(adaptor,"rule double_bound_expression");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:366:16: ( double_bound_expression -> ^( DIMENSION double_bound_expression ) | single_bound_expression -> ^( DIMENSION single_bound_expression ) )
            int alt30=2;
            int LA30_0 = input.LA(1);

            if ( (LA30_0==UNSIGNED_INTEGER) ) {
                switch ( input.LA(2) ) {
                case REFER_KEYWORD:
                    {
                    int LA30_2 = input.LA(3);

                    if ( (LA30_2==LEFT_PAREN) ) {
                        int LA30_5 = input.LA(4);

                        if ( (LA30_5==DATA_ITEM_NAME) ) {
                            int LA30_6 = input.LA(5);

                            if ( (LA30_6==RIGHT_PAREN) ) {
                                int LA30_7 = input.LA(6);

                                if ( (LA30_7==COLUMN) ) {
                                    alt30=1;
                                }
                                else if ( (LA30_7==COMMA||LA30_7==RIGHT_PAREN) ) {
                                    alt30=2;
                                }
                                else {
                                    if (state.backtracking>0) {state.failed=true; return retval;}
                                    NoViableAltException nvae =
                                        new NoViableAltException("", 30, 7, input);

                                    throw nvae;
                                }
                            }
                            else {
                                if (state.backtracking>0) {state.failed=true; return retval;}
                                NoViableAltException nvae =
                                    new NoViableAltException("", 30, 6, input);

                                throw nvae;
                            }
                        }
                        else {
                            if (state.backtracking>0) {state.failed=true; return retval;}
                            NoViableAltException nvae =
                                new NoViableAltException("", 30, 5, input);

                            throw nvae;
                        }
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 30, 2, input);

                        throw nvae;
                    }
                    }
                    break;
                case COLUMN:
                    {
                    alt30=1;
                    }
                    break;
                case COMMA:
                case RIGHT_PAREN:
                    {
                    alt30=2;
                    }
                    break;
                default:
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 30, 1, input);

                    throw nvae;
                }

            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 30, 0, input);

                throw nvae;
            }
            switch (alt30) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:367:5: double_bound_expression
                    {
                    pushFollow(FOLLOW_double_bound_expression_in_bound_attribute1874);
                    double_bound_expression112=double_bound_expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_double_bound_expression.add(double_bound_expression112.getTree());


                    // AST REWRITE
                    // elements: double_bound_expression
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 367:29: -> ^( DIMENSION double_bound_expression )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:367:31: ^( DIMENSION double_bound_expression )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(DIMENSION, "DIMENSION"), root_1);

                        adaptor.addChild(root_1, stream_double_bound_expression.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:368:7: single_bound_expression
                    {
                    pushFollow(FOLLOW_single_bound_expression_in_bound_attribute1889);
                    single_bound_expression113=single_bound_expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_single_bound_expression.add(single_bound_expression113.getTree());


                    // AST REWRITE
                    // elements: single_bound_expression
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 368:31: -> ^( DIMENSION single_bound_expression )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:368:33: ^( DIMENSION single_bound_expression )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(DIMENSION, "DIMENSION"), root_1);

                        adaptor.addChild(root_1, stream_single_bound_expression.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "bound_attribute"

    public static class double_bound_expression_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "double_bound_expression"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:371:1: double_bound_expression : UNSIGNED_INTEGER (v= refer_specification )? COLUMN UNSIGNED_INTEGER (w= refer_specification )? -> ^( LBOUND UNSIGNED_INTEGER ( $v)? ) ^( HBOUND UNSIGNED_INTEGER ( $w)? ) ;
    public final PLIStructureParser.double_bound_expression_return double_bound_expression() throws RecognitionException {
        PLIStructureParser.double_bound_expression_return retval = new PLIStructureParser.double_bound_expression_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token UNSIGNED_INTEGER114=null;
        Token COLUMN115=null;
        Token UNSIGNED_INTEGER116=null;
        PLIStructureParser.refer_specification_return v = null;

        PLIStructureParser.refer_specification_return w = null;


        Object UNSIGNED_INTEGER114_tree=null;
        Object COLUMN115_tree=null;
        Object UNSIGNED_INTEGER116_tree=null;
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleTokenStream stream_COLUMN=new RewriteRuleTokenStream(adaptor,"token COLUMN");
        RewriteRuleSubtreeStream stream_refer_specification=new RewriteRuleSubtreeStream(adaptor,"rule refer_specification");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:371:24: ( UNSIGNED_INTEGER (v= refer_specification )? COLUMN UNSIGNED_INTEGER (w= refer_specification )? -> ^( LBOUND UNSIGNED_INTEGER ( $v)? ) ^( HBOUND UNSIGNED_INTEGER ( $w)? ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:372:5: UNSIGNED_INTEGER (v= refer_specification )? COLUMN UNSIGNED_INTEGER (w= refer_specification )?
            {
            UNSIGNED_INTEGER114=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_double_bound_expression1916); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER114);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:372:23: (v= refer_specification )?
            int alt31=2;
            int LA31_0 = input.LA(1);

            if ( (LA31_0==REFER_KEYWORD) ) {
                alt31=1;
            }
            switch (alt31) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:372:23: v= refer_specification
                    {
                    pushFollow(FOLLOW_refer_specification_in_double_bound_expression1920);
                    v=refer_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_refer_specification.add(v.getTree());

                    }
                    break;

            }

            COLUMN115=(Token)match(input,COLUMN,FOLLOW_COLUMN_in_double_bound_expression1923); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_COLUMN.add(COLUMN115);

            UNSIGNED_INTEGER116=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_double_bound_expression1925); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER116);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:372:70: (w= refer_specification )?
            int alt32=2;
            int LA32_0 = input.LA(1);

            if ( (LA32_0==REFER_KEYWORD) ) {
                alt32=1;
            }
            switch (alt32) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:372:70: w= refer_specification
                    {
                    pushFollow(FOLLOW_refer_specification_in_double_bound_expression1929);
                    w=refer_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_refer_specification.add(w.getTree());

                    }
                    break;

            }



            // AST REWRITE
            // elements: UNSIGNED_INTEGER, UNSIGNED_INTEGER, w, v
            // token labels: 
            // rule labels: w, v, retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_w=new RewriteRuleSubtreeStream(adaptor,"rule w",w!=null?w.tree:null);
            RewriteRuleSubtreeStream stream_v=new RewriteRuleSubtreeStream(adaptor,"rule v",v!=null?v.tree:null);
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 373:5: -> ^( LBOUND UNSIGNED_INTEGER ( $v)? ) ^( HBOUND UNSIGNED_INTEGER ( $w)? )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:373:7: ^( LBOUND UNSIGNED_INTEGER ( $v)? )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(LBOUND, "LBOUND"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:373:33: ( $v)?
                if ( stream_v.hasNext() ) {
                    adaptor.addChild(root_1, stream_v.nextTree());

                }
                stream_v.reset();

                adaptor.addChild(root_0, root_1);
                }
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:373:38: ^( HBOUND UNSIGNED_INTEGER ( $w)? )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(HBOUND, "HBOUND"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:373:64: ( $w)?
                if ( stream_w.hasNext() ) {
                    adaptor.addChild(root_1, stream_w.nextTree());

                }
                stream_w.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "double_bound_expression"

    public static class single_bound_expression_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "single_bound_expression"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:376:1: single_bound_expression : UNSIGNED_INTEGER ( refer_specification )? -> ^( HBOUND UNSIGNED_INTEGER ( refer_specification )? ) ;
    public final PLIStructureParser.single_bound_expression_return single_bound_expression() throws RecognitionException {
        PLIStructureParser.single_bound_expression_return retval = new PLIStructureParser.single_bound_expression_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token UNSIGNED_INTEGER117=null;
        PLIStructureParser.refer_specification_return refer_specification118 = null;


        Object UNSIGNED_INTEGER117_tree=null;
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleSubtreeStream stream_refer_specification=new RewriteRuleSubtreeStream(adaptor,"rule refer_specification");
        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:376:24: ( UNSIGNED_INTEGER ( refer_specification )? -> ^( HBOUND UNSIGNED_INTEGER ( refer_specification )? ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:377:5: UNSIGNED_INTEGER ( refer_specification )?
            {
            UNSIGNED_INTEGER117=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_single_bound_expression1975); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(UNSIGNED_INTEGER117);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:377:22: ( refer_specification )?
            int alt33=2;
            int LA33_0 = input.LA(1);

            if ( (LA33_0==REFER_KEYWORD) ) {
                alt33=1;
            }
            switch (alt33) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:377:22: refer_specification
                    {
                    pushFollow(FOLLOW_refer_specification_in_single_bound_expression1977);
                    refer_specification118=refer_specification();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_refer_specification.add(refer_specification118.getTree());

                    }
                    break;

            }



            // AST REWRITE
            // elements: UNSIGNED_INTEGER, refer_specification
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 378:5: -> ^( HBOUND UNSIGNED_INTEGER ( refer_specification )? )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:378:7: ^( HBOUND UNSIGNED_INTEGER ( refer_specification )? )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(HBOUND, "HBOUND"), root_1);

                adaptor.addChild(root_1, stream_UNSIGNED_INTEGER.nextNode());
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:378:33: ( refer_specification )?
                if ( stream_refer_specification.hasNext() ) {
                    adaptor.addChild(root_1, stream_refer_specification.nextTree());

                }
                stream_refer_specification.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "single_bound_expression"

    public static class alignment_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "alignment_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:384:1: alignment_attribute : ( ALIGNED_KEYWORD -> ^( ALIGNMENT ALIGNED ) | UNALIGNED_KEYWORD -> ^( ALIGNMENT UNALIGNED ) );
    public final PLIStructureParser.alignment_attribute_return alignment_attribute() throws RecognitionException {
        PLIStructureParser.alignment_attribute_return retval = new PLIStructureParser.alignment_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ALIGNED_KEYWORD119=null;
        Token UNALIGNED_KEYWORD120=null;

        Object ALIGNED_KEYWORD119_tree=null;
        Object UNALIGNED_KEYWORD120_tree=null;
        RewriteRuleTokenStream stream_ALIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token ALIGNED_KEYWORD");
        RewriteRuleTokenStream stream_UNALIGNED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNALIGNED_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:384:20: ( ALIGNED_KEYWORD -> ^( ALIGNMENT ALIGNED ) | UNALIGNED_KEYWORD -> ^( ALIGNMENT UNALIGNED ) )
            int alt34=2;
            int LA34_0 = input.LA(1);

            if ( (LA34_0==ALIGNED_KEYWORD) ) {
                alt34=1;
            }
            else if ( (LA34_0==UNALIGNED_KEYWORD) ) {
                alt34=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 34, 0, input);

                throw nvae;
            }
            switch (alt34) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:385:5: ALIGNED_KEYWORD
                    {
                    ALIGNED_KEYWORD119=(Token)match(input,ALIGNED_KEYWORD,FOLLOW_ALIGNED_KEYWORD_in_alignment_attribute2014); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ALIGNED_KEYWORD.add(ALIGNED_KEYWORD119);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 385:21: -> ^( ALIGNMENT ALIGNED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:385:24: ^( ALIGNMENT ALIGNED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ALIGNMENT, "ALIGNMENT"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(ALIGNED, "ALIGNED"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:386:7: UNALIGNED_KEYWORD
                    {
                    UNALIGNED_KEYWORD120=(Token)match(input,UNALIGNED_KEYWORD,FOLLOW_UNALIGNED_KEYWORD_in_alignment_attribute2030); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNALIGNED_KEYWORD.add(UNALIGNED_KEYWORD120);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 386:25: -> ^( ALIGNMENT UNALIGNED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:386:28: ^( ALIGNMENT UNALIGNED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ALIGNMENT, "ALIGNMENT"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(UNALIGNED, "UNALIGNED"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "alignment_attribute"

    public static class initial_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "initial_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:392:1: initial_attribute : INITIAL_KEYWORD LEFT_PAREN (v= STRING_LITERAL | v= SIGNED_INTEGER | v= UNSIGNED_INTEGER | v= FLOAT ) RIGHT_PAREN -> ^( INITIAL $v) ;
    public final PLIStructureParser.initial_attribute_return initial_attribute() throws RecognitionException {
        PLIStructureParser.initial_attribute_return retval = new PLIStructureParser.initial_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token v=null;
        Token INITIAL_KEYWORD121=null;
        Token LEFT_PAREN122=null;
        Token RIGHT_PAREN123=null;

        Object v_tree=null;
        Object INITIAL_KEYWORD121_tree=null;
        Object LEFT_PAREN122_tree=null;
        Object RIGHT_PAREN123_tree=null;
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_UNSIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token UNSIGNED_INTEGER");
        RewriteRuleTokenStream stream_STRING_LITERAL=new RewriteRuleTokenStream(adaptor,"token STRING_LITERAL");
        RewriteRuleTokenStream stream_FLOAT=new RewriteRuleTokenStream(adaptor,"token FLOAT");
        RewriteRuleTokenStream stream_INITIAL_KEYWORD=new RewriteRuleTokenStream(adaptor,"token INITIAL_KEYWORD");
        RewriteRuleTokenStream stream_SIGNED_INTEGER=new RewriteRuleTokenStream(adaptor,"token SIGNED_INTEGER");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:392:18: ( INITIAL_KEYWORD LEFT_PAREN (v= STRING_LITERAL | v= SIGNED_INTEGER | v= UNSIGNED_INTEGER | v= FLOAT ) RIGHT_PAREN -> ^( INITIAL $v) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:5: INITIAL_KEYWORD LEFT_PAREN (v= STRING_LITERAL | v= SIGNED_INTEGER | v= UNSIGNED_INTEGER | v= FLOAT ) RIGHT_PAREN
            {
            INITIAL_KEYWORD121=(Token)match(input,INITIAL_KEYWORD,FOLLOW_INITIAL_KEYWORD_in_initial_attribute2056); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_INITIAL_KEYWORD.add(INITIAL_KEYWORD121);

            LEFT_PAREN122=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_initial_attribute2058); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN122);

            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:32: (v= STRING_LITERAL | v= SIGNED_INTEGER | v= UNSIGNED_INTEGER | v= FLOAT )
            int alt35=4;
            switch ( input.LA(1) ) {
            case STRING_LITERAL:
                {
                alt35=1;
                }
                break;
            case SIGNED_INTEGER:
                {
                alt35=2;
                }
                break;
            case UNSIGNED_INTEGER:
                {
                alt35=3;
                }
                break;
            case FLOAT:
                {
                alt35=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 35, 0, input);

                throw nvae;
            }

            switch (alt35) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:33: v= STRING_LITERAL
                    {
                    v=(Token)match(input,STRING_LITERAL,FOLLOW_STRING_LITERAL_in_initial_attribute2063); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_STRING_LITERAL.add(v);


                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:52: v= SIGNED_INTEGER
                    {
                    v=(Token)match(input,SIGNED_INTEGER,FOLLOW_SIGNED_INTEGER_in_initial_attribute2069); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SIGNED_INTEGER.add(v);


                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:71: v= UNSIGNED_INTEGER
                    {
                    v=(Token)match(input,UNSIGNED_INTEGER,FOLLOW_UNSIGNED_INTEGER_in_initial_attribute2075); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_UNSIGNED_INTEGER.add(v);


                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:393:92: v= FLOAT
                    {
                    v=(Token)match(input,FLOAT,FOLLOW_FLOAT_in_initial_attribute2081); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_FLOAT.add(v);


                    }
                    break;

            }

            RIGHT_PAREN123=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_initial_attribute2084); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN123);



            // AST REWRITE
            // elements: v
            // token labels: v
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleTokenStream stream_v=new RewriteRuleTokenStream(adaptor,"token v",v);
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 394:5: -> ^( INITIAL $v)
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:394:7: ^( INITIAL $v)
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(INITIAL, "INITIAL"), root_1);

                adaptor.addChild(root_1, stream_v.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "initial_attribute"

    public static class storage_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "storage_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:400:1: storage_attribute : ( AUTOMATIC_KEYWORD -> ^( STORAGE AUTOMATIC ) | STATIC_KEYWORD -> ^( STORAGE STATIC ) | BASED_KEYWORD ( LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN )? -> ^( STORAGE BASED ) | CONTROLLED_KEYWORD -> ^( STORAGE CONTROLLED ) );
    public final PLIStructureParser.storage_attribute_return storage_attribute() throws RecognitionException {
        PLIStructureParser.storage_attribute_return retval = new PLIStructureParser.storage_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token AUTOMATIC_KEYWORD124=null;
        Token STATIC_KEYWORD125=null;
        Token BASED_KEYWORD126=null;
        Token LEFT_PAREN127=null;
        Token DATA_ITEM_NAME128=null;
        Token RIGHT_PAREN129=null;
        Token CONTROLLED_KEYWORD130=null;

        Object AUTOMATIC_KEYWORD124_tree=null;
        Object STATIC_KEYWORD125_tree=null;
        Object BASED_KEYWORD126_tree=null;
        Object LEFT_PAREN127_tree=null;
        Object DATA_ITEM_NAME128_tree=null;
        Object RIGHT_PAREN129_tree=null;
        Object CONTROLLED_KEYWORD130_tree=null;
        RewriteRuleTokenStream stream_DATA_ITEM_NAME=new RewriteRuleTokenStream(adaptor,"token DATA_ITEM_NAME");
        RewriteRuleTokenStream stream_BASED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token BASED_KEYWORD");
        RewriteRuleTokenStream stream_LEFT_PAREN=new RewriteRuleTokenStream(adaptor,"token LEFT_PAREN");
        RewriteRuleTokenStream stream_RIGHT_PAREN=new RewriteRuleTokenStream(adaptor,"token RIGHT_PAREN");
        RewriteRuleTokenStream stream_AUTOMATIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token AUTOMATIC_KEYWORD");
        RewriteRuleTokenStream stream_CONTROLLED_KEYWORD=new RewriteRuleTokenStream(adaptor,"token CONTROLLED_KEYWORD");
        RewriteRuleTokenStream stream_STATIC_KEYWORD=new RewriteRuleTokenStream(adaptor,"token STATIC_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:400:18: ( AUTOMATIC_KEYWORD -> ^( STORAGE AUTOMATIC ) | STATIC_KEYWORD -> ^( STORAGE STATIC ) | BASED_KEYWORD ( LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN )? -> ^( STORAGE BASED ) | CONTROLLED_KEYWORD -> ^( STORAGE CONTROLLED ) )
            int alt37=4;
            switch ( input.LA(1) ) {
            case AUTOMATIC_KEYWORD:
                {
                alt37=1;
                }
                break;
            case STATIC_KEYWORD:
                {
                alt37=2;
                }
                break;
            case BASED_KEYWORD:
                {
                alt37=3;
                }
                break;
            case CONTROLLED_KEYWORD:
                {
                alt37=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 37, 0, input);

                throw nvae;
            }

            switch (alt37) {
                case 1 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:401:5: AUTOMATIC_KEYWORD
                    {
                    AUTOMATIC_KEYWORD124=(Token)match(input,AUTOMATIC_KEYWORD,FOLLOW_AUTOMATIC_KEYWORD_in_storage_attribute2114); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_AUTOMATIC_KEYWORD.add(AUTOMATIC_KEYWORD124);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 401:23: -> ^( STORAGE AUTOMATIC )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:401:26: ^( STORAGE AUTOMATIC )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STORAGE, "STORAGE"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(AUTOMATIC, "AUTOMATIC"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:402:7: STATIC_KEYWORD
                    {
                    STATIC_KEYWORD125=(Token)match(input,STATIC_KEYWORD,FOLLOW_STATIC_KEYWORD_in_storage_attribute2130); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_STATIC_KEYWORD.add(STATIC_KEYWORD125);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 402:22: -> ^( STORAGE STATIC )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:402:25: ^( STORAGE STATIC )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STORAGE, "STORAGE"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(STATIC, "STATIC"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:403:7: BASED_KEYWORD ( LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN )?
                    {
                    BASED_KEYWORD126=(Token)match(input,BASED_KEYWORD,FOLLOW_BASED_KEYWORD_in_storage_attribute2146); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_BASED_KEYWORD.add(BASED_KEYWORD126);

                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:403:21: ( LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN )?
                    int alt36=2;
                    int LA36_0 = input.LA(1);

                    if ( (LA36_0==LEFT_PAREN) ) {
                        alt36=1;
                    }
                    switch (alt36) {
                        case 1 :
                            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:403:22: LEFT_PAREN DATA_ITEM_NAME RIGHT_PAREN
                            {
                            LEFT_PAREN127=(Token)match(input,LEFT_PAREN,FOLLOW_LEFT_PAREN_in_storage_attribute2149); if (state.failed) return retval; 
                            if ( state.backtracking==0 ) stream_LEFT_PAREN.add(LEFT_PAREN127);

                            DATA_ITEM_NAME128=(Token)match(input,DATA_ITEM_NAME,FOLLOW_DATA_ITEM_NAME_in_storage_attribute2151); if (state.failed) return retval; 
                            if ( state.backtracking==0 ) stream_DATA_ITEM_NAME.add(DATA_ITEM_NAME128);

                            RIGHT_PAREN129=(Token)match(input,RIGHT_PAREN,FOLLOW_RIGHT_PAREN_in_storage_attribute2153); if (state.failed) return retval; 
                            if ( state.backtracking==0 ) stream_RIGHT_PAREN.add(RIGHT_PAREN129);


                            }
                            break;

                    }



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 403:62: -> ^( STORAGE BASED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:403:65: ^( STORAGE BASED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STORAGE, "STORAGE"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(BASED, "BASED"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:404:7: CONTROLLED_KEYWORD
                    {
                    CONTROLLED_KEYWORD130=(Token)match(input,CONTROLLED_KEYWORD,FOLLOW_CONTROLLED_KEYWORD_in_storage_attribute2171); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_CONTROLLED_KEYWORD.add(CONTROLLED_KEYWORD130);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 404:26: -> ^( STORAGE CONTROLLED )
                    {
                        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:404:29: ^( STORAGE CONTROLLED )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(STORAGE, "STORAGE"), root_1);

                        adaptor.addChild(root_1, (Object)adaptor.create(CONTROLLED, "CONTROLLED"));

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "storage_attribute"

    public static class union_attribute_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "union_attribute"
    // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:410:1: union_attribute : UNION_KEYWORD -> ^( UNION ) ;
    public final PLIStructureParser.union_attribute_return union_attribute() throws RecognitionException {
        PLIStructureParser.union_attribute_return retval = new PLIStructureParser.union_attribute_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token UNION_KEYWORD131=null;

        Object UNION_KEYWORD131_tree=null;
        RewriteRuleTokenStream stream_UNION_KEYWORD=new RewriteRuleTokenStream(adaptor,"token UNION_KEYWORD");

        try {
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:410:16: ( UNION_KEYWORD -> ^( UNION ) )
            // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:411:5: UNION_KEYWORD
            {
            UNION_KEYWORD131=(Token)match(input,UNION_KEYWORD,FOLLOW_UNION_KEYWORD_in_union_attribute2197); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_UNION_KEYWORD.add(UNION_KEYWORD131);



            // AST REWRITE
            // elements: 
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 411:19: -> ^( UNION )
            {
                // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:411:22: ^( UNION )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(UNION, "UNION"), root_1);

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "union_attribute"

    // $ANTLR start synpred1_PLIStructureParser
    public final void synpred1_PLIStructureParser_fragment() throws RecognitionException {   
        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:137:7: ( declare )
        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:137:8: declare
        {
        pushFollow(FOLLOW_declare_in_synpred1_PLIStructureParser298);
        declare();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred1_PLIStructureParser

    // $ANTLR start synpred2_PLIStructureParser
    public final void synpred2_PLIStructureParser_fragment() throws RecognitionException {   
        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:161:9: ( data_item_with_children )
        // D:\\Legsem\\Legstar\\sub-projects\\legstar-pli2cob\\src\\main\\antlr3/com/legstar/pli2cob/PLIStructureParser.g:161:10: data_item_with_children
        {
        pushFollow(FOLLOW_data_item_with_children_in_synpred2_PLIStructureParser390);
        data_item_with_children();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred2_PLIStructureParser

    // Delegated rules

    public final boolean synpred2_PLIStructureParser() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred2_PLIStructureParser_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred1_PLIStructureParser() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_PLIStructureParser_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA4 dfa4 = new DFA4(this);
    static final String DFA4_eotS =
        "\u00b8\uffff";
    static final String DFA4_eofS =
        "\2\uffff\36\74\1\uffff\4\74\4\uffff\10\74\2\uffff\2\74\1\uffff"+
        "\5\74\4\uffff\10\74\2\uffff\10\74\7\uffff\1\74\20\uffff\1\74\2\uffff"+
        "\1\74\2\uffff\1\74\3\uffff\3\74\6\uffff\1\74\3\uffff\1\74\2\uffff"+
        "\10\74\40\uffff\1\74\14\uffff";
    static final String DFA4_minS =
        "\2\55\36\56\1\60\4\56\3\72\1\120\10\56\2\116\2\56\1\116\5\56\2"+
        "\uffff\1\57\1\60\10\56\1\116\1\60\10\56\2\60\1\13\1\61\1\116\2\60"+
        "\1\56\1\100\1\60\1\57\1\116\1\60\2\57\5\117\1\61\2\57\1\116\1\56"+
        "\1\100\1\60\1\56\1\57\1\60\1\56\1\116\2\60\3\56\1\117\2\116\1\60"+
        "\1\61\1\116\1\56\2\117\1\60\1\56\2\117\10\56\1\61\3\57\2\61\1\57"+
        "\1\117\1\61\2\117\1\116\1\60\1\117\2\116\1\60\2\117\1\116\2\117"+
        "\2\57\2\61\3\57\1\61\1\117\1\60\1\56\2\117\1\116\3\117\2\57\1\61"+
        "\1\57\1\117\1\57";
    static final String DFA4_maxS =
        "\2\115\36\116\1\60\4\116\3\77\1\120\12\116\2\115\1\116\2\115\1"+
        "\116\2\115\2\uffff\1\122\1\60\3\115\4\116\1\115\1\116\1\60\10\116"+
        "\2\60\1\121\1\61\1\116\2\60\1\115\1\117\1\60\1\117\1\116\1\60\1"+
        "\117\1\122\5\117\1\61\1\117\1\122\1\116\1\115\1\117\1\121\1\115"+
        "\1\117\1\121\1\115\1\116\2\60\3\115\1\117\2\116\1\60\1\61\1\116"+
        "\1\115\2\117\1\121\1\115\2\117\10\116\1\61\1\117\2\122\2\61\2\117"+
        "\1\61\2\117\1\116\1\60\1\117\2\116\1\60\2\117\1\116\3\117\1\122"+
        "\2\61\2\117\1\122\1\61\1\117\1\121\1\115\2\117\1\116\4\117\1\122"+
        "\1\61\3\117";
    static final String DFA4_acceptS =
        "\73\uffff\1\1\1\2\173\uffff";
    static final String DFA4_specialS =
        "\2\uffff\1\47\1\51\1\43\1\45\1\34\1\37\1\30\1\32\1\65\1\70\1\61"+
        "\1\63\1\54\1\56\1\52\1\53\1\13\1\11\1\10\1\6\1\4\1\3\1\2\1\1\1\26"+
        "\1\25\1\24\1\23\1\20\1\16\1\uffff\1\122\1\120\1\116\1\115\4\uffff"+
        "\1\40\1\35\1\44\1\42\1\50\1\46\1\101\1\103\2\uffff\1\72\1\73\1\uffff"+
        "\1\5\1\7\1\66\1\27\1\102\4\uffff\1\77\1\76\1\74\1\104\1\105\1\107"+
        "\1\110\1\36\2\uffff\1\113\1\112\1\111\1\14\1\15\1\17\1\21\1\22\7"+
        "\uffff\1\41\20\uffff\1\117\2\uffff\1\75\2\uffff\1\71\3\uffff\1\0"+
        "\1\114\1\106\6\uffff\1\12\3\uffff\1\121\2\uffff\1\57\1\55\1\62\1"+
        "\60\1\67\1\64\1\33\1\31\40\uffff\1\100\14\uffff}>";
    static final String[] DFA4_transitionS = {
            "\1\4\2\uffff\1\1\1\2\1\3\1\5\1\6\1\7\1\10\1\11\1\12\1\13\1"+
            "\14\1\15\1\16\1\17\1\20\1\21\1\22\1\23\1\24\1\25\1\26\1\27\1"+
            "\30\1\31\1\32\1\33\1\34\1\35\1\36\1\37",
            "\1\4\3\uffff\1\2\1\3\1\5\1\6\1\7\1\10\1\11\1\12\1\13\1\14"+
            "\1\15\1\16\1\17\1\20\1\21\1\22\1\23\1\24\1\25\1\26\1\27\1\30"+
            "\1\31\1\32\1\33\1\34\1\35\1\36\1\37",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\40",
            "\1\75",
            "\1\74\1\73\21\uffff\1\77\1\100\1\101\1\uffff\1\62\1\63\1\64"+
            "\1\65\1\66\1\67\1\70\1\71\1\72\1\76",
            "\1\74\1\73\21\uffff\1\77\1\100\1\101\1\uffff\1\62\1\63\1\64"+
            "\1\65\1\66\1\67\1\70\1\71\1\72\1\76",
            "\1\74\1\73\21\uffff\1\77\1\100\1\101\1\uffff\1\62\1\63\1\64"+
            "\1\65\1\66\1\67\1\70\1\71\1\72\1\76",
            "\1\74\1\73\21\uffff\1\77\1\100\1\101\1\uffff\1\62\1\63\1\64"+
            "\1\65\1\66\1\67\1\70\1\71\1\72\1\76",
            "\1\102\2\uffff\1\103\1\105\1\104",
            "\1\102\2\uffff\1\103\1\105\1\104",
            "\1\102\2\uffff\1\103\1\105\1\104",
            "\1\106",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\107\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\110",
            "\1\121",
            "\1\122",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\123",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72\1\124",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "",
            "",
            "\1\127\20\uffff\1\125\16\uffff\1\130\2\uffff\1\126",
            "\1\131",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72\1\132",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72\1\132",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72\1\132",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72\1\132",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\110",
            "\1\133",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\134\1\114\1\113"+
            "\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1\67"+
            "\1\70\1\71\1\72\1\135",
            "\1\136",
            "\1\137",
            "\1\143\44\uffff\1\142\37\uffff\1\140\1\141",
            "\1\144",
            "\1\145",
            "\1\146",
            "\1\147",
            "\1\74\1\73\3\uffff\1\57\1\60\1\52\1\51\1\61\1\54\1\53\1\41"+
            "\1\55\1\56\1\42\1\44\1\43\1\uffff\1\45\1\46\1\47\1\50\1\62\1"+
            "\63\1\64\1\65\1\66\1\67\1\70\1\71\1\72",
            "\1\150\16\uffff\1\151",
            "\1\152",
            "\1\153\37\uffff\1\154",
            "\1\135",
            "\1\155",
            "\1\156\37\uffff\1\157",
            "\1\162\20\uffff\1\160\16\uffff\1\163\2\uffff\1\161",
            "\1\164",
            "\1\164",
            "\1\164",
            "\1\164",
            "\1\165",
            "\1\166",
            "\1\127\20\uffff\1\167\16\uffff\1\130",
            "\1\127\20\uffff\1\170\16\uffff\1\130\2\uffff\1\171",
            "\1\172",
            "\1\74\1\73\21\uffff\1\77\1\100\1\101\1\uffff\1\62\1\63\1\64"+
            "\1\65\1\66\1\67\1\70\1\71\1\72",
            "\1\173\16\uffff\1\174",
            "\1\175\40\uffff\1\176",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\uffff\1\114\1"+
            "\113\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1"+
            "\67\1\70\1\71\1\72",
            "\1\177\37\uffff\1\u0080",
            "\1\u0081\40\uffff\1\u0082",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\uffff"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72",
            "\1\u008b",
            "\1\u008c",
            "\1\u008d",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\u008e",
            "\1\u008f",
            "\1\u0090",
            "\1\u0091",
            "\1\u0092",
            "\1\u0093",
            "\1\74\1\73\25\uffff\1\62\1\63\1\64\1\65\1\66\1\67\1\70\1\71"+
            "\1\72",
            "\1\154",
            "\1\154",
            "\1\u0094\40\uffff\1\u0095",
            "\1\74\1\73\3\uffff\1\117\1\120\1\112\1\111\1\uffff\1\114\1"+
            "\113\1\uffff\1\115\1\116\10\uffff\1\62\1\63\1\64\1\65\1\66\1"+
            "\67\1\70\1\71\1\72",
            "\1\157",
            "\1\157",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\u0096"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72\1\u0097",
            "\1\u0098",
            "\1\162\20\uffff\1\u0099\16\uffff\1\163",
            "\1\162\20\uffff\1\u009a\16\uffff\1\163\2\uffff\1\u009b",
            "\1\127\37\uffff\1\130\2\uffff\1\126",
            "\1\u009c",
            "\1\u009d",
            "\1\127\20\uffff\1\u009e\16\uffff\1\130",
            "\1\u009f",
            "\1\u00a0",
            "\1\u0080",
            "\1\u0080",
            "\1\u0097",
            "\1\u00a1",
            "\1\u00a2",
            "\1\u00a3",
            "\1\u00a4",
            "\1\u00a5",
            "\1\u00a6",
            "\1\u00a7",
            "\1\u00a8",
            "\1\151",
            "\1\u00a9",
            "\1\u00aa\37\uffff\1\u00ab",
            "\1\162\37\uffff\1\163\2\uffff\1\161",
            "\1\u00ac",
            "\1\u00ad",
            "\1\162\20\uffff\1\u00ae\16\uffff\1\163",
            "\1\127\37\uffff\1\130",
            "\1\127\37\uffff\1\130\2\uffff\1\171",
            "\1\u00af",
            "\1\174",
            "\1\u00b0\40\uffff\1\u00b1",
            "\1\74\1\73\3\uffff\1\u0089\1\u008a\1\u0084\1\u0083\1\uffff"+
            "\1\u0086\1\u0085\1\uffff\1\u0087\1\u0088\10\uffff\1\62\1\63"+
            "\1\64\1\65\1\66\1\67\1\70\1\71\1\72",
            "\1\u00b2",
            "\1\u00b3",
            "\1\u00b4",
            "\1\u00b5",
            "\1\u00ab",
            "\1\u00ab",
            "\1\162\37\uffff\1\163",
            "\1\162\37\uffff\1\163\2\uffff\1\u009b",
            "\1\u00b6",
            "\1\127\37\uffff\1\130",
            "\1\u00b7",
            "\1\162\37\uffff\1\163"
    };

    static final short[] DFA4_eot = DFA.unpackEncodedString(DFA4_eotS);
    static final short[] DFA4_eof = DFA.unpackEncodedString(DFA4_eofS);
    static final char[] DFA4_min = DFA.unpackEncodedStringToUnsignedChars(DFA4_minS);
    static final char[] DFA4_max = DFA.unpackEncodedStringToUnsignedChars(DFA4_maxS);
    static final short[] DFA4_accept = DFA.unpackEncodedString(DFA4_acceptS);
    static final short[] DFA4_special = DFA.unpackEncodedString(DFA4_specialS);
    static final short[][] DFA4_transition;

    static {
        int numStates = DFA4_transitionS.length;
        DFA4_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA4_transition[i] = DFA.unpackEncodedString(DFA4_transitionS[i]);
        }
    }

    class DFA4 extends DFA {

        public DFA4(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 4;
            this.eot = DFA4_eot;
            this.eof = DFA4_eof;
            this.min = DFA4_min;
            this.max = DFA4_max;
            this.accept = DFA4_accept;
            this.special = DFA4_special;
            this.transition = DFA4_transition;
        }
        public String getDescription() {
            return "160:1: data_items returns [int level] : ( ( data_item_with_children )=> data_item_with_children | data_item );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            TokenStream input = (TokenStream)_input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA4_115 = input.LA(1);

                         
                        int index4_115 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_115==EOF||LA4_115==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_115==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_115==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_115==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_115==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_115==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_115==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_115==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_115==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_115==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_115==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_115);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA4_25 = input.LA(1);

                         
                        int index4_25 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_25==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_25==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_25==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_25==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_25==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_25==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_25==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_25==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_25==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_25==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_25==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_25==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_25==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_25==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_25==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_25==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_25==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_25==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_25==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_25==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_25==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_25==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_25==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_25==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_25==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_25==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_25==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_25==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_25==EOF||LA4_25==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_25);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA4_24 = input.LA(1);

                         
                        int index4_24 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_24==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_24==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_24==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_24==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_24==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_24==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_24==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_24==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_24==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_24==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_24==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_24==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_24==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_24==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_24==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_24==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_24==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_24==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_24==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_24==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_24==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_24==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_24==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_24==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_24==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_24==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_24==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_24==EOF||LA4_24==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_24==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_24);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA4_23 = input.LA(1);

                         
                        int index4_23 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_23==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_23==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_23==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_23==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_23==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_23==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_23==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_23==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_23==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_23==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_23==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_23==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_23==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_23==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_23==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_23==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_23==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_23==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_23==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_23==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_23==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_23==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_23==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_23==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_23==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_23==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_23==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_23==EOF||LA4_23==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_23==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_23);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA4_22 = input.LA(1);

                         
                        int index4_22 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_22==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_22==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_22==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_22==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_22==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_22==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_22==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_22==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_22==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_22==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_22==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_22==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_22==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_22==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_22==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_22==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_22==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_22==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_22==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_22==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_22==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_22==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_22==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_22==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_22==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_22==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_22==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_22==EOF||LA4_22==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_22==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_22);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA4_54 = input.LA(1);

                         
                        int index4_54 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_54==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_54==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_54==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_54==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_54==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_54==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_54==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_54==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_54==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_54==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_54==EOF||LA4_54==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_54);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA4_21 = input.LA(1);

                         
                        int index4_21 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_21==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_21==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_21==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_21==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_21==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_21==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_21==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_21==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_21==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_21==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_21==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_21==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_21==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_21==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_21==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_21==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_21==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_21==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_21==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_21==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_21==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_21==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_21==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_21==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_21==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_21==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_21==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_21==EOF||LA4_21==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_21==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_21);
                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA4_55 = input.LA(1);

                         
                        int index4_55 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_55==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_55==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_55==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_55==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_55==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_55==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_55==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_55==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_55==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_55==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_55==EOF||LA4_55==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_55);
                        if ( s>=0 ) return s;
                        break;
                    case 8 : 
                        int LA4_20 = input.LA(1);

                         
                        int index4_20 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_20==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_20==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_20==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_20==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_20==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_20==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_20==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_20==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_20==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_20==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_20==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_20==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_20==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_20==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_20==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_20==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_20==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_20==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_20==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_20==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_20==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_20==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_20==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_20==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_20==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_20==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_20==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_20==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_20==EOF||LA4_20==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_20);
                        if ( s>=0 ) return s;
                        break;
                    case 9 : 
                        int LA4_19 = input.LA(1);

                         
                        int index4_19 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_19==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_19==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_19==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_19==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_19==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_19==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_19==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_19==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_19==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_19==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_19==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_19==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_19==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_19==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_19==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_19==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_19==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_19==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_19==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_19==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_19==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_19==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_19==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_19==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_19==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_19==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_19==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_19==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_19==EOF||LA4_19==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_19);
                        if ( s>=0 ) return s;
                        break;
                    case 10 : 
                        int LA4_124 = input.LA(1);

                         
                        int index4_124 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_124==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_124==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_124==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_124==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_124==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_124==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_124==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_124==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_124==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_124==EOF||LA4_124==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_124==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_124);
                        if ( s>=0 ) return s;
                        break;
                    case 11 : 
                        int LA4_18 = input.LA(1);

                         
                        int index4_18 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_18==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_18==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_18==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_18==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_18==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_18==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_18==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_18==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_18==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_18==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_18==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_18==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_18==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_18==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_18==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_18==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_18==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_18==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_18==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_18==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_18==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_18==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_18==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_18==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_18==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_18==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_18==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_18==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_18==EOF||LA4_18==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_18);
                        if ( s>=0 ) return s;
                        break;
                    case 12 : 
                        int LA4_76 = input.LA(1);

                         
                        int index4_76 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_76==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_76==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_76==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_76==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_76==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_76==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_76==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_76==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_76==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_76==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_76==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_76==EOF||LA4_76==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_76==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_76==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_76==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_76==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_76==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_76==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_76==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_76==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_76==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_76);
                        if ( s>=0 ) return s;
                        break;
                    case 13 : 
                        int LA4_77 = input.LA(1);

                         
                        int index4_77 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_77==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_77==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_77==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_77==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_77==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_77==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_77==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_77==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_77==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_77==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_77==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_77==EOF||LA4_77==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_77==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_77==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_77==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_77==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_77==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_77==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_77==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_77==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_77==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_77);
                        if ( s>=0 ) return s;
                        break;
                    case 14 : 
                        int LA4_31 = input.LA(1);

                         
                        int index4_31 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_31==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_31==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_31==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_31==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_31==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_31==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_31==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_31==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_31==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_31==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_31==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_31==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_31==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_31==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_31==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_31==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_31==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_31==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_31==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_31==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_31==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_31==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_31==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_31==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_31==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_31==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_31==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_31==EOF||LA4_31==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_31==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_31);
                        if ( s>=0 ) return s;
                        break;
                    case 15 : 
                        int LA4_78 = input.LA(1);

                         
                        int index4_78 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_78==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_78==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_78==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_78==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_78==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_78==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_78==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_78==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_78==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_78==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_78==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_78==EOF||LA4_78==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_78==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_78==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_78==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_78==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_78==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_78==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_78==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_78==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_78==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_78);
                        if ( s>=0 ) return s;
                        break;
                    case 16 : 
                        int LA4_30 = input.LA(1);

                         
                        int index4_30 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_30==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_30==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_30==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_30==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_30==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_30==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_30==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_30==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_30==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_30==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_30==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_30==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_30==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_30==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_30==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_30==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_30==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_30==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_30==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_30==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_30==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_30==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_30==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_30==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_30==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_30==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_30==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_30==EOF||LA4_30==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_30==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_30);
                        if ( s>=0 ) return s;
                        break;
                    case 17 : 
                        int LA4_79 = input.LA(1);

                         
                        int index4_79 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_79==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_79==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_79==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_79==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_79==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_79==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_79==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_79==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_79==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_79==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_79==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_79==EOF||LA4_79==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_79==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_79==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_79==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_79==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_79==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_79==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_79==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_79==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_79==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_79);
                        if ( s>=0 ) return s;
                        break;
                    case 18 : 
                        int LA4_80 = input.LA(1);

                         
                        int index4_80 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_80==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_80==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_80==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_80==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_80==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_80==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_80==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_80==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_80==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_80==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_80==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_80==EOF||LA4_80==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_80==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_80==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_80==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_80==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_80==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_80==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_80==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_80==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_80==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_80);
                        if ( s>=0 ) return s;
                        break;
                    case 19 : 
                        int LA4_29 = input.LA(1);

                         
                        int index4_29 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_29==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_29==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_29==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_29==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_29==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_29==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_29==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_29==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_29==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_29==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_29==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_29==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_29==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_29==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_29==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_29==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_29==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_29==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_29==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_29==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_29==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_29==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_29==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_29==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_29==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_29==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_29==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_29==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_29==EOF||LA4_29==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_29);
                        if ( s>=0 ) return s;
                        break;
                    case 20 : 
                        int LA4_28 = input.LA(1);

                         
                        int index4_28 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_28==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_28==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_28==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_28==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_28==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_28==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_28==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_28==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_28==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_28==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_28==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_28==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_28==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_28==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_28==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_28==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_28==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_28==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_28==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_28==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_28==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_28==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_28==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_28==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_28==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_28==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_28==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_28==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_28==EOF||LA4_28==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_28);
                        if ( s>=0 ) return s;
                        break;
                    case 21 : 
                        int LA4_27 = input.LA(1);

                         
                        int index4_27 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_27==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_27==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_27==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_27==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_27==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_27==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_27==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_27==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_27==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_27==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_27==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_27==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_27==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_27==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_27==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_27==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_27==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_27==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_27==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_27==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_27==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_27==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_27==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_27==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_27==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_27==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_27==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_27==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_27==EOF||LA4_27==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_27);
                        if ( s>=0 ) return s;
                        break;
                    case 22 : 
                        int LA4_26 = input.LA(1);

                         
                        int index4_26 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_26==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_26==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_26==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_26==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_26==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_26==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_26==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_26==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_26==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_26==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_26==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_26==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_26==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_26==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_26==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_26==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_26==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_26==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_26==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_26==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_26==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_26==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_26==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_26==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_26==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_26==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_26==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_26==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_26==EOF||LA4_26==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_26);
                        if ( s>=0 ) return s;
                        break;
                    case 23 : 
                        int LA4_57 = input.LA(1);

                         
                        int index4_57 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_57==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_57==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_57==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_57==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_57==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_57==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_57==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_57==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_57==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_57==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_57==EOF||LA4_57==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_57);
                        if ( s>=0 ) return s;
                        break;
                    case 24 : 
                        int LA4_8 = input.LA(1);

                         
                        int index4_8 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_8==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_8==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_8==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_8==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_8==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_8==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_8==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_8==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_8==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_8==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_8==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_8==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_8==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_8==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_8==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_8==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_8==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_8==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_8==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_8==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_8==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_8==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_8==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_8==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_8==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_8==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_8==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_8==EOF||LA4_8==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_8==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_8);
                        if ( s>=0 ) return s;
                        break;
                    case 25 : 
                        int LA4_138 = input.LA(1);

                         
                        int index4_138 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_138==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_138==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_138==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_138==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_138==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_138==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_138==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_138==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_138==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_138==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_138==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_138==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_138==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_138==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_138==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_138==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_138==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_138==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_138==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_138==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_138==EOF||LA4_138==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_138);
                        if ( s>=0 ) return s;
                        break;
                    case 26 : 
                        int LA4_9 = input.LA(1);

                         
                        int index4_9 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_9==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_9==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_9==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_9==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_9==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_9==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_9==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_9==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_9==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_9==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_9==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_9==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_9==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_9==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_9==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_9==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_9==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_9==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_9==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_9==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_9==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_9==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_9==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_9==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_9==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_9==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_9==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_9==EOF||LA4_9==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_9==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_9);
                        if ( s>=0 ) return s;
                        break;
                    case 27 : 
                        int LA4_137 = input.LA(1);

                         
                        int index4_137 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_137==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_137==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_137==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_137==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_137==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_137==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_137==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_137==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_137==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_137==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_137==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_137==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_137==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_137==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_137==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_137==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_137==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_137==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_137==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_137==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_137==EOF||LA4_137==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_137);
                        if ( s>=0 ) return s;
                        break;
                    case 28 : 
                        int LA4_6 = input.LA(1);

                         
                        int index4_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_6==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_6==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_6==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_6==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_6==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_6==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_6==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_6==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_6==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_6==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_6==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_6==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_6==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_6==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_6==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_6==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_6==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_6==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_6==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_6==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_6==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_6==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_6==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_6==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_6==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_6==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_6==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_6==EOF||LA4_6==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_6==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_6);
                        if ( s>=0 ) return s;
                        break;
                    case 29 : 
                        int LA4_42 = input.LA(1);

                         
                        int index4_42 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_42==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_42==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_42==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_42==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_42==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_42==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_42==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_42==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_42==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_42==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_42==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_42==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_42==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_42==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_42==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_42==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_42==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_42==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_42==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_42==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_42==EOF||LA4_42==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_42);
                        if ( s>=0 ) return s;
                        break;
                    case 30 : 
                        int LA4_70 = input.LA(1);

                         
                        int index4_70 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_70==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_70==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_70==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_70==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_70==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_70==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_70==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_70==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_70==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_70==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_70==EOF||LA4_70==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_70);
                        if ( s>=0 ) return s;
                        break;
                    case 31 : 
                        int LA4_7 = input.LA(1);

                         
                        int index4_7 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_7==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_7==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_7==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_7==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_7==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_7==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_7==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_7==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_7==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_7==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_7==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_7==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_7==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_7==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_7==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_7==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_7==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_7==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_7==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_7==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_7==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_7==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_7==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_7==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_7==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_7==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_7==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_7==EOF||LA4_7==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_7==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_7);
                        if ( s>=0 ) return s;
                        break;
                    case 32 : 
                        int LA4_41 = input.LA(1);

                         
                        int index4_41 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_41==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_41==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_41==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_41==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_41==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_41==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_41==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_41==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_41==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_41==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_41==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_41==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_41==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_41==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_41==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_41==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_41==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_41==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_41==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_41==EOF||LA4_41==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_41==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_41);
                        if ( s>=0 ) return s;
                        break;
                    case 33 : 
                        int LA4_88 = input.LA(1);

                         
                        int index4_88 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_88==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_88==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_88==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_88==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_88==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_88==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_88==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_88==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_88==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_88==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_88==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_88==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_88==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_88==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_88==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_88==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_88==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_88==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_88==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_88==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_88==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_88==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_88==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_88==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_88==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_88==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_88==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_88==EOF||LA4_88==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_88);
                        if ( s>=0 ) return s;
                        break;
                    case 34 : 
                        int LA4_44 = input.LA(1);

                         
                        int index4_44 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_44==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_44==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_44==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_44==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_44==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_44==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_44==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_44==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_44==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_44==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_44==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_44==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_44==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_44==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_44==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_44==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_44==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_44==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_44==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_44==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_44==EOF||LA4_44==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_44);
                        if ( s>=0 ) return s;
                        break;
                    case 35 : 
                        int LA4_4 = input.LA(1);

                         
                        int index4_4 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_4==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_4==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_4==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_4==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_4==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_4==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_4==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_4==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_4==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_4==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_4==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_4==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_4==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_4==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_4==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_4==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_4==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_4==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_4==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_4==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_4==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_4==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_4==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_4==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_4==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_4==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_4==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_4==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_4==EOF||LA4_4==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_4);
                        if ( s>=0 ) return s;
                        break;
                    case 36 : 
                        int LA4_43 = input.LA(1);

                         
                        int index4_43 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_43==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_43==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_43==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_43==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_43==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_43==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_43==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_43==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_43==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_43==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_43==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_43==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_43==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_43==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_43==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_43==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_43==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_43==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_43==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_43==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_43==EOF||LA4_43==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_43);
                        if ( s>=0 ) return s;
                        break;
                    case 37 : 
                        int LA4_5 = input.LA(1);

                         
                        int index4_5 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_5==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_5==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_5==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_5==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_5==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_5==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_5==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_5==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_5==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_5==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_5==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_5==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_5==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_5==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_5==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_5==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_5==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_5==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_5==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_5==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_5==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_5==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_5==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_5==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_5==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_5==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_5==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_5==EOF||LA4_5==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_5==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_5);
                        if ( s>=0 ) return s;
                        break;
                    case 38 : 
                        int LA4_46 = input.LA(1);

                         
                        int index4_46 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_46==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_46==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_46==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_46==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_46==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_46==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_46==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_46==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_46==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_46==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_46==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_46==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_46==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_46==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_46==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_46==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_46==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_46==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_46==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_46==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_46==EOF||LA4_46==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_46);
                        if ( s>=0 ) return s;
                        break;
                    case 39 : 
                        int LA4_2 = input.LA(1);

                         
                        int index4_2 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_2==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_2==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_2==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_2==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_2==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_2==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_2==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_2==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_2==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_2==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_2==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_2==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_2==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_2==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_2==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_2==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_2==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_2==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_2==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_2==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_2==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_2==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_2==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_2==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_2==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_2==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_2==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_2==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_2==EOF||LA4_2==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_2);
                        if ( s>=0 ) return s;
                        break;
                    case 40 : 
                        int LA4_45 = input.LA(1);

                         
                        int index4_45 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_45==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_45==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_45==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_45==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_45==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_45==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_45==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_45==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_45==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_45==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_45==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_45==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_45==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_45==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_45==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_45==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_45==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_45==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_45==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_45==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_45==EOF||LA4_45==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_45);
                        if ( s>=0 ) return s;
                        break;
                    case 41 : 
                        int LA4_3 = input.LA(1);

                         
                        int index4_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_3==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_3==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_3==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_3==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_3==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_3==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_3==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_3==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_3==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_3==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_3==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_3==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_3==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_3==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_3==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_3==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_3==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_3==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_3==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_3==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_3==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_3==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_3==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_3==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_3==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_3==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_3==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_3==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_3==EOF||LA4_3==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_3);
                        if ( s>=0 ) return s;
                        break;
                    case 42 : 
                        int LA4_16 = input.LA(1);

                         
                        int index4_16 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_16==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_16==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_16==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_16==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_16==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_16==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_16==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_16==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_16==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_16==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_16==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_16==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_16==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_16==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_16==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_16==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_16==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_16==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_16==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_16==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_16==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_16==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_16==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_16==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_16==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_16==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_16==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_16==EOF||LA4_16==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_16==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_16);
                        if ( s>=0 ) return s;
                        break;
                    case 43 : 
                        int LA4_17 = input.LA(1);

                         
                        int index4_17 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_17==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_17==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_17==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_17==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_17==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_17==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_17==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_17==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_17==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_17==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_17==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_17==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_17==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_17==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_17==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_17==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_17==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_17==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_17==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_17==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_17==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_17==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_17==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_17==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_17==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_17==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_17==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_17==EOF||LA4_17==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_17==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_17);
                        if ( s>=0 ) return s;
                        break;
                    case 44 : 
                        int LA4_14 = input.LA(1);

                         
                        int index4_14 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_14==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_14==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_14==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_14==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_14==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_14==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_14==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_14==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_14==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_14==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_14==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_14==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_14==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_14==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_14==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_14==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_14==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_14==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_14==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_14==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_14==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_14==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_14==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_14==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_14==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_14==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_14==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_14==EOF||LA4_14==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_14==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_14);
                        if ( s>=0 ) return s;
                        break;
                    case 45 : 
                        int LA4_132 = input.LA(1);

                         
                        int index4_132 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_132==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_132==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_132==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_132==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_132==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_132==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_132==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_132==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_132==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_132==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_132==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_132==EOF||LA4_132==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_132==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_132==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_132==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_132==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_132==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_132==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_132==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_132==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_132==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_132);
                        if ( s>=0 ) return s;
                        break;
                    case 46 : 
                        int LA4_15 = input.LA(1);

                         
                        int index4_15 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_15==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_15==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_15==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_15==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_15==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_15==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_15==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_15==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_15==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_15==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_15==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_15==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_15==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_15==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_15==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_15==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_15==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_15==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_15==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_15==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_15==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_15==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_15==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_15==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_15==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_15==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_15==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_15==EOF||LA4_15==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_15==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_15);
                        if ( s>=0 ) return s;
                        break;
                    case 47 : 
                        int LA4_131 = input.LA(1);

                         
                        int index4_131 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_131==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_131==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_131==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_131==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_131==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_131==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_131==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_131==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_131==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_131==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_131==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_131==EOF||LA4_131==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_131==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_131==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_131==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_131==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_131==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_131==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_131==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_131==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_131==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_131);
                        if ( s>=0 ) return s;
                        break;
                    case 48 : 
                        int LA4_134 = input.LA(1);

                         
                        int index4_134 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_134==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_134==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_134==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_134==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_134==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_134==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_134==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_134==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_134==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_134==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_134==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_134==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_134==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_134==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_134==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_134==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_134==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_134==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_134==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_134==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_134==EOF||LA4_134==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_134);
                        if ( s>=0 ) return s;
                        break;
                    case 49 : 
                        int LA4_12 = input.LA(1);

                         
                        int index4_12 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_12==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_12==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_12==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_12==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_12==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_12==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_12==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_12==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_12==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_12==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_12==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_12==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_12==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_12==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_12==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_12==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_12==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_12==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_12==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_12==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_12==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_12==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_12==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_12==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_12==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_12==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_12==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_12==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_12==EOF||LA4_12==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_12);
                        if ( s>=0 ) return s;
                        break;
                    case 50 : 
                        int LA4_133 = input.LA(1);

                         
                        int index4_133 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_133==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_133==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_133==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_133==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_133==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_133==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_133==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_133==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_133==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_133==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_133==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_133==EOF||LA4_133==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_133==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_133==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_133==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_133==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_133==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_133==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_133==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_133==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_133==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_133);
                        if ( s>=0 ) return s;
                        break;
                    case 51 : 
                        int LA4_13 = input.LA(1);

                         
                        int index4_13 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_13==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_13==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_13==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_13==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_13==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_13==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_13==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_13==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_13==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_13==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_13==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_13==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_13==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_13==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_13==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_13==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_13==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_13==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_13==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_13==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_13==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_13==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_13==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_13==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_13==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_13==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_13==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_13==EOF||LA4_13==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_13==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_13);
                        if ( s>=0 ) return s;
                        break;
                    case 52 : 
                        int LA4_136 = input.LA(1);

                         
                        int index4_136 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_136==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_136==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_136==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_136==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_136==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_136==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_136==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_136==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_136==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_136==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_136==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_136==EOF||LA4_136==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_136==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_136==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_136==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_136==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_136==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_136==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_136==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_136==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_136==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_136);
                        if ( s>=0 ) return s;
                        break;
                    case 53 : 
                        int LA4_10 = input.LA(1);

                         
                        int index4_10 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_10==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_10==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_10==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_10==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_10==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_10==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_10==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_10==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_10==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_10==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_10==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_10==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_10==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_10==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_10==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_10==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_10==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_10==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_10==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_10==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_10==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_10==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_10==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_10==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_10==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_10==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_10==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_10==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_10==EOF||LA4_10==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_10);
                        if ( s>=0 ) return s;
                        break;
                    case 54 : 
                        int LA4_56 = input.LA(1);

                         
                        int index4_56 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_56==LEFT_PAREN) ) {s = 84;}

                        else if ( (LA4_56==EOF||LA4_56==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_56==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_56==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_56==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_56==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_56==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_56==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_56==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_56==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_56==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_56==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_56);
                        if ( s>=0 ) return s;
                        break;
                    case 55 : 
                        int LA4_135 = input.LA(1);

                         
                        int index4_135 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_135==PRECISION_KEYWORD) ) {s = 150;}

                        else if ( (LA4_135==LEFT_PAREN) ) {s = 151;}

                        else if ( (LA4_135==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_135==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_135==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_135==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_135==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_135==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_135==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_135==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_135==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_135==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_135==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_135==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_135==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_135==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_135==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_135==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_135==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_135==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_135==EOF||LA4_135==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_135);
                        if ( s>=0 ) return s;
                        break;
                    case 56 : 
                        int LA4_11 = input.LA(1);

                         
                        int index4_11 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_11==LEFT_PAREN) ) {s = 32;}

                        else if ( (LA4_11==BIT_KEYWORD) ) {s = 33;}

                        else if ( (LA4_11==CHARACTER_KEYWORD) ) {s = 34;}

                        else if ( (LA4_11==GRAPHIC_KEYWORD) ) {s = 35;}

                        else if ( (LA4_11==WIDECHAR_KEYWORD) ) {s = 36;}

                        else if ( (LA4_11==NONVARYING_KEYWORD) ) {s = 37;}

                        else if ( (LA4_11==VARYING_KEYWORD) ) {s = 38;}

                        else if ( (LA4_11==VARYINGZ_KEYWORD) ) {s = 39;}

                        else if ( (LA4_11==PICTURE_KEYWORD) ) {s = 40;}

                        else if ( (LA4_11==FLOAT_KEYWORD) ) {s = 41;}

                        else if ( (LA4_11==FIXED_KEYWORD) ) {s = 42;}

                        else if ( (LA4_11==BINARY_KEYWORD) ) {s = 43;}

                        else if ( (LA4_11==DECIMAL_KEYWORD) ) {s = 44;}

                        else if ( (LA4_11==SIGNED_KEYWORD) ) {s = 45;}

                        else if ( (LA4_11==UNSIGNED_KEYWORD) ) {s = 46;}

                        else if ( (LA4_11==REAL_KEYWORD) ) {s = 47;}

                        else if ( (LA4_11==COMPLEX_KEYWORD) ) {s = 48;}

                        else if ( (LA4_11==PRECISION_KEYWORD) ) {s = 49;}

                        else if ( (LA4_11==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_11==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_11==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_11==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_11==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_11==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_11==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_11==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_11==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_11==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_11==EOF||LA4_11==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_11);
                        if ( s>=0 ) return s;
                        break;
                    case 57 : 
                        int LA4_111 = input.LA(1);

                         
                        int index4_111 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_111==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_111==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_111==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_111==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_111==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_111==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_111==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_111==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_111==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_111==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_111==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_111==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_111==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_111==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_111==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_111==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_111==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_111==EOF||LA4_111==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_111==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_111);
                        if ( s>=0 ) return s;
                        break;
                    case 58 : 
                        int LA4_51 = input.LA(1);

                         
                        int index4_51 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_51==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_51==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_51==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_51==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_51==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_51==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_51==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_51==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_51==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_51==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_51==EOF||LA4_51==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_51);
                        if ( s>=0 ) return s;
                        break;
                    case 59 : 
                        int LA4_52 = input.LA(1);

                         
                        int index4_52 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_52==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_52==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_52==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_52==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_52==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_52==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_52==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_52==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_52==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_52==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_52==EOF||LA4_52==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_52);
                        if ( s>=0 ) return s;
                        break;
                    case 60 : 
                        int LA4_65 = input.LA(1);

                         
                        int index4_65 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_65==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_65==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_65==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_65==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_65==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_65==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_65==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_65==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_65==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_65==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_65==EOF||LA4_65==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_65);
                        if ( s>=0 ) return s;
                        break;
                    case 61 : 
                        int LA4_108 = input.LA(1);

                         
                        int index4_108 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_108==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_108==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_108==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_108==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_108==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_108==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_108==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_108==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_108==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_108==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_108==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_108==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_108==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_108==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_108==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_108==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_108==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_108==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_108==EOF||LA4_108==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_108);
                        if ( s>=0 ) return s;
                        break;
                    case 62 : 
                        int LA4_64 = input.LA(1);

                         
                        int index4_64 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_64==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_64==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_64==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_64==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_64==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_64==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_64==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_64==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_64==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_64==EOF||LA4_64==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_64==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_64);
                        if ( s>=0 ) return s;
                        break;
                    case 63 : 
                        int LA4_63 = input.LA(1);

                         
                        int index4_63 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_63==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_63==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_63==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_63==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_63==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_63==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_63==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_63==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_63==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_63==EOF||LA4_63==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_63==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_63);
                        if ( s>=0 ) return s;
                        break;
                    case 64 : 
                        int LA4_171 = input.LA(1);

                         
                        int index4_171 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_171==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_171==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_171==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_171==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_171==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_171==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_171==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_171==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_171==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_171==EOF||LA4_171==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_171==FLOAT_KEYWORD) ) {s = 131;}

                        else if ( (LA4_171==FIXED_KEYWORD) ) {s = 132;}

                        else if ( (LA4_171==BINARY_KEYWORD) ) {s = 133;}

                        else if ( (LA4_171==DECIMAL_KEYWORD) ) {s = 134;}

                        else if ( (LA4_171==SIGNED_KEYWORD) ) {s = 135;}

                        else if ( (LA4_171==UNSIGNED_KEYWORD) ) {s = 136;}

                        else if ( (LA4_171==REAL_KEYWORD) ) {s = 137;}

                        else if ( (LA4_171==COMPLEX_KEYWORD) ) {s = 138;}

                        else if ( (LA4_171==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_171);
                        if ( s>=0 ) return s;
                        break;
                    case 65 : 
                        int LA4_47 = input.LA(1);

                         
                        int index4_47 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_47==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_47==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_47==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_47==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_47==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_47==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_47==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_47==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_47==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_47==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_47==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_47==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_47==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_47==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_47==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_47==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_47==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_47==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_47==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_47==EOF||LA4_47==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_47==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_47);
                        if ( s>=0 ) return s;
                        break;
                    case 66 : 
                        int LA4_58 = input.LA(1);

                         
                        int index4_58 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_58==EOF||LA4_58==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_58==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_58==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_58==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_58==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_58==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_58==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_58==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_58==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_58==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_58==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_58);
                        if ( s>=0 ) return s;
                        break;
                    case 67 : 
                        int LA4_48 = input.LA(1);

                         
                        int index4_48 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_48==PRECISION_KEYWORD) ) {s = 71;}

                        else if ( (LA4_48==LEFT_PAREN) ) {s = 72;}

                        else if ( (LA4_48==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_48==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_48==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_48==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_48==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_48==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_48==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_48==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_48==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_48==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_48==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_48==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_48==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_48==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_48==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_48==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_48==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_48==EOF||LA4_48==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_48==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_48);
                        if ( s>=0 ) return s;
                        break;
                    case 68 : 
                        int LA4_66 = input.LA(1);

                         
                        int index4_66 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_66==LEFT_PAREN) ) {s = 90;}

                        else if ( (LA4_66==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_66==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_66==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_66==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_66==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_66==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_66==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_66==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_66==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_66==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_66==EOF||LA4_66==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_66);
                        if ( s>=0 ) return s;
                        break;
                    case 69 : 
                        int LA4_67 = input.LA(1);

                         
                        int index4_67 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_67==LEFT_PAREN) ) {s = 90;}

                        else if ( (LA4_67==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_67==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_67==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_67==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_67==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_67==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_67==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_67==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_67==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_67==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_67==EOF||LA4_67==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_67);
                        if ( s>=0 ) return s;
                        break;
                    case 70 : 
                        int LA4_117 = input.LA(1);

                         
                        int index4_117 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_117==EOF||LA4_117==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_117==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_117==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_117==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_117==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_117==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_117==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_117==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_117==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_117==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_117==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_117);
                        if ( s>=0 ) return s;
                        break;
                    case 71 : 
                        int LA4_68 = input.LA(1);

                         
                        int index4_68 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_68==LEFT_PAREN) ) {s = 90;}

                        else if ( (LA4_68==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_68==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_68==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_68==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_68==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_68==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_68==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_68==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_68==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_68==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_68==EOF||LA4_68==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_68);
                        if ( s>=0 ) return s;
                        break;
                    case 72 : 
                        int LA4_69 = input.LA(1);

                         
                        int index4_69 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_69==LEFT_PAREN) ) {s = 90;}

                        else if ( (LA4_69==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_69==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_69==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_69==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_69==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_69==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_69==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_69==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_69==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_69==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_69==EOF||LA4_69==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_69);
                        if ( s>=0 ) return s;
                        break;
                    case 73 : 
                        int LA4_75 = input.LA(1);

                         
                        int index4_75 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_75==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_75==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_75==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_75==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_75==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_75==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_75==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_75==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_75==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_75==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_75==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_75==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_75==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_75==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_75==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_75==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_75==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_75==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_75==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_75==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_75==EOF||LA4_75==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_75);
                        if ( s>=0 ) return s;
                        break;
                    case 74 : 
                        int LA4_74 = input.LA(1);

                         
                        int index4_74 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_74==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_74==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_74==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_74==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_74==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_74==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_74==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_74==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_74==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_74==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_74==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_74==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_74==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_74==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_74==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_74==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_74==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_74==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_74==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_74==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_74==EOF||LA4_74==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_74);
                        if ( s>=0 ) return s;
                        break;
                    case 75 : 
                        int LA4_73 = input.LA(1);

                         
                        int index4_73 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_73==PRECISION_KEYWORD) ) {s = 92;}

                        else if ( (LA4_73==LEFT_PAREN) ) {s = 93;}

                        else if ( (LA4_73==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_73==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_73==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_73==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_73==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_73==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_73==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_73==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_73==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_73==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_73==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_73==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_73==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_73==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_73==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_73==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_73==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_73==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_73==EOF||LA4_73==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_73);
                        if ( s>=0 ) return s;
                        break;
                    case 76 : 
                        int LA4_116 = input.LA(1);

                         
                        int index4_116 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_116==EOF||LA4_116==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_116==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_116==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_116==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_116==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_116==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_116==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_116==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_116==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_116==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_116==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_116);
                        if ( s>=0 ) return s;
                        break;
                    case 77 : 
                        int LA4_36 = input.LA(1);

                         
                        int index4_36 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_36==LEFT_PAREN) ) {s = 62;}

                        else if ( (LA4_36==NONVARYING_KEYWORD) ) {s = 63;}

                        else if ( (LA4_36==VARYING_KEYWORD) ) {s = 64;}

                        else if ( (LA4_36==VARYINGZ_KEYWORD) ) {s = 65;}

                        else if ( (LA4_36==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_36==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_36==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_36==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_36==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_36==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_36==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_36==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_36==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_36==EOF||LA4_36==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_36==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_36);
                        if ( s>=0 ) return s;
                        break;
                    case 78 : 
                        int LA4_35 = input.LA(1);

                         
                        int index4_35 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_35==LEFT_PAREN) ) {s = 62;}

                        else if ( (LA4_35==NONVARYING_KEYWORD) ) {s = 63;}

                        else if ( (LA4_35==VARYING_KEYWORD) ) {s = 64;}

                        else if ( (LA4_35==VARYINGZ_KEYWORD) ) {s = 65;}

                        else if ( (LA4_35==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_35==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_35==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_35==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_35==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_35==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_35==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_35==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_35==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_35==EOF||LA4_35==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_35==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_35);
                        if ( s>=0 ) return s;
                        break;
                    case 79 : 
                        int LA4_105 = input.LA(1);

                         
                        int index4_105 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_105==NONVARYING_KEYWORD) ) {s = 63;}

                        else if ( (LA4_105==VARYING_KEYWORD) ) {s = 64;}

                        else if ( (LA4_105==VARYINGZ_KEYWORD) ) {s = 65;}

                        else if ( (LA4_105==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_105==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_105==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_105==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_105==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_105==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_105==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_105==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_105==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_105==EOF||LA4_105==SEMICOLON) ) {s = 60;}

                        else if ( (LA4_105==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                         
                        input.seek(index4_105);
                        if ( s>=0 ) return s;
                        break;
                    case 80 : 
                        int LA4_34 = input.LA(1);

                         
                        int index4_34 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_34==LEFT_PAREN) ) {s = 62;}

                        else if ( (LA4_34==NONVARYING_KEYWORD) ) {s = 63;}

                        else if ( (LA4_34==VARYING_KEYWORD) ) {s = 64;}

                        else if ( (LA4_34==VARYINGZ_KEYWORD) ) {s = 65;}

                        else if ( (LA4_34==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_34==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_34==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_34==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_34==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_34==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_34==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_34==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_34==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_34==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_34==EOF||LA4_34==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_34);
                        if ( s>=0 ) return s;
                        break;
                    case 81 : 
                        int LA4_128 = input.LA(1);

                         
                        int index4_128 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_128==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_128==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_128==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_128==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_128==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_128==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_128==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_128==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_128==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_128==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_128==FLOAT_KEYWORD) ) {s = 73;}

                        else if ( (LA4_128==FIXED_KEYWORD) ) {s = 74;}

                        else if ( (LA4_128==BINARY_KEYWORD) ) {s = 75;}

                        else if ( (LA4_128==DECIMAL_KEYWORD) ) {s = 76;}

                        else if ( (LA4_128==SIGNED_KEYWORD) ) {s = 77;}

                        else if ( (LA4_128==UNSIGNED_KEYWORD) ) {s = 78;}

                        else if ( (LA4_128==REAL_KEYWORD) ) {s = 79;}

                        else if ( (LA4_128==COMPLEX_KEYWORD) ) {s = 80;}

                        else if ( (LA4_128==EOF||LA4_128==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_128);
                        if ( s>=0 ) return s;
                        break;
                    case 82 : 
                        int LA4_33 = input.LA(1);

                         
                        int index4_33 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_33==LEFT_PAREN) ) {s = 62;}

                        else if ( (LA4_33==NONVARYING_KEYWORD) ) {s = 63;}

                        else if ( (LA4_33==VARYING_KEYWORD) ) {s = 64;}

                        else if ( (LA4_33==VARYINGZ_KEYWORD) ) {s = 65;}

                        else if ( (LA4_33==DIMENSION_KEYWORD) ) {s = 50;}

                        else if ( (LA4_33==ALIGNED_KEYWORD) ) {s = 51;}

                        else if ( (LA4_33==UNALIGNED_KEYWORD) ) {s = 52;}

                        else if ( (LA4_33==INITIAL_KEYWORD) ) {s = 53;}

                        else if ( (LA4_33==AUTOMATIC_KEYWORD) ) {s = 54;}

                        else if ( (LA4_33==STATIC_KEYWORD) ) {s = 55;}

                        else if ( (LA4_33==BASED_KEYWORD) ) {s = 56;}

                        else if ( (LA4_33==CONTROLLED_KEYWORD) ) {s = 57;}

                        else if ( (LA4_33==UNION_KEYWORD) ) {s = 58;}

                        else if ( (LA4_33==COMMA) && (synpred2_PLIStructureParser())) {s = 59;}

                        else if ( (LA4_33==EOF||LA4_33==SEMICOLON) ) {s = 60;}

                         
                        input.seek(index4_33);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 4, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

    public static final BitSet FOLLOW_statement_in_pl1code278 = new BitSet(new long[]{0xFFFFFFFFFFFFFFF2L,0x000000000007FFFFL});
    public static final BitSet FOLLOW_declare_in_statement301 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_non_declare_in_statement309 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DECLARE_KEYWORD_in_declare329 = new BitSet(new long[]{0xFFFF200000000000L,0x0000000000003FFFL});
    public static final BitSet FOLLOW_data_items_in_declare332 = new BitSet(new long[]{0x0000400000000000L});
    public static final BitSet FOLLOW_SEMICOLON_in_declare334 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SEMICOLON_in_non_declare365 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_data_item_with_children_in_data_items393 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_data_item_in_data_items405 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_data_item_in_data_item_with_children435 = new BitSet(new long[]{0x0000800000000000L});
    public static final BitSet FOLLOW_COMMA_in_data_item_with_children437 = new BitSet(new long[]{0xFFFF200000000000L,0x0000000000003FFFL});
    public static final BitSet FOLLOW_data_items_in_data_item_with_children439 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_level_in_data_item478 = new BitSet(new long[]{0xFFFF200000000000L,0x0000000000003FFFL});
    public static final BitSet FOLLOW_data_item_name_in_data_item481 = new BitSet(new long[]{0xFFF8000000000002L,0x0000000000007FFEL});
    public static final BitSet FOLLOW_implicit_dimension_attribute_in_data_item483 = new BitSet(new long[]{0xFFF8000000000002L,0x0000000000003FFEL});
    public static final BitSet FOLLOW_elementary_data_item_attribute_in_data_item486 = new BitSet(new long[]{0x0000000000000002L,0x0000000000003FE0L});
    public static final BitSet FOLLOW_misc_attribute_in_data_item489 = new BitSet(new long[]{0x0000000000000002L,0x0000000000003FE0L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_level539 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DATA_ITEM_NAME_in_data_item_name565 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ASTERISK_in_data_item_name580 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DECLARE_KEYWORD_in_data_item_name595 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_REAL_KEYWORD_in_data_item_name610 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_COMPLEX_KEYWORD_in_data_item_name625 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_FIXED_KEYWORD_in_data_item_name640 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_FLOAT_KEYWORD_in_data_item_name655 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_PRECISION_KEYWORD_in_data_item_name670 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DECIMAL_KEYWORD_in_data_item_name685 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BINARY_KEYWORD_in_data_item_name700 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BIT_KEYWORD_in_data_item_name715 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SIGNED_KEYWORD_in_data_item_name731 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNSIGNED_KEYWORD_in_data_item_name746 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_CHARACTER_KEYWORD_in_data_item_name761 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_WIDECHAR_KEYWORD_in_data_item_name777 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_GRAPHIC_KEYWORD_in_data_item_name793 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_REFER_KEYWORD_in_data_item_name809 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NONVARYING_KEYWORD_in_data_item_name825 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_VARYING_KEYWORD_in_data_item_name841 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_VARYINGZ_KEYWORD_in_data_item_name857 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_PICTURE_KEYWORD_in_data_item_name873 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DIMENSION_KEYWORD_in_data_item_name889 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ALIGNED_KEYWORD_in_data_item_name904 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNALIGNED_KEYWORD_in_data_item_name919 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_INITIAL_KEYWORD_in_data_item_name934 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_AUTOMATIC_KEYWORD_in_data_item_name949 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STATIC_KEYWORD_in_data_item_name964 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BASED_KEYWORD_in_data_item_name979 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_CONTROLLED_KEYWORD_in_data_item_name994 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNION_KEYWORD_in_data_item_name1009 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_string_attribute_in_elementary_data_item_attribute1033 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_picture_attribute_in_elementary_data_item_attribute1041 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_arithmetic_attribute_in_elementary_data_item_attribute1049 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_dimension_attribute_in_misc_attribute1066 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_alignment_attribute_in_misc_attribute1075 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_initial_attribute_in_misc_attribute1083 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_storage_attribute_in_misc_attribute1091 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_union_attribute_in_misc_attribute1099 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_string_keyword_in_string_attribute1118 = new BitSet(new long[]{0xE400000000000002L,0x000000000000400EL});
    public static final BitSet FOLLOW_string_length_specification_in_string_attribute1120 = new BitSet(new long[]{0xE400000000000002L,0x000000000000000EL});
    public static final BitSet FOLLOW_varying_attribute_in_string_attribute1123 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_varying_attribute_in_string_attribute1151 = new BitSet(new long[]{0xE400000000000000L});
    public static final BitSet FOLLOW_string_keyword_in_string_attribute1153 = new BitSet(new long[]{0x0000000000000002L,0x0000000000004000L});
    public static final BitSet FOLLOW_string_length_specification_in_string_attribute1155 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BIT_KEYWORD_in_string_keyword1192 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_CHARACTER_KEYWORD_in_string_keyword1205 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_GRAPHIC_KEYWORD_in_string_keyword1218 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_WIDECHAR_KEYWORD_in_string_keyword1231 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_string_length_specification1253 = new BitSet(new long[]{0x0001000000000000L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_string_length_specification1255 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008001L});
    public static final BitSet FOLLOW_refer_specification_in_string_length_specification1257 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_string_length_specification1260 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_REFER_KEYWORD_in_refer_specification1290 = new BitSet(new long[]{0x0000000000000000L,0x0000000000004000L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_refer_specification1292 = new BitSet(new long[]{0x0002000000000000L});
    public static final BitSet FOLLOW_DATA_ITEM_NAME_in_refer_specification1294 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_refer_specification1296 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NONVARYING_KEYWORD_in_varying_attribute1327 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_VARYING_KEYWORD_in_varying_attribute1343 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_VARYINGZ_KEYWORD_in_varying_attribute1359 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_PICTURE_KEYWORD_in_picture_attribute1385 = new BitSet(new long[]{0x0000000000000000L,0x0000000000010000L});
    public static final BitSet FOLLOW_picture_value_in_picture_attribute1388 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STRING_LITERAL_in_picture_value1404 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_arithmetic_keyword_in_arithmetic_attribute1430 = new BitSet(new long[]{0x1BF8000000000002L,0x0000000000004000L});
    public static final BitSet FOLLOW_precision_specification_in_arithmetic_attribute1432 = new BitSet(new long[]{0x1B78000000000002L});
    public static final BitSet FOLLOW_other_arithmetic_attribute_in_arithmetic_attribute1435 = new BitSet(new long[]{0x1B78000000000002L});
    public static final BitSet FOLLOW_PRECISION_KEYWORD_in_arithmetic_attribute1462 = new BitSet(new long[]{0x0080000000000000L,0x0000000000004000L});
    public static final BitSet FOLLOW_implicit_precision_specification_in_arithmetic_attribute1464 = new BitSet(new long[]{0x1B78000000000002L});
    public static final BitSet FOLLOW_other_arithmetic_attribute_in_arithmetic_attribute1466 = new BitSet(new long[]{0x1B78000000000002L});
    public static final BitSet FOLLOW_FLOAT_KEYWORD_in_arithmetic_keyword1500 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_FIXED_KEYWORD_in_arithmetic_keyword1517 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BINARY_KEYWORD_in_arithmetic_keyword1534 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DECIMAL_KEYWORD_in_arithmetic_keyword1551 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SIGNED_KEYWORD_in_arithmetic_keyword1568 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNSIGNED_KEYWORD_in_arithmetic_keyword1585 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_REAL_KEYWORD_in_arithmetic_keyword1602 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_COMPLEX_KEYWORD_in_arithmetic_keyword1619 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_arithmetic_keyword_in_other_arithmetic_attribute1644 = new BitSet(new long[]{0x0080000000000002L,0x0000000000004000L});
    public static final BitSet FOLLOW_precision_specification_in_other_arithmetic_attribute1646 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_PRECISION_KEYWORD_in_precision_specification1663 = new BitSet(new long[]{0x0080000000000000L,0x0000000000004000L});
    public static final BitSet FOLLOW_implicit_precision_specification_in_precision_specification1666 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_implicit_precision_specification1691 = new BitSet(new long[]{0x0001000000000000L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_implicit_precision_specification1693 = new BitSet(new long[]{0x0000800000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_scaling_factor_in_implicit_precision_specification1695 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_implicit_precision_specification1698 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_COMMA_in_scaling_factor1728 = new BitSet(new long[]{0x0001000000000000L,0x0000000000020000L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_scaling_factor1733 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SIGNED_INTEGER_in_scaling_factor1739 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SIGNED_KEYWORD_in_sign_sequence1771 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNSIGNED_KEYWORD_in_sign_sequence1777 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DIMENSION_KEYWORD_in_dimension_attribute1808 = new BitSet(new long[]{0x0000000000000000L,0x0000000000004000L});
    public static final BitSet FOLLOW_implicit_dimension_attribute_in_dimension_attribute1811 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_implicit_dimension_attribute1831 = new BitSet(new long[]{0x0001000000000000L});
    public static final BitSet FOLLOW_bound_attribute_in_implicit_dimension_attribute1833 = new BitSet(new long[]{0x0000800000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_COMMA_in_implicit_dimension_attribute1836 = new BitSet(new long[]{0x0001000000000000L});
    public static final BitSet FOLLOW_bound_attribute_in_implicit_dimension_attribute1838 = new BitSet(new long[]{0x0000800000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_implicit_dimension_attribute1842 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_double_bound_expression_in_bound_attribute1874 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_single_bound_expression_in_bound_attribute1889 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_double_bound_expression1916 = new BitSet(new long[]{0x0000000000000000L,0x0000000000040001L});
    public static final BitSet FOLLOW_refer_specification_in_double_bound_expression1920 = new BitSet(new long[]{0x0000000000000000L,0x0000000000040000L});
    public static final BitSet FOLLOW_COLUMN_in_double_bound_expression1923 = new BitSet(new long[]{0x0001000000000000L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_double_bound_expression1925 = new BitSet(new long[]{0x0000000000000002L,0x0000000000000001L});
    public static final BitSet FOLLOW_refer_specification_in_double_bound_expression1929 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_single_bound_expression1975 = new BitSet(new long[]{0x0000000000000002L,0x0000000000000001L});
    public static final BitSet FOLLOW_refer_specification_in_single_bound_expression1977 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ALIGNED_KEYWORD_in_alignment_attribute2014 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNALIGNED_KEYWORD_in_alignment_attribute2030 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_INITIAL_KEYWORD_in_initial_attribute2056 = new BitSet(new long[]{0x0000000000000000L,0x0000000000004000L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_initial_attribute2058 = new BitSet(new long[]{0x0001000000000800L,0x0000000000030000L});
    public static final BitSet FOLLOW_STRING_LITERAL_in_initial_attribute2063 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_SIGNED_INTEGER_in_initial_attribute2069 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_UNSIGNED_INTEGER_in_initial_attribute2075 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_FLOAT_in_initial_attribute2081 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_initial_attribute2084 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_AUTOMATIC_KEYWORD_in_storage_attribute2114 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STATIC_KEYWORD_in_storage_attribute2130 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BASED_KEYWORD_in_storage_attribute2146 = new BitSet(new long[]{0x0000000000000002L,0x0000000000004000L});
    public static final BitSet FOLLOW_LEFT_PAREN_in_storage_attribute2149 = new BitSet(new long[]{0x0002000000000000L});
    public static final BitSet FOLLOW_DATA_ITEM_NAME_in_storage_attribute2151 = new BitSet(new long[]{0x0000000000000000L,0x0000000000008000L});
    public static final BitSet FOLLOW_RIGHT_PAREN_in_storage_attribute2153 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_CONTROLLED_KEYWORD_in_storage_attribute2171 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_UNION_KEYWORD_in_union_attribute2197 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_declare_in_synpred1_PLIStructureParser298 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_data_item_with_children_in_synpred2_PLIStructureParser390 = new BitSet(new long[]{0x0000000000000002L});

}
