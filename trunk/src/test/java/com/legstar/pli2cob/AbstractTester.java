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
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;

import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.Tree;
import org.antlr.runtime.tree.TreeNodeStream;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.ANTLRNoCaseReaderStream;
import com.legstar.pli2cob.PLIStructureParser.pl1code_return;
import com.legstar.pli2cob.model.PLIDataItem;

import junit.framework.TestCase;

/**
 * Common test helper code.
 *
 */
public abstract class AbstractTester extends TestCase {

    /** Line separator (OS specific).*/
    public static final String LS = System.getProperty("line.separator");

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     */
    public CommonTokenStream lexify(final String source) {
        try {
            PLIStructureLexer lex = new PLIStructureLexerImpl(
                    new ANTLRNoCaseReaderStream(new StringReader(source)));
            CommonTokenStream tokens = new CommonTokenStream(lex);
            assertEquals(0, lex.getNumberOfSyntaxErrors());
            assertTrue(tokens != null);
            return tokens;
        } catch (IOException e) {
            _log.error("test failed", e);
            fail(e.toString());
        }
        return null;
    }

    /**
     * Apply Lexer + Parser to produce an abstract syntax tree from source. 
     * @param source the source code
     * @return an antlr abstract syntax tree
     */
    public CommonTree parse(final String source) {
        try {
            CommonTokenStream tokens = lexify(source);
            PLIStructureParser parser = new PLIStructureParserImpl(tokens);
            pl1code_return parserResult = parser.pl1code();
            assertEquals(0, parser.getNumberOfSyntaxErrors());
            assertTrue(parserResult != null);
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            e.printStackTrace();
            fail(e.toString());
        }
        return null;
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    public void parseAndCheck(final String source, final String expected) {
        CommonTree ast = parse(source);
        if (_log.isDebugEnabled()) {
            _log.debug(getGraph(ast).toString());
        }
        assertEquals(expected, (ast == null) ? "" : ast.toStringTree());
    }

    /**
     * Apply Lexer + Parser + Enhancer to produce an abstract syntax tree from source. 
     * @param source the source code
     * @return an antlr abstract syntax tree where nodes are normalized
     */
    public CommonTree parseAndEnhance(final String source) {
        try {
            CommonTree ast = parse(source);
            if (_log.isDebugEnabled()) {
                _log.debug(ast.toStringTree());
            }
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            PLIStructureEnhancer enhancer = new PLIStructureEnhancer(nodes);
            return enhancer.pl1code().tree;
        } catch (RecognitionException e) {
            e.printStackTrace();
            fail(e.toString());
        }
        return null;
    }

    /**
     * Starting from a PL/I source fragment translates to COBOL.
     * @param source PL/I source fragment.
     * @return a COBOL fragment
     * @throws Exception if emit fails
     */
    private String emit(final String source) throws Exception {
        CommonTree ast = parse(source);
        if (_log.isDebugEnabled()) {
            _log.debug(ast.toStringTree());
        }
        TreeNodeStream nodes = new CommonTreeNodeStream(ast);
        PLIStructureCobolEmitter emitter = new PLIStructureCobolEmitter(nodes);
        StringTemplateGroup stgGroup = new StringTemplateGroup(
                new BufferedReader(
                        new InputStreamReader(
                                getClass().getResourceAsStream(
                                "/pli2cob.stg"))));
        emitter.setTemplateLib(stgGroup);
        PLIStructureCobolEmitter.pl1code_return r = emitter.pl1code();
        StringTemplate output = (StringTemplate) r.getTemplate();
        return output.toString();
    }

    /**
     * Translate PL/I fragment to COBOL and test it against an expected result.
     * <p/>
     * For convenience we trim the generated COBOL comment header so that it does
     * not clutter the expected results.
     * <p/>
     * We also trim the last newline sequence if any.
     * @param source PL/I source fragment
     * @param expected expected COBOL fragment
     * @throws Exception if test fails
     */
    public void emitAndCheck(final String source, final String expected) throws Exception {
        String result = emit(source);
        if (result == null) {
            if (expected == null) {
                return;
            }
            fail();
        }
        assertEquals(expected, trimCobolComment(result));
    }

    /**
     * A helper to perform a PLI to COBOL conversion.
     * @param source the PLI source fragment
     * @param expected the expected COBOL result
     * @param syncpad whether to generate padding for PLI alignments
     * @param synchang whether to generate initial hang for PL/I alignment.
     */
    public void translateAndCheck(
            final String source,
            final String expected,
            final boolean syncpad,
            final boolean synchang) {
        try {
            PLIStructureToCobol pli2cob = new PLIStructureToCobol();
            pli2cob.getContext().setAddPAd(syncpad);
            pli2cob.getContext().setAddHang(synchang);
            assertEquals(expected, trimCobolComment(pli2cob.translate(source)));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
    
    /**
     * Generated source will start with a comment header that we remove
     * here to prevent all expected results to have to specify this
     * comment.
     * <p/>
     * Also trims trailing new line for convenience
     * @param source the source code to trim
     * @return source without comment header (if any)
     */
    public String trimCobolComment(final String source) {
        String result = source;
        int idx = result.indexOf(""
                + "      *" + LS
                + "      * Generated by legstar-pli2cob" + LS
                + "      *" + LS);
        if (idx > -1) {
            result = result.substring(
                    ("      *" + LS
                            + "      * Generated by legstar-pli2cob" + LS
                            + "      *" + LS).length());
        }
        int lnli = result.lastIndexOf(LS);
        if (lnli > -1) {
            result = result.substring(0, result.length() - LS.length());
        }
        return result;
    }

    /**
     * @param source a single declare source fragment
     * @return the corresponding data item
     */
    public PLIDataItem getPLIDataItem(final String source) {
        CommonTree ast = parseAndEnhance(source);
        return new PLIDataItem(ast, null);
    }

    /**
     * Produce a graphviz source for an abstract syntax tree.
     * @param ast the abstract syntax tree
     * @return a graphviz source
     */
    public String getGraph(final Tree ast) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(ast);
        return st.toString();
    }

    /**
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

}
