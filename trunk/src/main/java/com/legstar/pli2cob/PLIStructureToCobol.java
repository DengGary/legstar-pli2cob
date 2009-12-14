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
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;

import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.TreeNodeStream;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.ANTLRNoCaseReaderStream;
import com.legstar.pli2cob.smap.ASTStructureMapper;
import com.legstar.pli2cob.smap.StructureMappingException;

/**
 * Implements a PL/I Structure to COBOL translator.
 * <p/>
 * There are 7 steps involved:
 * <ul>
 * <li>Cleaning the source from non PL/I Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful PL/I statements</li>
 * <li>Enhancing the AST, which adds attributes and nodes to the AST produced by the previous step</li>
 * <li>Mapping (if selected) adds filler nodes to account for PL/I special structure mapping algorithm</li>
 * <li>Emitting COBOL equivalents to the PL/I statements</li>
 * <li>Formatting COBOL source so that it respects column constraints</li>
 * </ul>
 * This is the API made available to programmatically invoke the PL/I to COBOL translator.
 * 
 * 
 */
public class PLIStructureToCobol {

    /** Execution parameters for the PL/I to COBOL utility. */
    private Pli2CobContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Default constructor.
     */
    public PLIStructureToCobol() {
        this(new Pli2CobContext());
    }

    /**
     * @param context execution parameters for the PL/I to COBOL utility
     */
    public PLIStructureToCobol(final Pli2CobContext context) {
        _context = context;
    }

    /**
     * Execute the translation from PL/I to COBOL.
     * @param pliSource the PL/I source code
     * @return the COBOL source code
     * @throws PLIStructureLexingException if PL/I structure is unreadable
     * @throws PLIStructureParsingException if source contains unsupported statements
     * @throws CobolFormatException if formatting fails
     * @throws PLIStructureReadingException if source code cannot be read
     */
    public String translate(
            final String pliSource) throws PLIStructureLexingException,
            PLIStructureParsingException,
            CobolFormatException,
            PLIStructureReadingException {
        return format(emit(map(enhance(parse(lexify(clean(pliSource)))))));
    }

    /**
     * Execute the translation from PL/I to COBOL.
     * @param pliSourceFile the PL/I source code
     * @param targetDir folder where COBOL result is to be written
     * @return the COBOL source code
     * @throws PLIStructureLexingException if PL/I structure is unreadable
     * @throws PLIStructureParsingException if source contains unsupported statements
     * @throws CobolFormatException if formatting fails
     * @throws PLIStructureReadingException if source code cannot be read
     */
    public File translate(
            final File pliSourceFile,
            final File targetDir) throws PLIStructureLexingException,
            PLIStructureParsingException,
            CobolFormatException,
            PLIStructureReadingException {
        try {
            _log.info("Translating PL/I file: " + pliSourceFile);
            String cobolSource = translate(
                    FileUtils.readFileToString(pliSourceFile));
            String cobolFileName = pliSourceFile.getName() + ".cbl";
            File cobolSourceFile = new File(targetDir, cobolFileName);
            FileUtils.writeStringToFile(cobolSourceFile, cobolSource);
            _log.info("Created COBOL file: " + cobolSourceFile);
            return cobolSourceFile;
        } catch (IOException e) {
            _log.error("Translation failed", e);
            throw new PLIStructureReadingException(e);
        }
    }

    /**
     * Remove any non PL/I Structure characters from the source.
     * @param pliSource the raw source
     * @return a cleaned up source
     * @throws PLIStructureReadingException if source code cannot be read
     */
    public String clean(final String pliSource) throws PLIStructureReadingException {
        if (_log.isDebugEnabled()) {
            debug("Cleaning PL/I source code:", pliSource);
        }
        PLISourceCleaner cleaner = new PLISourceCleaner();
        return cleaner.execute(pliSource);
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     * @throws PLIStructureLexingException if PL/I structure is unreadable
     */
    public CommonTokenStream lexify(final String source) throws PLIStructureLexingException {
        if (_log.isDebugEnabled()) {
            _log.debug("Lexing PL/I source code");
        }
        String errorMessage = "Lexing failed.";
        try {
            PLIStructureLexer lex = new PLIStructureLexerImpl(
                    new ANTLRNoCaseReaderStream(new StringReader(source)));
            CommonTokenStream tokens = new CommonTokenStream(lex);
            if (lex.getNumberOfSyntaxErrors() != 0 || tokens == null) {
                _log.error(errorMessage);
                throw (new PLIStructureLexingException(errorMessage));
            }
            return tokens;
        } catch (IOException e) {
            _log.error(errorMessage, e);
            throw (new PLIStructureLexingException(e));
        }
    }

    /**
     * Apply Parser to produce an abstract syntax tree from a token stream. 
     * @param tokens the stream token produced by lexer
     * @return an antlr abstract syntax tree
     * @throws PLIStructureParsingException if source contains unsupported statements
     */
    public CommonTree parse(final CommonTokenStream tokens) throws PLIStructureParsingException {
        if (_log.isDebugEnabled()) {
            debug("Parsing tokens:", tokens.toString());
        }
        String errorMessage = "Parsing token stream failed.";
        try {
            PLIStructureParser parser = new PLIStructureParserImpl(tokens);
            PLIStructureParser.pl1code_return parserResult = parser.pl1code();
            if (parser.getNumberOfSyntaxErrors() != 0 || parserResult == null) {
                _log.error(errorMessage);
                throw (new PLIStructureParsingException(errorMessage));
            }
            return (CommonTree) parserResult.getTree();
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new PLIStructureParsingException(e));
        }
    }

    /**
     * Apply Enhancer to produce a complete abstract syntax tree from source. 
     * @param ast the abstract syntax tree produced by parser
     * @return an antlr abstract syntax tree where nodes got enhanced
     * @throws PLIStructureParsingException if tree cannot be walked
     */
    public CommonTree enhance(final CommonTree ast) throws PLIStructureParsingException {
        if (_log.isDebugEnabled()) {
            debug("Source abstract syntax tree: ", ((ast == null) ? "null" : ast.toStringTree()));
        }
        if (ast == null) {
            return ast;
        }
        String errorMessage = "Parsing nodes stream failed.";
        try {
            TreeNodeStream nodes = new CommonTreeNodeStream(ast);
            PLIStructureEnhancer enhancer = new PLIStructureEnhancer(nodes);
            PLIStructureEnhancer.pl1code_return enhancerResult = enhancer.pl1code();
            return (CommonTree) enhancerResult.getTree();
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new PLIStructureParsingException(e));
        }
    }

    /**
     * Analyzes the PL/I structures mapping. This can produce reports and,
     * if requested, add padding characters where needed in the abstract
     * syntax tree in order for COBOL to map the PL/I structure alignments. 
     * @param ast the abstract syntax tree produced by enhancer
     * @return a hierarchical abstract syntax tree with extra padding bytes where needed
     * @throws PLIStructureParsingException if padding algorithm fails
     */
    public CommonTree map(final CommonTree ast) throws PLIStructureParsingException {
        String errorMessage = "Mapping PL/I structures failed.";
        try {
            ASTStructureMapper mapper = new ASTStructureMapper(getContext());
            return mapper.map(ast);
        } catch (StructureMappingException e) {
            _log.error(errorMessage, e);
            throw (new PLIStructureParsingException(e));
        }
    }

    /**
     * Emit COBOL statements from enhanced abstract syntax tree. 
     * @param ast the abstract syntax tree produced by parser/enhancer/mapper
     * @return a COBOL source where statements do not necessarily fit in 72 columns
     * @throws CobolFormatException if formatting fails
     */
    public String emit(final CommonTree ast) throws CobolFormatException {
        if (_log.isDebugEnabled()) {
            debug("Enhanced abstract syntax tree:", ast);
        }
        if (ast == null) {
            return "";
        }
        String errorMessage = "Emiting COBOL from AST failed.";
        try {
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
        } catch (RecognitionException e) {
            _log.error(errorMessage, e);
            throw (new CobolFormatException(e));
        }
    }

    /**
     * Fit COBOL statements in 72 columns.
     * @param cobolSource the source where statements may not wrap
     * @return a COBOL source ready for compilation
     * @throws CobolFormatException if source code cannot be formatted
     */
    public String format(final String cobolSource) throws CobolFormatException {
        if (_log.isDebugEnabled()) {
            debug("Formatting COBOL source code:", cobolSource);
        }
        if (cobolSource == null) {
            return cobolSource;
        }
        CobolFormatter formatter = new CobolFormatter();
        return formatter.format(new StringReader(cobolSource));
    }

    /**
     * Produce long text in a delimited debug zone.
     * @param title the debug text title
     * @param text the text itself
     */
    private void debug(final String title, final String text) {
        _log.debug(title);
        _log.debug(text);
        _log.debug("----------------------------------------------------------------");
    }

    /**
     * Produce a graph of an abstract syntax tree.
     * @param title the debug text title
     * @param ast the abstract syntax tree
     */
    private void debug(final String title, final CommonTree ast) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(ast);
        _log.debug(title);
        _log.debug(st.toString());
        _log.debug("----------------------------------------------------------------");
    }

    /**
     * @return the execution parameters for the PL/I to COBOL utility
     */
    public Pli2CobContext getContext() {
        return _context;
    }

}
