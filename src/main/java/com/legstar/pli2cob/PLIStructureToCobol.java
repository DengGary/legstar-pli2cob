package com.legstar.pli2cob;

import java.io.IOException;
import java.io.StringReader;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.stringtemplate.StringTemplate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.PLIStructureParser.script_return;

/**
 * Implements a PLI Structure to COBOL converter.
 * <p/>
 * There are 4 steps involved:
 * <ul>
 * <li>Cleaning the source from non PLI Structure characters</li>
 * <li>Lexing the source to extract meaningful keywords</li>
 * <li>Parsing keywords to extract meaningful PLI statements</li>
 * <li>Converting PLI statements to COBOL equivalents</li>
 * </ul>
 * 
 */
public class PLIStructureToCobol {

    /** Execution parameters for the PLI to COBOL utility. */
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
     * @param context execution parameters for the PLI to COBOL utility
     */
    public PLIStructureToCobol(final Pli2CobContext context) {
        _context = context;
    }

    /**
     * Execute the transformation from PLI to COBOL.
     * @param pliSource the PLI source code
     * @return the COBOL source code
     * @throws PLIStructureLexingException if PLI structure is unreadable
     * @throws PLIStructureParsingException if source contains unsupported statements
     * @throws CobolFormatException if formatting fails
     * @throws PLIStructureReadingException if source code cannot be read
     */
    public String execute(
            final String pliSource) throws PLIStructureLexingException,
            PLIStructureParsingException,
            CobolFormatException,
            PLIStructureReadingException {
        return convert(normalize(parse(lexify(clean(pliSource)))));
    }


    /**
     * Remove any non PLI Structure characters from the source.
     * @param pliSource the raw source
     * @return a cleaned up source
     * @throws PLIStructureReadingException if source code cannot be read
     */
    public String clean(final String pliSource) throws PLIStructureReadingException {
        PLISourceCleaner cleaner = new PLISourceCleaner();
        return cleaner.execute(pliSource);
    }

    /**
     * Apply the lexer to produce a token stream from source.
     * @param source the source code
     * @return an antlr token stream
     * @throws PLIStructureLexingException if PLI structure is unreadable
     */
    public CommonTokenStream lexify(final String source) throws PLIStructureLexingException {
        if (_log.isDebugEnabled()) {
            debug("Lexing source:", source);
        }
        String errorMessage = "Lexing failed.";
        try {
            PLIStructureLexer lex = new PLIStructureLexer(
                    new ANTLRReaderStream(new StringReader(source)));
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
            PLIStructureParser parser = new PLIStructureParser(tokens);
            script_return parserResult = parser.script();
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
     * Apply Normalizer to produce a normalized abstract syntax tree from source. 
     * @param ast the abstract syntax tree produced by parser (flat)
     * @return an antlr abstract syntax tree where nodes are organized in a hierarchy
     */
    public CommonTree normalize(final CommonTree ast) {
        if (_log.isDebugEnabled()) {
            debug("Flat abstract syntax tree:", ast);
        }
        ASTNormalizer normalizer = new ASTNormalizer();
        return normalizer.normalize(ast);
    }

    /**
     * Final conversion starting from normalized abstract syntax tree. 
     * @param ast the abstract syntax tree produced by normalizer (hierarchy)
     * @return a PLI source
     * @throws CobolFormatException if formatting fails
     */
    public String convert(final CommonTree ast) throws CobolFormatException {
        if (_log.isDebugEnabled()) {
            debug("Normalized abstract syntax tree:", ast);
        }
        String errorMessage = "Converting abstract syntax tree: " + ast + " failed.";
        try {
            ASTToCobol converter = new ASTToCobol(getContext());
            String cobolSource = converter.convert(ast);
            if (_log.isDebugEnabled()) {
                debug("COBOL structure produced:", cobolSource);
            }
            return cobolSource;
        } catch (CobolFormatException e) {
            _log.error(errorMessage, e);
            throw e;
        }
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
     * @return the execution parameters for the PLI to COBOL utility
     */
    public Pli2CobContext getContext() {
        return _context;
    }

}
