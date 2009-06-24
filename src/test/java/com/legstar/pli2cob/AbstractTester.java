package com.legstar.pli2cob;


import java.io.IOException;
import java.io.StringReader;

import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.Tree;
import org.antlr.stringtemplate.StringTemplate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.PLIStructureParser.script_return;

import junit.framework.TestCase;

/**
 * Common test helper code.
 *
 */
public abstract class AbstractTester extends TestCase {

	/** Line separator (OS specific)*/
	public static final String _LS = System.getProperty("line.separator");
	
	/** Logger. */
	private final Log _log = LogFactory.getLog(getClass());

	/**
	 * Apply the lexer to produce a token stream from source.
	 * @param source the source code
	 * @return an antlr token stream
	 */
	public CommonTokenStream lexify(final String source) {
		try {
			PLIStructureLexer lex = new PLIStructureLexer(
					new ANTLRReaderStream(new StringReader(source)));
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
			PLIStructureParser parser = new PLIStructureParser(tokens);
			script_return parserResult = parser.script();
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
	 * Apply Lexer + Parser + Normalizer to produce an abstract syntax tree from source. 
	 * @param source the source code
	 * @return an antlr abstract syntax tree where nodes are normalized
	 */
	public CommonTree normalize(final String source) {
		return PLIStructureTreeNormalizer.normalize(
				parse(source));
	}
	
	/**
	 * Produce a graphviz source for an abstract syntax tree.
	 * @param ast the abstract syntax tree
	 * @return a graphviz source
	 */
	public String getGraph(final Tree ast) {
		DOTTreeGenerator gen = new DOTTreeGenerator();
		StringTemplate st = gen.toDOT(ast);
		if (_log.isDebugEnabled()) {
			_log.debug(st.toString());
		}
		return st.toString();
	}
	
	/**
	 * To ease testing, extract the subpart of the graph that shows the
	 * hierarchy.
	 * @param graph the graphviz graph
	 * @return a subpart with hierarchy only
	 */
	public String getSubGraph(final String graph) {
		int start = graph.indexOf("n0 -> n1");
		if (start > -1) {
			return graph.substring(start -2, graph.length() - 7).replace(_LS, "");
		} else {
			return "";
		}
	}
}