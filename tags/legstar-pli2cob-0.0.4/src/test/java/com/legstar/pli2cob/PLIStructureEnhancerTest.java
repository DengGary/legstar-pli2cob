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

import org.antlr.runtime.tree.CommonTree;

/**
 * The Enhancer grammer is responsible for adding to the AST created by
 * the parser.
 *
 */
public class PLIStructureEnhancerTest extends AbstractTester {

    /**
     * A hierarchy with no explicit alignment should leave the tree unchanged.
     * @throws Exception if test fails
     */
    public void testNoExplicitAlignment() throws Exception {
        enhanceAndCheck("dcl a based, 2 b fixed bin(15) init(3);",
                "(DATA_ITEM (NAME a) (STORAGE BASED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ARITHMETIC FIXED BINARY (PRECISION 15)) (INITIAL 3)))"
                );
    }

    /**
     * An aligned item should stay aligned if parent is not explicitly aligned.
     * @throws Exception if test fails
     */
    public void testParentNotExplicitlyAligned() throws Exception {
        enhanceAndCheck("dcl a based, 2 b fixed bin aligned;",
                "(DATA_ITEM (NAME a) (STORAGE BASED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ARITHMETIC FIXED BINARY) (ALIGNMENT ALIGNED)))"
                );
    }

    /**
     * A non explicitly aligned item should inherit parent's explicit alignment.
     * @throws Exception if test fails
     */
    public void testParentExplicitlyAligned() throws Exception {
        enhanceAndCheck("dcl a aligned, 2 b fixed bin;",
                "(DATA_ITEM (NAME a) (ALIGNMENT ALIGNED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ARITHMETIC FIXED BINARY) (ALIGNMENT ALIGNED)))"
                );
    }

    /**
     * Alignment should propagate to all children.
     * @throws Exception if test fails
     */
    public void testPropagationToAllChildren() throws Exception {
        enhanceAndCheck("dcl a aligned, 2 b, 2 c fixed bin;",
                "(DATA_ITEM (NAME a) (ALIGNMENT ALIGNED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ALIGNMENT ALIGNED))"
                + " (DATA_ITEM (LEVEL 2) (NAME c) (ARITHMETIC FIXED BINARY) (ALIGNMENT ALIGNED)))"
                );
    }

    /**
     * Alignment should propagate down a hierarchy.
     * @throws Exception if test fails
     */
    public void testPropagationToHierarchy() throws Exception {
        enhanceAndCheck("dcl a aligned, 2 b, 3 c fixed bin;",
                "(DATA_ITEM (NAME a) (ALIGNMENT ALIGNED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ALIGNMENT ALIGNED)"
                + " (DATA_ITEM (LEVEL 3) (NAME c) (ARITHMETIC FIXED BINARY) (ALIGNMENT ALIGNED))))"
                );
    }

    /**
     * Child alignment should take precedence over its parent.
     * @throws Exception if test fails
     */
    public void testPropagationPrecedence() throws Exception {
        enhanceAndCheck("dcl a aligned, 2 b unaligned, 3 c fixed bin;",
                "(DATA_ITEM (NAME a) (ALIGNMENT ALIGNED)"
                + " (DATA_ITEM (LEVEL 2) (NAME b) (ALIGNMENT UNALIGNED)"
                + " (DATA_ITEM (LEVEL 3) (NAME c) (ARITHMETIC FIXED BINARY) (ALIGNMENT UNALIGNED))))"
                );
    }

    /**
     * Enhance PL/I AST and test it against an expected result.
     * @param source PL/I source fragment
     * @param expected tree (as returned from toStringTree())
     * @throws Exception if test fails
     */
    private void enhanceAndCheck(final String source, final String expected) throws Exception {
        CommonTree ast = parseAndEnhance(source);
        if (ast == null) {
            if (expected == null) {
                return;
            }
            fail();
        }
        assertEquals(expected, ast.toStringTree());
    }
}
