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
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.DOTTreeGenerator;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.stringtemplate.StringTemplate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import junit.framework.TestCase;

/**
 * Not a real test. A convenience class that generates a reference graph for
 * documentation purposes. The Graph is an abstract syntax tree representing
 * mainframe data.
 *
 */
public class ASTReferenceGraph extends TestCase {
    
    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Produce a graph of all possible attributes.
     */
    public void testGraphProd() {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        CommonTree ast = new CommonTree();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        Object dataItem = adaptor.create(PLIStructureParser.DATA_ITEM, "DATA_ITEM");
        addAttribute(adaptor, dataItem, PLIStructureParser.NAME, "NAME", "name | *");
        addAttribute(adaptor, dataItem, PLIStructureParser.LEVEL, "LEVEL", "level");
        addAttribute(adaptor, dataItem, PLIStructureParser.UNION, "UNION", null);

        /* String elementary types */
        Object stringNode = addAttribute(adaptor, dataItem, PLIStructureParser.STRING,
                "STRING", "BIT | CHARACTER | GRAPHIC | WIDECHAR");
        Object lengthNode = addAttribute(adaptor, stringNode, PLIStructureParser.LENGTH, "LENGTH", "length");
        addAttribute(adaptor, lengthNode, PLIStructureParser.REFER, "REFER", "dataItemName");
        addAttribute(adaptor, stringNode, PLIStructureParser.VARYING, "VARYING", "NONVARYING | VARYING | VARYINGZ");

        /* Picture elementary types */
        addAttribute(adaptor, dataItem, PLIStructureParser.PICTURE, "PICTURE", "pictureValue");

        /* Numeric elementary types */
        Object arithmeticNode = addAttribute(adaptor, dataItem, PLIStructureParser.ARITHMETIC, "ARITHMETIC",
                "FLOAT | FIXED | BINARY | DECIMAL | SIGNED | UNSIGNED | REAL | COMPLEX");
        Object precisionNode = addAttribute(adaptor, arithmeticNode, PLIStructureParser.PRECISION, "PRECISION",
                "precision");
        addAttribute(adaptor, precisionNode, PLIStructureParser.SCALING_FACTOR, "SCALING_FACTOR", "scalingFactor");

        /* Array types */
        Object dimensionsNode = addAttribute(adaptor, dataItem, PLIStructureParser.DIMENSIONS, "DIMENSIONS", null);
        Object dimensionNode = addAttribute(adaptor, dimensionsNode, PLIStructureParser.DIMENSION, "DIMENSION", null);
        Object lowBoundNode = addAttribute(adaptor, dimensionNode, PLIStructureParser.LBOUND, "LBOUND", "lowBound");
        addAttribute(adaptor, lowBoundNode, PLIStructureParser.REFER, "REFER", "dataItemName");
        Object highBoundNode = addAttribute(adaptor, dimensionNode, PLIStructureParser.HBOUND, "HBOUND", "highBound");
        addAttribute(adaptor, highBoundNode, PLIStructureParser.REFER, "REFER", "dataItemName");
        
        /* Other attributes */
        addAttribute(adaptor, dataItem, PLIStructureParser.ALIGNMENT, "ALIGNMENT", "ALIGNED | UNALIGNED");
        addAttribute(adaptor, dataItem, PLIStructureParser.INITIAL, "INITIAL", "value");
        addAttribute(adaptor, dataItem, PLIStructureParser.STORAGE, "STORAGE",
                "AUTOMATIC | STATIC | BASED | CONTROLLED");

        adaptor.addChild(ast, dataItem);
        StringTemplate st = gen.toDOT(ast);
        _log.info(st.toString());
    }
    
    /**
     * Add a new attribute to a parent node.
     * @param adaptor the tree helper
     * @param parent parent node
     * @param tokenType the new node token type
     * @param tokenText the new node name
     * @param value the new node value
     * @return a new node that was just added to parent
     */
    private Object addAttribute(
            final TreeAdaptor adaptor,
            final Object parent,
            final int tokenType,
            final String tokenText, final String value) {
        Object node = adaptor.create(tokenType, tokenText);
        if (value != null) {
            String[] values = value.split("\\s\\|\\s");
            for (String avalue :  values) {
                Object nodeValue = adaptor.create(PLIStructureParser.STRING_LITERAL, avalue);
                adaptor.addChild(node, nodeValue);
            }
        }
        adaptor.addChild(parent, node);
        return node;
        
    }
    

}
