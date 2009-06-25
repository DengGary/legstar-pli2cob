package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

/**
 * Normalize an abstract syntax tree.
 * <p/>
 * In the long term this can be used to produce an AST which model is common to
 * z/OS legacy languages (COBOL, PLI, Natural, ...).
 * <p/>
 * For now it simply reorganizes the AST produced by the parser.
 * AST produced by parser is flat. All data item nodes are direct children of nil.
 * This program creates a new AST where data item nodes are attached to their natural
 * parent. This recreates a hierarchy instead of a mere list.
 * <p/>
 * Parent/child relationship is determined by their level numbers. A child must have a
 * higher level than its parent.
 *
 */
public final class PLIStructureTreeNormalizer {

    /**
     * Utility class. No instantiation.
     */
    private PLIStructureTreeNormalizer() {

    }

    /**
     * Takes a flat AST produced by {@link PLIStructureParser} and creates a hierarchical AST
     * where data item nodes are attached to their parent based on their PLI level number.
     * <p/>
     * The input ast might hold a single data item in which case it is the root node. Otherwise
     * there is a root node that serves as parent for the flat collection of data items.
     * @param ast the flat AST 
     * @return a hierarchical AST
     */
    public static CommonTree normalize(final CommonTree ast) {
        if (ast == null) {
            return null;
        }
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        CommonTree root = new CommonTree();
        Object currentDataItem = root;
        if (ast.getType() == PLIStructureParser.DATA_ITEM) {
            return ast;
        }
        int n = adaptor.getChildCount(ast);
        for (int i = 0; i < n; i++) {
            Object dataItem = adaptor.getChild(ast, i);
            int type = adaptor.getType(dataItem);
            if (type == PLIStructureParser.DATA_ITEM) {
                insertNode(adaptor, currentDataItem, dataItem);
                currentDataItem = dataItem;
            }
        }
        return root;

    }

    /**
     * Inserts a new data item as a child of the current data item or one of its parents.
     * <p/>
     * If the current data item is not higher the hierarchy than the new data item
     * (ie has a higher or equal level) then we lookup the parents untl we find one
     * that is higher. Knowing that the root is higher than anyone, this will always
     * find a place to insert the new node.
     * @param adaptor the tree navigator
     * @param currentDataItem the current data item
     * @param newDataItem the new data item
     */
    private static void insertNode(
            final TreeAdaptor adaptor,
            final Object currentDataItem,
            final Object newDataItem) {
        Object parentDataItem = currentDataItem;
        while (getLevel(adaptor, parentDataItem) >= getLevel(adaptor, newDataItem)) {
            parentDataItem = adaptor.getParent(parentDataItem);
        }
        adaptor.addChild(parentDataItem, newDataItem);
    }

    /**
     * Gets the PLI level number for a given data item.
     * <p/>
     * For the root node, if it is not a data item, return an artificial level of 0 which will
     * be lower than any real data item level.
     * <p/>
     * If a data item other than root doesn't have a level we return 1.
     * @param adaptor the tree navigator
     * @param dataItem the data item
     * @return the level number if one is found, 1 otherwise
     */
    private static int getLevel(final TreeAdaptor adaptor, final Object dataItem) {
        if (adaptor.isNil(dataItem)) {
            return 0;
        }
        int n = adaptor.getChildCount(dataItem);
        for (int i = 0; i < n; i++) {
            Object attribute = adaptor.getChild(dataItem, i);
            if (adaptor.getType(attribute) == PLIStructureParser.LEVEL) {
                Object value = adaptor.getChild(attribute, 0);
                return Integer.parseInt(adaptor.getText(value));
            }
        }
        return 1;
    }

}
