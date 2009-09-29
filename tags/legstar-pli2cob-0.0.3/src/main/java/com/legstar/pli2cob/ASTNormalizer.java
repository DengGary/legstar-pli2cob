package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.util.ASTUtils;

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
 * <p/>
 * Normalizing also involves completing evaluation for certain attributes which are
 * inherited from the hierarchy. ALIGNED/UNALIGNED are such attributes.
 * <p/>
 * Normalizing is also used to inform union children that they redefine each other.
 *
 */
public class ASTNormalizer {

    /**
     * Takes a flat AST produced by {@link PLIStructureParser} and creates a hierarchical AST
     * where data item nodes are attached to their parent based on their PLI level number.
     * <p/>
     * The input ast might hold a single data item in which case it is the root node. Otherwise
     * there is a root node that serves as parent for the flat collection of data items.
     * @param ast the flat AST 
     * @return a hierarchical AST
     */
    public CommonTree normalize(final CommonTree ast) {
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
                insertChildNode(adaptor, currentDataItem, dataItem);
                currentDataItem = dataItem;
            }
        }
        return root;

    }

    /**
     * Inserts a new data item as a child of the current data item or one of its parents.
     * <p/>
     * If the current data item is not higher in the hierarchy than the new data item
     * (ie it has a higher or equal level) then we lookup the parents until we find one
     * that is higher. Knowing that the root is higher than anyone, this will always
     * find a place to insert the new node.
     * <p/>
     * The root node, if it is not a data item (ie is Nil), has an artificial level of
     * 0 which is lower than any real data item level.
     * <p/>
     * When a parent/child relationship is identified, propagates inherited attributes.
     * @param adaptor the tree navigator
     * @param currentNode the current data item node
     * @param newNode the new data item node
     */
    private void insertChildNode(
            final TreeAdaptor adaptor,
            final Object currentNode,
            final Object newNode) {
        Object parentNode = currentNode;
        int parentLevel = ASTUtils.getAttributeIntValue(adaptor, parentNode, PLIStructureParser.LEVEL, 0);
        int newDataItemLevel = ASTUtils.getAttributeIntValue(adaptor, newNode, PLIStructureParser.LEVEL, 0);
        while (parentLevel >= newDataItemLevel) {
            parentNode = adaptor.getParent(parentNode);
            parentLevel = ASTUtils.getAttributeIntValue(adaptor, parentNode, PLIStructureParser.LEVEL, 0);
        }
        adaptor.addChild(parentNode, newNode);
        propagateAlignment(adaptor, parentNode, newNode);
        propagateRefines(adaptor, parentNode, newNode);
    }

    /**
     * If a parent is explicitly aligned or unaligned and the child has no explicit
     * alignment then it inherits its parent alignment.
     * <p/>
     * This results in the child being explicitly aligned or unaligned. We add an artificial
     * attribute to the AST node.
     * @param adaptor the tree navigator helper
     * @param parentNode the parent node
     * @param childNode the child node
     */
    private void propagateAlignment(
            final TreeAdaptor adaptor,
            final Object parentNode,
            final Object childNode) {

        Object alignmentNode = ASTUtils.getAttribute(adaptor, parentNode, PLIStructureParser.ALIGNMENT);
        if (alignmentNode != null) {
            Object childAlignmentNode = ASTUtils.getAttribute(
                    adaptor, childNode, PLIStructureParser.ALIGNMENT);
            if (childAlignmentNode == null) {
                Object newAttributeTree = adaptor.dupTree(alignmentNode);
                adaptor.addChild(childNode, newAttributeTree);
            }
        }

    }
    
    /**
     * When a child node is added to a parent which happens to be a union, then the
     * child node is marked as redefining the first child of that union. This does
     * not apply to the first child (no point in redefining oneself).  
     * @param adaptor the tree navigator helper
     * @param parentNode the parent node (potentially a union)
     * @param childNode the child node. If it is not the first union child it needs
     *  to know the first child child which memory location it redefines
     */
    private void propagateRefines(
            final TreeAdaptor adaptor,
            final Object parentNode,
            final Object childNode) {
        Object unionNode = ASTUtils.getAttribute(adaptor, parentNode, PLIStructureParser.UNION);
        if (unionNode != null) {
            String redefinesName = ASTUtils.getAttributeStringValue(
                    adaptor, parentNode, PLIStructureParser.REDEFINES, null);
          
            if (redefinesName == null) {

                /* The first union child marks the union with its name to be propagated
                 *  on the next children */

                redefinesName = ASTUtils.getAttributeStringValue(
                        adaptor, childNode, PLIStructureParser.NAME, null);
                ASTUtils.setAttribute(adaptor, parentNode, PLIStructureParser.REDEFINES,
                        PLIStructureParser.DATA_ITEM_NAME, redefinesName);

            } else {
                
                /* Subsequent children are marked with the first child name (since they 
                 * redefine its memory location)*/
                ASTUtils.setAttribute(adaptor, childNode, PLIStructureParser.REDEFINES,
                        PLIStructureParser.DATA_ITEM_NAME, redefinesName);
            }
            
        }
    }

}
