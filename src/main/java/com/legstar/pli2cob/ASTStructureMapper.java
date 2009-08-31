package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.model.PLIDataItem;

/**
 * This class contains the logic described in PLI Language Reference for
 * structure mapping.
 * <p/>
 * Its role is to insert padding characters where needed in a structure in order
 * for the converted COBOL structure to map exactly the same data items in memory
 * as the original PLI structure.
 * <p/>
 * Padding characters result in new logical AST nodes in the Abstract Syntax Tree.
 *
 */
public class ASTStructureMapper {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Takes a normalized (hierarchical) AST produced by {@link ASTNormalizer} and inserts
     * padding nodes where needed to satisfy alignment requirements.
     * <p/>
     * The input AST might contain a single root statement or multiple root statements under
     * a NIL node.
     * @param ast the normalized AST 
     * @return an enhanced AST
     */
    public CommonTree map(final CommonTree ast) {
        if (ast == null) {
            return null;
        }
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = new MinorStructure();
        int logicalLevelNumber = 1;
        if (ast.isNil()) {
            int n = adaptor.getChildCount(ast);
            for (int i = 0; i < n; i++) {
                Object childNode = adaptor.getChild(ast, i);
                deepestStructure = findDeepestMinorStructure(
                        adaptor, childNode, logicalLevelNumber, deepestStructure);
            }
        } else {
            deepestStructure = findDeepestMinorStructure(
                    adaptor, ast, logicalLevelNumber, deepestStructure);
        }
        return ast;
    }

    /**
     * Determines the deepest minor structure recursively.
     * <p/>
     * Every child is examined in turn, if it yields a deeper minor structure
     * than this node, then the child deepest minor structure takes precedence.
     * <p/>
     * When multiple children have same deepest minor structure levels, the
     * first child deepest structure is returned.
     * @param adaptor the tree navigator helper
     * @param node the current minor structure node
     * @param logicalLevelNumber the current minor structure logical level
     * @param deepestStructure the current deepest structure
     * @return the new deepest structure
     */
    public MinorStructure findDeepestMinorStructure(
            final TreeAdaptor adaptor,
            final Object node,
            final int logicalLevelNumber,
            final MinorStructure deepestStructure) {
        int childsLogicalLevelNumber = logicalLevelNumber + 1;
        PLIDataItem dataItem = new PLIDataItem(adaptor, node);
        if (_log.isDebugEnabled()) {
            _log.debug("Looking for deepest minor structure in: " + dataItem);
        }

        MinorStructure resultDeepestStructure = deepestStructure;
        if (dataItem.isGroup() && logicalLevelNumber > deepestStructure._logicalLevel) {
            resultDeepestStructure = new MinorStructure(dataItem, logicalLevelNumber, node);
        }
        int n = adaptor.getChildCount(node);
        for (int i = 0; i < n; i++) {
            Object childNode = adaptor.getChild(node, i);
            if (adaptor.getType(childNode) == PLIStructureParser.DATA_ITEM) {
                resultDeepestStructure = findDeepestMinorStructure(
                        adaptor, childNode, childsLogicalLevelNumber, resultDeepestStructure);
            }
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Found deepest minor structure: " + resultDeepestStructure);
        }
        return resultDeepestStructure;
    }

    public void OrderPairing(final TreeAdaptor adaptor, final MinorStructure deepestStructure) {
        if (deepestStructure.isNil()) {
            return;
        }
        Object node = deepestStructure.getNode();
        System.out.println(" Minor Structure=" + new PLIDataItem(adaptor, node));

        /* List data item elements*/
        int n = adaptor.getChildCount(node);
        Object firstChildNode = null;
        for (int i = 0; i < n; i++) {
            Object childNode = adaptor.getChild(node, i);
            if (adaptor.getType(childNode) == PLIStructureParser.DATA_ITEM) {
                if (firstChildNode == null) {
                    firstChildNode = childNode;
                } else {
                    Object secondChildNode = childNode;
                    System.out.println(" Pair."
                            + " First=" + new PLIDataItem(adaptor, firstChildNode)
                            + " Second=" + new PLIDataItem(adaptor, secondChildNode));
                }
            }
        }

    }

    /**
     * In the sense of the PLI structure mapping algorithm, a minor structure
     * is a structure declared with a level number greater than 1.
     */
    public class MinorStructure {

        /** The structure data item. */
        private PLIDataItem _dataItem;

        /** The logical level at which this structure appears. */
        private int _logicalLevel;

        /** The corresponding node in the Abstract Syntax Tree. */
        private Object _node;

        /**
         * Constructs a nil minor structure.
         */
        public MinorStructure() {
            this(null, 0, null);
        }

        /**
         * @param dataItem structure data item
         * @param logicalLevel logical level at which this structure appears
         * @param node corresponding node in the Abstract Syntax Tree
         */
        public MinorStructure(
                final PLIDataItem dataItem,
                final int logicalLevel,
                final Object node) {
            _dataItem = dataItem;
            _logicalLevel = logicalLevel;
            _node = node;
        }

        /** {@inheritDoc}*/
        public String toString() {
            return "Structure."
            + " Logical level=" + _logicalLevel
            + " Item=" + _dataItem
            + " Node=" + _node;
        }

        /**
         * @return the structure data item
         */
        public PLIDataItem getDataItem() {
            return _dataItem;
        }

        /**
         * @return the logical level at which this structure appears
         */
        public int getLogicalLevel() {
            return _logicalLevel;
        }

        /**
         * @return the corresponding node in the Abstract Syntax Tree
         */
        public Object getNode() {
            return _node;
        }

        /**
         * @return true if this is a nil minor structure
         */
        public boolean isNil() {
            return (getDataItem() == null) && (getLogicalLevel() == 0) && (getNode() == null);
        }

    }
    
    /**
     * Represents an aggregate of consecutive data items.
     * <p/>
     *
     */
    public class mappingUnit {
        
        /** Offset of this unit from a doubleword boundary. */
        private int _offset = 0;
        
        /** Alignment requirement of this unit.*/
        private PLIDataItem.AlignmentRequirement _alignmentRequirement = PLIDataItem.AlignmentRequirement.BYTE;
        
        /** Size in bytes of this unit. */
        private int _length;
        
        public mappingUnit(final PLIDataItem firstDataItem, final PLIDataItem secondDataItem) {
            
        }
        
        
        
    }

}
