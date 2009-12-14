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
package com.legstar.pli2cob.smap;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.antlr.ANTLRNoCaseReaderStream;
import com.legstar.pli2cob.PLIStructureLexer;
import com.legstar.pli2cob.PLIStructureLexerImpl;
import com.legstar.pli2cob.PLIStructureParser;
import com.legstar.pli2cob.PLIStructureParserImpl;
import com.legstar.pli2cob.Pli2CobContext;
import com.legstar.pli2cob.PLIStructureParser.pl1code_return;
import com.legstar.pli2cob.model.PLIDataItem;

/**
 * This class contains the logic described in PL/I Language Reference for
 * structure mapping.
 * <p/>
 * It will analyze structures mapping and detect where padding bytes might
 * be needed.
 * <p/>
 * If requested, it sill insert padding characters where needed in a structure in order
 * for the translated COBOL structure to map exactly the same data items in memory
 * as the original PL/I structure.
 * <p/>
 * Padding characters result in new logical AST nodes in the Abstract Syntax Tree.
 *
 */
public class ASTStructureMapper {

    /** Execution parameters for the PL/I to COBOL utility. */
    private Pli2CobContext _context;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Default constructor.
     */
    public ASTStructureMapper() {
        this(new Pli2CobContext());
    }

    /**
     * @param context execution parameters for the PL/I to COBOL utility
     */
    public ASTStructureMapper(final Pli2CobContext context) {
        _context = context;
    }

    /**
     * Takes an enhanced AST produced by {@link PLIStructureEnhancer} and inserts
     * padding nodes where needed to satisfy alignment requirements.
     * <p/>
     * The input AST might contain a single root statement or multiple root statements under
     * a NIL node.
     * @param ast the normalized AST 
     * @return an enhanced AST
     * @throws StructureMappingException if mapping fails
     */
    public CommonTree map(final CommonTree ast) throws StructureMappingException {
        if (ast == null) {
            return null;
        }
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        if (ast.isNil()) {
            int n = adaptor.getChildCount(ast);
            for (int i = 0; i < n; i++) {
                Object childNode = adaptor.getChild(ast, i);
                map(adaptor, (CommonTree) childNode);
            }
        } else {
            map(adaptor, ast);
        }
        return ast;
    }

    /**
     * Same as previous but assumes the root node is a data item not a nil.
     * <p/>
     * There are 4 steps:
     * <ul>
     * <li>Identify the deepest minor structure</li>
     * <li>Classify all minor structures relative to the deepest</li>
     * <li>Pair data items as described in PL/I language reference</li>
     * <li>Create new padding nodes in the abstract syntax tree</li>
     * </ul>
     * @param adaptor the tree navigator helper
     * @param ast a non nil abstract syntax tree node
     * @return an updated abstract syntax tree with extra padding nodes if necessary
     * @throws StructureMappingException if mapping fails
     */
    private CommonTree map(
            final TreeAdaptor adaptor,
            final CommonTree ast) throws StructureMappingException {
        MinorStructure deepestStructure = new MinorStructure();
        deepestStructure = findDeepestMinorStructure(
                adaptor, ast, null, 1, deepestStructure);

        List < List < MinorStructure > > minorStructures = classifyStructures(
                adaptor, ast, null, deepestStructure.getLogicalLevel());

        Map < Object, Integer> paddingNodes = new LinkedHashMap < Object, Integer>();
        StructureMappingUnit mappingUnit = mapStructures(
                adaptor, minorStructures, paddingNodes);

        if (getContext().isAddPad()) {
            addPaddingNodes(adaptor, paddingNodes);
        }
        if (getContext().isAddHang()) {
            addHangNode(adaptor, ast, mappingUnit.getOffset());
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
     * <p/>
     * This method will also populate the _minorStructures array in order to
     * prepare for pairing algorithm which process minor structures by depth.
     * @param adaptor the tree navigator helper
     * @param node the current minor structure node
     * @param qualifier used to uniquely identify nodes
     * @param logicalLevelNumber the current minor structure logical level
     * @param deepestStructure the current deepest structure
     * @return the new deepest structure
     */
    public MinorStructure findDeepestMinorStructure(
            final TreeAdaptor adaptor,
            final Object node,
            final String qualifier,
            final int logicalLevelNumber,
            final MinorStructure deepestStructure) {
        int childsLogicalLevelNumber = logicalLevelNumber + 1;
        PLIDataItem dataItem = new PLIDataItem(adaptor, node, qualifier);
        if (_log.isDebugEnabled()) {
            _log.debug("Looking for deepest minor structure in: " + dataItem);
        }

        MinorStructure resultDeepestStructure = deepestStructure;
        if (dataItem.isGroup() && logicalLevelNumber > deepestStructure.getLogicalLevel()) {
            resultDeepestStructure = new MinorStructure(dataItem, logicalLevelNumber, node);
        }
        int n = adaptor.getChildCount(node);
        String childQualifier = dataItem.getQualifiedName();
        for (int i = 0; i < n; i++) {
            Object childNode = adaptor.getChild(node, i);
            if (adaptor.getType(childNode) == PLIStructureParser.DATA_ITEM) {
                resultDeepestStructure = findDeepestMinorStructure(
                        adaptor, childNode, childQualifier, childsLogicalLevelNumber, resultDeepestStructure);
            }
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Found deepest minor structure: " + resultDeepestStructure);
        }
        return resultDeepestStructure;
    }

    /**
     * Organizes minor structures in an array where each entry is a possible depth in the 
     * Abstract Syntax Tree. This makes it easy to process all minor structures at a given
     * depth.
     * @param adaptor the tree navigator helper
     * @param node a root data item node in the AST
     * @param qualifier used to uniquely identify nodes
     * @param deepestLevel the deepest level possible
     * @return an array of structures organized by depth
     */
    public List < List < MinorStructure > > classifyStructures(
            final TreeAdaptor adaptor,
            final Object node,
            final String qualifier,
            final int deepestLevel) {
        List < List < MinorStructure > > minorStructures = new ArrayList < List < MinorStructure > >();
        for (int i = 0; i < deepestLevel; i++) {
            minorStructures.add(new ArrayList < MinorStructure >());
        }
        if (deepestLevel == 0) {
            return minorStructures;
        }
        addMinorStructures(adaptor, node, qualifier, 1, minorStructures);
        if (_log.isDebugEnabled()) {
            _log.debug("Minor structures classified by logical level:");
            for (int i = 0; i < deepestLevel; i++) {
                StringBuilder sb = new StringBuilder();
                for (MinorStructure minorStructure : minorStructures.get(i)) {
                    sb.append("[" + minorStructure.getDataItem().getName() + "] ");
                }
                _log.debug("Logical level: " + (i + 1) + " Minor structures: " + sb);
            }
        }
        return minorStructures;

    }

    /**
     * Adds a minor structure in the array at the corresponding depth and then processes
     * children.
     * @param adaptor the tree navigator helper
     * @param node and AST node corresponding to a structure
     * @param qualifier used to uniquely identify nodes
     * @param logicalLevelNumber the logical level of that structure
     * @param minorStructures an array of structures organized by depth in the AST
     */
    private void addMinorStructures(
            final TreeAdaptor adaptor,
            final Object node,
            final String qualifier,
            final int logicalLevelNumber,
            final List < List < MinorStructure > > minorStructures) {
        List < MinorStructure > siblingStructures = minorStructures.get(logicalLevelNumber - 1);
        PLIDataItem dataItem = new PLIDataItem(node, qualifier);
        MinorStructure minorStructure = new MinorStructure(
                dataItem, logicalLevelNumber, node);
        if (_log.isDebugEnabled()) {
            _log.debug("Adding minor structure: " + minorStructure);
        }
        siblingStructures.add(minorStructure);
        int n = adaptor.getChildCount(node);
        String childQualifier = minorStructure.getDataItem().getQualifiedName();
        int childsLogicalLevelNumber = logicalLevelNumber + 1;
        for (int i = 0; i < n; i++) {
            Object childNode = adaptor.getChild(node, i);
            if (adaptor.getType(childNode) == PLIStructureParser.DATA_ITEM) {
                PLIDataItem childItem = new PLIDataItem(childNode, childQualifier);
                if (childItem.isGroup()) {
                    addMinorStructures(adaptor, childNode, childQualifier, childsLogicalLevelNumber, minorStructures);
                }
            }
        }
    }

    /**
     * Processes minor structures starting by the deepest. When all minor structures at
     * the deepest level have been processed, the algorithm climbs a level up and processes
     * all minor structures at that level and so forth.
     * 
     * @param adaptor the tree navigator helper
     * @param minorStructures an array of structures organized by depth in the AST
     * @param paddingNodes nodes before which padding must be added and amount of padding
     * @return a mapping unit built from all the structure children data items
     */
    public StructureMappingUnit mapStructures(
            final TreeAdaptor adaptor,
            final List < List < MinorStructure > > minorStructures,
            final Map < Object, Integer> paddingNodes) {
        Map < String, StructureMappingUnit> structureMappingUnits =
            new HashMap < String, StructureMappingUnit>();
        StructureMappingUnit structureMappingUnit = null;
        for (int i = minorStructures.size(); i > 0; i--) {
            for (MinorStructure minorStructure : minorStructures.get(i - 1)) {
                structureMappingUnit = mapMinorStructure(
                        adaptor, minorStructure, structureMappingUnits, paddingNodes);
                structureMappingUnits.put(minorStructure.getDataItem().getQualifiedName(),
                        structureMappingUnit);
            }
        }
        /* The last mapping unit evaluated aggregates all children*/
        return structureMappingUnit;

    }

    /**
     * A minor structure is mapped starting by the first child and then successively pairing with
     * each sibling in turn.
     * <p/>
     * It is assumed children which are minor structures were already mapped and a corresponding
     * mapping unit is available from structureMappingUnits.
     * <p/>
     * When padding is necessary between 2 elements of a pair, this will store the node before
     * which the padding must be inserted and the amount of padding necessary for later processing.
     * We don't want to insert nodes in the AST at this stage because that would interfere with
     * navigation.
     * @param adaptor the tree navigator helper
     * @param minorStructure the minor structure to be mapped
     * @param structureMappingUnits mapping units from deeper minor structures
     * @param paddingNodes nodes before which padding must be added and amount of padding
     * @return a mapping unit built from all the structure children data items
     */
    public StructureMappingUnit mapMinorStructure(
            final TreeAdaptor adaptor,
            final MinorStructure minorStructure,
            final Map < String, StructureMappingUnit> structureMappingUnits,
            final Map < Object, Integer> paddingNodes) {
        Object minorStructureNode = minorStructure.getNode();
        int n = adaptor.getChildCount(minorStructureNode);
        Object firstChildNode = null;
        StructureMappingUnit structureMappingUnit = null;
        String childQualifier = minorStructure.getDataItem().getQualifiedName();
        for (int i = 0; i < n; i++) {
            Object childNode = adaptor.getChild(minorStructureNode, i);
            if (adaptor.getType(childNode) == PLIStructureParser.DATA_ITEM) {
                if (firstChildNode == null) {
                    firstChildNode = childNode;
                    PLIDataItem firstDataItem = new PLIDataItem(adaptor, firstChildNode, childQualifier);
                    structureMappingUnit = new StructureMappingUnit(
                            getMappable(firstDataItem, structureMappingUnits));
                } else {
                    PLIDataItem childDataItem = new PLIDataItem(adaptor, childNode, childQualifier);
                    structureMappingUnit = new StructureMappingUnit(
                            structureMappingUnit, getMappable(childDataItem, structureMappingUnits));
                    if (structureMappingUnit.getPadding() > 0) {
                        paddingNodes.put(childNode, structureMappingUnit.getPadding());
                    }
                }
            }
        }
        /* Rename the mapping  unit after the structure rather than concatenation of inner items names*/
        return new StructureMappingUnit(minorStructure.getDataItem().getName(), structureMappingUnit);
    }

    /**
     * Padding nodes are inserted in the Abstract Syntax Tree.
     * <p/>
     * The paddingNodes map contain nodes which serve as insertion guides. The new padding
     * nodes need to be inserted right before such insertion nodes.
     * <p/>
     * Starting at the padding index, all children are shifted one position to make room
     * for the new padding node. 
     * @param adaptor the tree navigator helper
     * @param paddingNodes the insertion nodes associated with padding amount
     * @throws StructureMappingException if padding nodes cannot be created
     */
    public void addPaddingNodes(
            final TreeAdaptor adaptor,
            final Map < Object, Integer> paddingNodes) throws StructureMappingException {
        for (Map.Entry < Object, Integer> entry : paddingNodes.entrySet()) {
            addPaddingNode(adaptor, entry.getKey(), entry.getValue());
        }
    }
    
    /**
     * Add an initial filler to map the PL/I hang which is a number of bytes
     * that PL/I offsets before the start of the actual data in a structure.
     * <p/>
     * Hang node is added before the first child node of the structure.
     * @param adaptor the tree navigator helper
     * @param ast a non nil abstract syntax tree node representing the structure
     * @param offset the offset amount, i.e. the hang
     * @throws StructureMappingException if adding hang node fails
     */
    public void addHangNode(
            final TreeAdaptor adaptor,
            final CommonTree ast,
            final int offset) throws StructureMappingException {
        if (offset > 0) {
            Object insertionNode = ast.getFirstChildWithType(PLIStructureParser.DATA_ITEM);
            if (insertionNode != null) {
                addPaddingNode(adaptor, insertionNode, offset);
            }
        }
    }
    
    /**
     * Adds a new padding node right before an insertion node.
     * @param adaptor the tree navigator helper
     * @param insertionNode the node before which the padding node must be added
     * @param padding the padding amount
     * @throws StructureMappingException if adding padding node fails
     */
    @SuppressWarnings("unchecked")
    private void addPaddingNode(
            final TreeAdaptor adaptor,
            final Object insertionNode,
            final int padding) throws StructureMappingException {
        PLIDataItem insertionItem = new PLIDataItem(insertionNode, null);
        Object paddingNode = createPaddingNode(insertionItem.getLevel(), padding);
        Object parentNode = adaptor.getParent(insertionNode);
        int paddingIndex = adaptor.getChildIndex(insertionNode);
        CommonTree tree = (CommonTree) parentNode;
        tree.getChildren().add(paddingIndex, paddingNode);
        tree.freshenParentAndChildIndexes();
    }

    /**
     * Create an Abstract Syntax Tree node corresponding to padding characters.
     * @param physicalLevel the level this padding item should exhibit
     * @param padding the amount of padding
     * @return a node ready to be inserted in the AST
     * @throws StructureMappingException if node cannot be created
     */
    private Object createPaddingNode(
            final int physicalLevel,
            final int padding) throws StructureMappingException {

        try {
            String source = String.format("dcl %1$d * char(%2$d);", physicalLevel, padding);
            PLIStructureLexer lexer = new PLIStructureLexerImpl(
                    new ANTLRNoCaseReaderStream(new StringReader(source)));
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            PLIStructureParser parser = new PLIStructureParserImpl(tokens);
            pl1code_return parserResult = parser.pl1code();
            return parserResult.getTree();
        } catch (IOException e) {
            throw new StructureMappingException(e);
        } catch (RecognitionException e) {
            throw new StructureMappingException(e);
        }
    }


    /**
     * Elementary data items are mappable but minor structures are not. For minor structures,
     * we fetch the corresponding mapping unit assumed to have been evaluated already.
     * <p/>
     * Minor structures are identified by their qualified names to prevent name conflicts.
     * @param dataItem the data item to map
     * @param structureMappingUnits mapping units from minor structures
     * @return a mappable unit (the item itself or a previous mapping unit).
     */
    private IMappable getMappable(
            final PLIDataItem dataItem,
            final Map < String, StructureMappingUnit> structureMappingUnits) {
        if (dataItem.isGroup()) {
            return structureMappingUnits.get(dataItem.getQualifiedName());
        } else {
            return dataItem;
        }
    }

    /**
     * @return the execution parameters for the PL/I to COBOL utility
     */
    public Pli2CobContext getContext() {
        return _context;
    }


}
