package com.legstar.pli2cob.smap;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.Tree;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.AbstractTester;
import com.legstar.pli2cob.model.PLIDataItem;


/**
 * Test the ASTStructureMapper class.
 *
 */
public class ASTStructureMapperTest extends AbstractTester {

    /**
     * Test ability to find the deepest minor structure.
     */
    public void testFindDeepestMinorStructure() {
        assertEquals("Structure. Logical level=4"
                + " Item=[level : 5, name : C, qualifiedName : A.B.C, aligned : true]"
                + " Node=DATA_ITEM",
                findDeespestMinorStructure("dcl 1 A, 4 B, 5 C, 5 D, 3 E, 8 F, 7 G;"));

        assertEquals("Structure. Logical level=0 Item=null Node=null",
                findDeespestMinorStructure("Declare 4 Name char(15);"));

        assertEquals("Structure. Logical level=5"
                + " Item=[level : 4, name : G, qualifiedName : A.C.E.G, aligned : true]"
                + " Node=DATA_ITEM",
                findDeespestMinorStructure(
                        "declare 1 A aligned,"
                        + " 2 B fixed bin(31),"
                        + " 2 C,"
                        + " 3 D float decimal(14),"
                        + " 3 E,"
                        + " 4 F char(8),"
                        + " 4 G,"
                        + " 5 H character(2),"
                        + " 5 I float decimal(13),"
                        + " 4 J fixed binary(31,0),"
                        + " 3 K character(2),"
                        + " 3 L fixed binary(20,0),"
                        + " 2 M,"
                        + " 3 N,"
                        + " 4 P fixed binary(15),"
                        + " 4 Q character(5),"
                        + " 4 R float decimal(2),"
                        + " 3 S,"
                        + " 4 T float decimal(15),"
                        + " 4 U bit(3),"
                        + " 4 V char(1),"
                        + " 3 W fixed bin(31),"
                        + " 2 X picture '$9V99';"));

        assertEquals("Structure. Logical level=5"
                + " Item=[level : 4, name : G, qualifiedName : A.C.E.G, aligned : true]"
                + " Node=DATA_ITEM",
                findDeespestMinorStructure(
                        "declare 1 A aligned,"
                        + " 2 B fixed bin(31),"
                        + " 2 C,"
                        + " 3 D float decimal(14),"
                        + " 3 E,"
                        + " 4 F char(8),"
                        + " 4 G,"
                        + " 5 H character(2),"
                        + " 5 I float decimal(13),"
                        + " 4 J fixed binary(31,0),"
                        + " 3 K character(2),"
                        + " 3 L fixed binary(20,0),"
                        + " 2 M,"
                        + " 18 N,"
                        + " 19 P fixed binary(15),"
                        + " 19 Q character(5),"
                        + " 19 R float decimal(2),"
                        + " 3 S,"
                        + " 4 T float decimal(15),"
                        + " 4 U bit(3),"
                        + " 4 V char(1),"
                        + " 3 W fixed bin(31),"
                        + " 2 X picture '$9V99';"));
    }


    /**
     * Cases where there are no minor structures.
     */
    public void testClassifyNoMinorStructures() {
        assertEquals("[]", classifyMinorStructures("Declare 1 Name char(15);").toString());
        assertEquals("[[Structure. Logical level=1"
                + " Item=[level : 1, name : Payroll, qualifiedName : Payroll, aligned : true]"
                + " Node=DATA_ITEM]]",
                classifyMinorStructures("Declare 1 Payroll, 4 Name char(15);").toString());
    }

    /**
     * Single branch structure.
     */
    public void testSingleBranch() {
        assertEquals("["
                + "[Structure. Logical level=1"
                + " Item=[level : 1, name : A, qualifiedName : A, aligned : true] Node=DATA_ITEM],"
                + " [Structure. Logical level=2"
                + " Item=[level : 4, name : B, qualifiedName : A.B, aligned : true] Node=DATA_ITEM,"
                + " Structure. Logical level=2"
                + " Item=[level : 3, name : E, qualifiedName : A.E, aligned : true] Node=DATA_ITEM],"
                + " [Structure. Logical level=3"
                + " Item=[level : 5, name : C, qualifiedName : A.B.C, aligned : true] Node=DATA_ITEM,"
                + " Structure. Logical level=3"
                + " Item=[level : 5, name : D, qualifiedName : A.B.D, aligned : true] Node=DATA_ITEM,"
                + " Structure. Logical level=3"
                + " Item=[level : 8, name : F, qualifiedName : A.E.F, aligned : true] Node=DATA_ITEM,"
                + " Structure. Logical level=3"
                + " Item=[level : 7, name : G, qualifiedName : A.E.G, aligned : true] Node=DATA_ITEM]]",
                classifyMinorStructures("dcl 1 A, 4 B, 5 C, 5 D, 3 E, 8 F, 7 G;").toString());
    }


    /**
     * Map a structure with a single item.
     */
    public void testStructureWithSingleItem() {
        assertEquals("[name : client, alignment : BYTE, offset : 0, length : 15, padding : 0]",
                mapMinorStructure("dcl 1 client, 2 Name char(15);"));
    }

    /**
     * Map a structure with a two item.
     */
    public void testStructureWithTwoItems() {
        assertEquals("[name : A, alignment : HALFWORD, offset : 1, length : 3, padding : 0]",
                mapMinorStructure("dcl 1 A, 2 B char(1), 2 C bin fixed(15);"));
    }

    /**
     * Map a structure with a multiple item.
     */
    public void testStructureWithMultipleItems() {
        assertEquals("[name : A, alignment : FULLWORD, offset : 3, length : 13, padding : 0]",
                mapMinorStructure("dcl 1 A,"
                        + " 2 B char(1),"
                        + " 2 C float decimal(2),"
                        + " 2 D character(5),"
                        + " 2 E bin fixed(15);"));
    }

    /**
     * Try an elementary data item.
     */
    public void testMapStructuresSElementary() {
        assertEquals("No minor structures",
                mapStructures("dcl 1 Name char(15);"));
    }

    /**
     * Try a major structure without any minor structure.
     */
    public void testMapStructuresSingleLevel() {
        assertEquals("[name : A, alignment : BYTE, offset : 0, length : 15, padding : 0]",
                mapStructures("dcl 1 A, 2 B char(15);"));
    }

    /**
     * Try a major structure with multiple elementary items.
     */
    public void testMapStructuresMultipleElementary() {
        assertEquals("[name : A, alignment : FULLWORD, offset : 3, length : 13, padding : 0]",
                mapStructures("dcl 1 A,"
                        + " 2 B char(1),"
                        + " 2 C float decimal(2),"
                        + " 2 D character(5),"
                        + " 2 E bin fixed(15);"));
    }

    /**
     * Try a major structure with a single minor structure.
     */
    public void testMapStructuresSingleMinor() {
        assertEquals("[name : A, alignment : HALFWORD, offset : 2, length : 8, padding : 0]",
                mapStructures("dcl 1 A,"
                        + " 2 B char(1),"
                        + " 2 C,"
                        + " 3 D character(5),"
                        + " 3 E bin fixed(15);"));
    }

    /**
     * Try a major structure with a multiple minor structures.
     */
    public void testMapStructuresMultipleMinor() {
        assertEquals("[name : A, alignment : DOUBLEWORD, offset : 4, length : 80, padding : 0]",
                mapStructures("declare 1 A aligned,"
                        + " 2 B fixed bin(31),"
                        + " 2 C,"
                        + " 3 D float decimal(14),"
                        + " 3 E,"
                        + " 4 F(2) fixed binary(31),"
                        + " 4 G,"
                        + " 5 H character(2),"
                        + " 5 I float decimal(13),"
                        + " 4 J fixed binary(31,0),"
                        + " 3 K character(2),"
                        + " 3 L fixed binary(20,0),"
                        + " 2 M,"
                        + " 3 N,"
                        + " 4 P fixed binary(15),"
                        + " 4 Q character(5),"
                        + " 4 R float decimal(2),"
                        + " 3 S,"
                        + " 4 T float decimal(15),"
                        + " 4 U bit(3),"
                        + " 4 V char(1),"
                        + " 3 W fixed bin(31),"
                        + " 2 X picture '$9V99';"));
    }

    /**
     * Test that there are no paddig nodes when there is no padding.
     */
    public void testNoPaddingNodes() {

        Map < Object, Integer> paddingNodes;

        paddingNodes = getPaddingNodes("dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C,"
                + " 3 D character(5),"
                + " 3 E bin fixed(15);");
        assertTrue(paddingNodes.isEmpty());
    }

    /**
     * Test that a single padding node is found.
     */
    public void testOnePaddingNode() {

        Map < Object, Integer> paddingNodes;

        paddingNodes = getPaddingNodes("dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15),"
                + " 2 D char(1),"
                + " 2 E fixed bin(31);");
        for (Map.Entry < Object, Integer> entry : paddingNodes.entrySet()) {
            assertEquals("[level : 2,"
                    + " name : E,"
                    + " qualifiedName : E,"
                    + " scale : FIXED,"
                    + " base : BINARY,"
                    + " signed : true,"
                    + " precision : 31,"
                    + " scaling factor : 0,"
                    + " length : 4,"
                    + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
            assertEquals("1", entry.getValue().toString());
        }
    }

    /**
     * Test that a single padding node is found case 2.
     */
    public void testOnePaddingNodeCase2() {

        Map < Object, Integer> paddingNodes;

        paddingNodes = getPaddingNodes("dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15),"
                + " 2 D,"
                + " 3 E char(1),"
                + " 3 F fixed bin(31);");
        for (Map.Entry < Object, Integer> entry : paddingNodes.entrySet()) {
            assertEquals("[level : 2,"
                    + " name : D,"
                    + " qualifiedName : D,"
                    + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
            assertEquals("1", entry.getValue().toString());
        }
    }

    /**
     * Test that multiple padding nodes are found.
     */
    public void testMultiplePaddingNodes() {

        Map < Object, Integer> paddingNodes;

        paddingNodes = getPaddingNodes("declare 1 A aligned,"
                + " 2 B fixed bin(31),"
                + " 2 C,"
                + " 3 D float decimal(14),"
                + " 3 E,"
                + " 4 F(2) fixed binary(31),"
                + " 4 G,"
                + " 5 H character(2),"
                + " 5 I float decimal(13),"
                + " 4 J fixed binary(31,0),"
                + " 3 K character(2),"
                + " 3 L fixed binary(20,0),"
                + " 2 M,"
                + " 3 N,"
                + " 4 P fixed binary(15),"
                + " 4 Q character(5),"
                + " 4 R float decimal(2),"
                + " 3 S,"
                + " 4 T float decimal(15),"
                + " 4 U bit(3),"
                + " 4 V char(1),"
                + " 3 W fixed bin(31),"
                + " 2 X picture '$9V99';");
        int order = 0;
        for (Map.Entry < Object, Integer> entry : paddingNodes.entrySet()) {
            switch (order) {
            case 0:
                assertEquals("[level : 4,"
                        + " name : G,"
                        + " qualifiedName : G,"
                        + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
                assertEquals("2", entry.getValue().toString());
                break;
            case 1:
                assertEquals("[level : 4,"
                        + " name : R,"
                        + " qualifiedName : R,"
                        + " scale : FLOAT,"
                        + " base : DECIMAL,"
                        + " signed : true,"
                        + " precision : 2,"
                        + " scaling factor : 0,"
                        + " length : 4,"
                        + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
                assertEquals("1", entry.getValue().toString());
                break;
            case 2:
                assertEquals("[level : 3,"
                        + " name : E,"
                        + " qualifiedName : E,"
                        + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
                assertEquals("4", entry.getValue().toString());
                break;
            case 3:
                assertEquals("[level : 3,"
                        + " name : L,"
                        + " qualifiedName : L,"
                        + " scale : FIXED,"
                        + " base : BINARY,"
                        + " signed : true,"
                        + " precision : 20,"
                        + " scaling factor : 0,"
                        + " length : 4,"
                        + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
                assertEquals("2", entry.getValue().toString());
                break;
            case 4:
                assertEquals("[level : 3,"
                        + " name : W,"
                        + " qualifiedName : W,"
                        + " scale : FIXED,"
                        + " base : BINARY,"
                        + " signed : true,"
                        + " precision : 31,"
                        + " scaling factor : 0,"
                        + " length : 4,"
                        + " aligned : true]", (new PLIDataItem(entry.getKey(), null)).toString());
                assertEquals("2", entry.getValue().toString());
                break;
            default:
                fail();
            }
            order++;
        }
    }

    /**
     * A simple structure with a padding node and no hang.
     */
    public void testMappingSimpleStructure() {
        mapAndCheck("dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15),"
                + " 2 D,"
                + " 3 E char(1),"
                + " 3 F fixed bin(31);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"A\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"B\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
                + "  n11 -> n12 // \"STRING\" -> \"CHARACTER\""
                + "  n11 -> n13 // \"STRING\" -> \"LENGTH\""
                + "  n13 -> n14 // \"LENGTH\" -> \"1\""
                + "  n1 -> n15 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n16 -> n17 // \"LEVEL\" -> \"2\""
                + "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
                + "  n18 -> n19 // \"NAME\" -> \"C\""
                + "  n15 -> n20 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n20 -> n21 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n20 -> n22 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n20 -> n23 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n23 -> n24 // \"PRECISION\" -> \"15\""
                + "  n1 -> n25 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n25 -> n26 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n26 -> n27 // \"LEVEL\" -> \"2\""
                + "  n25 -> n28 // \"DATA_ITEM\" -> \"NAME\""
                + "  n28 -> n29 // \"NAME\" -> \"FILLER\""
                + "  n25 -> n30 // \"DATA_ITEM\" -> \"STRING\""
                + "  n30 -> n31 // \"STRING\" -> \"CHARACTER\""
                + "  n30 -> n32 // \"STRING\" -> \"LENGTH\""
                + "  n32 -> n33 // \"LENGTH\" -> \"1\""
                + "  n1 -> n34 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n34 -> n35 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n35 -> n36 // \"LEVEL\" -> \"2\""
                + "  n34 -> n37 // \"DATA_ITEM\" -> \"NAME\""
                + "  n37 -> n38 // \"NAME\" -> \"D\""
                + "  n34 -> n39 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n39 -> n40 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n40 -> n41 // \"LEVEL\" -> \"3\""
                + "  n39 -> n42 // \"DATA_ITEM\" -> \"NAME\""
                + "  n42 -> n43 // \"NAME\" -> \"E\""
                + "  n39 -> n44 // \"DATA_ITEM\" -> \"STRING\""
                + "  n44 -> n45 // \"STRING\" -> \"CHARACTER\""
                + "  n44 -> n46 // \"STRING\" -> \"LENGTH\""
                + "  n46 -> n47 // \"LENGTH\" -> \"1\""
                + "  n34 -> n48 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n48 -> n49 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n49 -> n50 // \"LEVEL\" -> \"3\""
                + "  n48 -> n51 // \"DATA_ITEM\" -> \"NAME\""
                + "  n51 -> n52 // \"NAME\" -> \"F\""
                + "  n48 -> n53 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n53 -> n54 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n53 -> n55 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n53 -> n56 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n56 -> n57 // \"PRECISION\" -> \"31\"",

                false);
    }

    /**
     * A simple structure with a hang.
     */
    public void testMappingSimpleStructureWithHang() {
        mapAndCheck("dcl 1 A,"
                + " 2 B char(1),"
                + " 2 C fixed bin(15);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"A\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"FILLER\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
                + "  n11 -> n12 // \"STRING\" -> \"CHARACTER\""
                + "  n11 -> n13 // \"STRING\" -> \"LENGTH\""
                + "  n13 -> n14 // \"LENGTH\" -> \"1\""
                + "  n1 -> n15 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n16 -> n17 // \"LEVEL\" -> \"2\""
                + "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
                + "  n18 -> n19 // \"NAME\" -> \"B\""
                + "  n15 -> n20 // \"DATA_ITEM\" -> \"STRING\""
                + "  n20 -> n21 // \"STRING\" -> \"CHARACTER\""
                + "  n20 -> n22 // \"STRING\" -> \"LENGTH\""
                + "  n22 -> n23 // \"LENGTH\" -> \"1\""
                + "  n1 -> n24 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n24 -> n25 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n25 -> n26 // \"LEVEL\" -> \"2\""
                + "  n24 -> n27 // \"DATA_ITEM\" -> \"NAME\""
                + "  n27 -> n28 // \"NAME\" -> \"C\""
                + "  n24 -> n29 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n29 -> n30 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n29 -> n31 // \"ARITHMETIC\" -> \"BINARY\""
                + "  n29 -> n32 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n32 -> n33 // \"PRECISION\" -> \"15\"",

                true);
    }

    /**
     * Generic helper to find deepest minor structure.
     * @param source assumed to contain a single declare statement
     * @return the deepest minor structure found
     */
    private String findDeespestMinorStructure(final String source) {
        CommonTree ast = parseAndNormalize(source);
        ASTStructureMapper mapper = new ASTStructureMapper();
        return mapper.findDeepestMinorStructure(
                new CommonTreeAdaptor(), ast, null, 1, new MinorStructure()).toString();
    }

    /**
     * Generic helper to classify minor structures.
     * @param source assumed to contain a single declare statement
     * @return an array for minor structures organized by depth
     */
    private List < List < MinorStructure > > classifyMinorStructures(final String source) {
        Tree ast = parseAndNormalize(source);
        if (ast.isNil()) {
            ast = ast.getChild(0);
        }
        ASTStructureMapper mapper = new ASTStructureMapper();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = mapper.findDeepestMinorStructure(
                adaptor, ast, null, 1, new MinorStructure());
        return mapper.classifyStructures(adaptor, ast, null, deepestStructure.getLogicalLevel());
    }

    /**
     * Generic helper to map a minor structure.
     * @param source assumed to describe at least one minor structure
     * @return the mapping unit created
     */
    private String mapMinorStructure(final String source) {
        Tree ast = parseAndNormalize(source);
        if (ast.isNil()) {
            ast = ast.getChild(0);
        }
        ASTStructureMapper mapper = new ASTStructureMapper();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = mapper.findDeepestMinorStructure(
                adaptor, ast, null, 1, new MinorStructure());
        return mapper.mapMinorStructure(
                adaptor,
                deepestStructure,
                new HashMap < String, StructureMappingUnit>(),
                new HashMap < Object, Integer>()).toString();
    }

    /**
     * Generic helper to map all minor structures for a major structure.
     * @param source an array for minor structures organized by depth
     * @return the aggregate mapping unit created
     */
    private String mapStructures(final String source) {
        Tree ast = parseAndNormalize(source);
        if (ast.isNil()) {
            ast = ast.getChild(0);
        }
        ASTStructureMapper mapper = new ASTStructureMapper();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = mapper.findDeepestMinorStructure(
                adaptor, ast, null, 1, new MinorStructure());
        if (deepestStructure.isNil()) {
            return "No minor structures";
        }
        List < List < MinorStructure > > minorStructures = mapper.classifyStructures(
                adaptor, ast, null, deepestStructure.getLogicalLevel());
        Map < Object, Integer> paddingNodes = new LinkedHashMap < Object, Integer>();
        return mapper.mapStructures(adaptor, minorStructures, paddingNodes).toString();
    }

    /**
     * Generic helper to map all minor structures for a major structure.
     * @param source a PLI source fragment
     * @return the padding nodes info
     */
    private Map < Object, Integer> getPaddingNodes(final String source) {
        Tree ast = parseAndNormalize(source);
        if (ast.isNil()) {
            ast = ast.getChild(0);
        }
        ASTStructureMapper mapper = new ASTStructureMapper();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = mapper.findDeepestMinorStructure(
                adaptor, ast, null, 1, new MinorStructure());
        Map < Object, Integer> paddingNodes = new LinkedHashMap < Object, Integer>();
        if (deepestStructure.isNil()) {
            return paddingNodes;
        }
        List < List < MinorStructure > > minorStructures = mapper.classifyStructures(
                adaptor, ast, null, deepestStructure.getLogicalLevel());
        mapper.mapStructures(adaptor, minorStructures, paddingNodes);
        return paddingNodes;
    }

    /**
     * Generic helper to perform the complete structure mapping including
     * inserting passing nodes into the abstract syntax tree.
     * @param source a PLI source fragment
     * @param expected the expected AST graph
     * @param synchang true if padding should be added for PL/I hang
     */
    private void mapAndCheck(final String source,
            final String expected,
            final boolean synchang) {
        CommonTree ast = parseAndNormalize(source);
        try {
            ASTStructureMapper mapper = new ASTStructureMapper();
            mapper.getContext().setSynchang(synchang);
            mapper.map(ast);
            String graph = getGraph(ast);
            assertEquals(expected, getSubGraph(graph));
        } catch (StructureMappingException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
}
