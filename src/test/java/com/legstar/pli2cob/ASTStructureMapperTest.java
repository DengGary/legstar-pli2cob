package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;

import com.legstar.pli2cob.ASTStructureMapper.MinorStructure;


/**
 * Test the ASTStructureMapper class.
 *
 */
public class ASTStructureMapperTest extends AbstractTester {
    
    public void testSimple1() {
        CommonTree ast = parseAndNormalize("Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);");
        ASTStructureMapper mapper = new ASTStructureMapper();
        mapper.map(ast);
    }

    /**
     * Test ability to find the deepest minor structure.
     */
    public void testFindDeepestMinorStructure() {
        assertEquals("Structure. Logical level=4 Item=[level : 5, name : C, aligned : true] Node=DATA_ITEM",
                findDeespestMinorStructure("dcl 1 A, 4 B, 5 C, 5 D, 3 E, 8 F, 7 G;"));

        assertEquals("Structure. Logical level=0 Item=null Node=null",
                findDeespestMinorStructure("Declare 4 Name char(15);"));

        assertEquals("Structure. Logical level=5 Item=[level : 4, name : G, aligned : true] Node=DATA_ITEM",
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

        assertEquals("Structure. Logical level=5 Item=[level : 4, name : G, aligned : true] Node=DATA_ITEM",
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
    
    
    public void testOrderPairing() {
        orderPairing("dcl 1 A, 2 B, 3 C char(1), 3 D char(1), 3 E char(1), 2 F, 3 G char(1), 3 H char(1);");
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
                new CommonTreeAdaptor(), ast, 1, mapper.new MinorStructure()).toString();
    }

    private void orderPairing(final String source) {
        CommonTree ast = parseAndNormalize(source);
        ASTStructureMapper mapper = new ASTStructureMapper();
        TreeAdaptor adaptor = new CommonTreeAdaptor();
        MinorStructure deepestStructure = mapper.findDeepestMinorStructure(
                adaptor, ast, 1, mapper.new MinorStructure());
        mapper.OrderPairing(adaptor, deepestStructure);
    }
    
}
