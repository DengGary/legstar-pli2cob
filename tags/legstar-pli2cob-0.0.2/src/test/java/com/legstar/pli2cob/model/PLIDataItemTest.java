package com.legstar.pli2cob.model;

import org.antlr.runtime.tree.CommonTree;

import com.legstar.pli2cob.AbstractTester;
import com.legstar.pli2cob.model.PLIDataItem.AlignmentRequirement;


/**
 * Test the PLIDataItem class.
 *
 */
public class PLIDataItemTest extends AbstractTester {

    /**
     * Check that numeric length is calculated correctly.
     */
    public void testNumericLength() {
        
        parseCheck("dcl 1 A bin fixed(5,2);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : BINARY,"
                + " signed : true,"
                + " precision : 5,"
                + " scaling factor : 2,"
                + " length : 1,"
                + " aligned : true]");

        parseCheck("dcl 1 A bin fixed(16,2) unsigned;",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : BINARY,"
                + " signed : false,"
                + " precision : 16,"
                + " scaling factor : 2,"
                + " length : 2,"
                + " aligned : true]");

        parseCheck("dcl 1 A dec fixed(16,2);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : DECIMAL,"
                + " signed : true,"
                + " precision : 16,"
                + " scaling factor : 2,"
                + " length : 9,"
                + " aligned : true]");

        parseCheck("dcl 1 A dec float(3);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FLOAT,"
                + " base : DECIMAL,"
                + " signed : true,"
                + " precision : 3,"
                + " scaling factor : 0,"
                + " length : 4,"
                + " aligned : true]");

        parseCheck("dcl 1 A bin float(42);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FLOAT,"
                + " base : BINARY,"
                + " signed : true,"
                + " precision : 42,"
                + " scaling factor : 0,"
                + " length : 8,"
                + " aligned : true]");
    }
    
    /**
     * Check explicit and default alignment attributes.
     */
    public void testAligned() {
        parseCheck("dcl 1 A char(1);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " stringType : CHARACTER,"
                + " length : 1,"
                + " varying : NONVARYING,"
                + " aligned : false]");

        parseCheck("dcl 1 A char(1) aligned;",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " stringType : CHARACTER,"
                + " length : 1,"
                + " varying : NONVARYING,"
                + " aligned : true]");

        parseCheck("dcl 1 A bin fixed(5,2);",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : BINARY,"
                + " signed : true,"
                + " precision : 5,"
                + " scaling factor : 2,"
                + " length : 1,"
                + " aligned : true]");

        parseCheck("dcl 1 A bin fixed(5,2) aligned;",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : BINARY,"
                + " signed : true,"
                + " precision : 5,"
                + " scaling factor : 2,"
                + " length : 1,"
                + " aligned : true]");

        parseCheck("dcl 1 A bin fixed(5,2) unaligned;",
                "[level : 1,"
                + " name : A,"
                + " qualifiedName : parent.A,"
                + " scale : FIXED,"
                + " base : BINARY,"
                + " signed : true,"
                + " precision : 5,"
                + " scaling factor : 2,"
                + " length : 1,"
                + " aligned : false]");
    }
    
    /**
     * Check that alignment requirement is derived properly from other attributes.
     */
    public void testAlignmentRequirement() {
        
        PLIDataItem dataItem;
        
        dataItem = parseItem("dcl 1 A bit(1);");
        assertEquals(AlignmentRequirement.BIT, dataItem.getAlignmentRequirement());
        
        dataItem = parseItem("dcl 1 A bit(1) aligned;");
        assertEquals(AlignmentRequirement.BYTE, dataItem.getAlignmentRequirement());
        
        dataItem = parseItem("dcl 1 A char(1);");
        assertEquals(AlignmentRequirement.BYTE, dataItem.getAlignmentRequirement());
        
        dataItem = parseItem("dcl 1 A bin fixed(15);");
        assertEquals(AlignmentRequirement.HALFWORD, dataItem.getAlignmentRequirement());
        
        dataItem = parseItem("dcl 1 A bin fixed(15) unaligned;");
        assertEquals(AlignmentRequirement.BYTE, dataItem.getAlignmentRequirement());
    }
    
    /**
     * Check that we know how to calculate array sizes.
     */
    public void testArrayLength() {
        
        PLIDataItem dataItem;
        
        dataItem = parseItem("dcl 1 A char(1);");
        assertEquals(1, dataItem.getLength());
        
        dataItem = parseItem("dcl 1 A(2) char(1);");
        assertEquals(2, dataItem.getLength());
        
        dataItem = parseItem("dcl 1 A(1:2) char(1);");
        assertEquals(2, dataItem.getLength());
        
        dataItem = parseItem("dcl 1 A(2:2) char(1);");
        assertEquals(1, dataItem.getLength());
        
        dataItem = parseItem("dcl 1 A(2 refer(X)) char(1);");
        assertEquals(0, dataItem.getLength());
        
        dataItem = parseItem("dcl 1 A(2,3) char(1);");
        assertEquals(6, dataItem.getLength());
    }

    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected data item
     */
    private void parseCheck(final String source, final String expected) {
        assertEquals(expected, parseItem(source).toString());
    }

    /**
     * A generic test helper that takes a source fragment and produce a data item.
     * @param source the source fragment (assumed to contain a single item declaration)
     * @return a data item
     */
    private PLIDataItem parseItem(final String source) {
        CommonTree ast = parse(source);
        return new PLIDataItem(ast, "parent");
    }
}
