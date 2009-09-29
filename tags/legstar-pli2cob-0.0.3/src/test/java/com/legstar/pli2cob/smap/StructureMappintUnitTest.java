package com.legstar.pli2cob.smap;

import com.legstar.pli2cob.AbstractTester;
import com.legstar.pli2cob.model.PLIDataItem.AlignmentRequirement;

/**
 * Test the StructureMappintUnit class.
 *
 */
public class StructureMappintUnitTest extends AbstractTester {
    
    /**
     * Pair 2 byte aligned items.
     */
    public void testByteByte() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A char(1);"),
                getPLIDataItem("dcl 1 B char(1);"));
        assertEquals(AlignmentRequirement.BYTE, unit.getAlignmentRequirement());
        assertEquals(2, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }
    
    /**
     * Pair a byte aligned and a halfword aligned item.
     */
    public void testByteHalfword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A char(1);"),
                getPLIDataItem("dcl 1 B bin fixed(15);"));
        assertEquals(AlignmentRequirement.HALFWORD, unit.getAlignmentRequirement());
        assertEquals(3, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(1, unit.getOffset());
    }

    /**
     * Pair a byte aligned and a fullword aligned item.
     */
    public void testByteFullword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A char(1);"),
                getPLIDataItem("dcl 1 B bin fixed(31);"));
        assertEquals(AlignmentRequirement.FULLWORD, unit.getAlignmentRequirement());
        assertEquals(5, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(3, unit.getOffset());
    }

    /**
     * Pair a byte aligned and a doubleword aligned item.
     */
    public void testByteDoubleword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A char(1);"),
                getPLIDataItem("dcl 1 B bin fixed(63);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit.getAlignmentRequirement());
        assertEquals(9, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(7, unit.getOffset());
    }

    /**
     * Pair a halfword aligned and a byte aligned item.
     */
    public void testHalfwordByte() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(15);"),
                getPLIDataItem("dcl 1 B char(1);"));
        assertEquals(AlignmentRequirement.HALFWORD, unit.getAlignmentRequirement());
        assertEquals(3, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Pair a fullword aligned and a byte aligned item.
     */
    public void testFullwordByte() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(31);"),
                getPLIDataItem("dcl 1 B char(1);"));
        assertEquals(AlignmentRequirement.FULLWORD, unit.getAlignmentRequirement());
        assertEquals(5, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Pair a doubleword aligned and a byte aligned item.
     */
    public void testDoublewordByte() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(63);"),
                getPLIDataItem("dcl 1 B char(1);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit.getAlignmentRequirement());
        assertEquals(9, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Pair a halfword aligned and a halfword aligned item.
     */
    public void testHalfwordHalfword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(15);"),
                getPLIDataItem("dcl 1 B bin fixed(15);"));
        assertEquals(AlignmentRequirement.HALFWORD, unit.getAlignmentRequirement());
        assertEquals(4, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Pair a halfword aligned and a fullword aligned item.
     */
    public void testHalfwordFullword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(15);"),
                getPLIDataItem("dcl 1 B bin fixed(31);"));
        assertEquals(AlignmentRequirement.FULLWORD, unit.getAlignmentRequirement());
        assertEquals(6, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(2, unit.getOffset());
    }

    /**
     * Pair a halfword aligned and a doubleword aligned item.
     */
    public void testHalfwordDoubleword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(15);"),
                getPLIDataItem("dcl 1 B bin fixed(63);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit.getAlignmentRequirement());
        assertEquals(10, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(6, unit.getOffset());
    }

    /**
     * Pair a fullword aligned and a halfword aligned item.
     */
    public void testFullwordHalfword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(31);"),
                getPLIDataItem("dcl 1 B bin fixed(15);"));
        assertEquals(AlignmentRequirement.FULLWORD, unit.getAlignmentRequirement());
        assertEquals(6, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Pair a doubleword aligned and a halfword aligned item.
     */
    public void testDoublewordHalfword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A bin fixed(63);"),
                getPLIDataItem("dcl 1 B bin fixed(15);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit.getAlignmentRequirement());
        assertEquals(10, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(0, unit.getOffset());
    }

    /**
     * Test with sample from PLI programmer reference guide.
     */
    public void testDocSampleG() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 H character(2);"),
                getPLIDataItem("dcl 1 I float decimal(13);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit.getAlignmentRequirement());
        assertEquals(10, unit.getByteLength());
        assertEquals(0, unit.getPadding());
        assertEquals(6, unit.getOffset());
    }
    
    /**
     * Try pairing an elementary item with a composite unit.
     */
    public void testDoublewordUnit() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 H character(2);"),
                getPLIDataItem("dcl 1 I float decimal(13);"));
        StructureMappingUnit unit2 = new StructureMappingUnit(
                getPLIDataItem("dcl 1 F(2) fixed bin(31);"),
                unit);
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit2.getAlignmentRequirement());
        assertEquals(20, unit2.getByteLength());
        assertEquals(2, unit2.getPadding());
        assertEquals(4, unit2.getOffset());
        
    }

    /**
     * Try pairing a composite unit with an elementary item.
     */
    public void testUnitFullword() {
        StructureMappingUnit unit = new StructureMappingUnit(
                getPLIDataItem("dcl 1 H character(2);"),
                getPLIDataItem("dcl 1 I float decimal(13);"));
        StructureMappingUnit unit2 = new StructureMappingUnit(
                getPLIDataItem("dcl 1 F(2) fixed bin(31);"),
                unit);
        StructureMappingUnit unit3 = new StructureMappingUnit(
                unit2,
                getPLIDataItem("dcl 1 J fixed bin(31,0);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unit3.getAlignmentRequirement());
        assertEquals(24, unit3.getByteLength());
        assertEquals(0, unit3.getPadding());
        assertEquals(4, unit3.getOffset());
        
    }

    /**
     * Try another sequence from PLI programmers reference.
     */
    public void testDocSampleM() {
        StructureMappingUnit unitPQ = new StructureMappingUnit(
                getPLIDataItem("dcl 1 P fixed binary(15);"),
                getPLIDataItem("dcl 1 Q character(5);"));
        assertEquals(AlignmentRequirement.HALFWORD, unitPQ.getAlignmentRequirement());
        assertEquals(7, unitPQ.getByteLength());
        assertEquals(0, unitPQ.getPadding());
        assertEquals(0, unitPQ.getOffset());

        StructureMappingUnit unitN = new StructureMappingUnit(
                unitPQ,
                getPLIDataItem("dcl 1 R float decimal(2)"));
        assertEquals(AlignmentRequirement.FULLWORD, unitN.getAlignmentRequirement());
        assertEquals(12, unitN.getByteLength());
        assertEquals(1, unitN.getPadding());
        assertEquals(0, unitN.getOffset());
        
        StructureMappingUnit unitTU = new StructureMappingUnit(
                getPLIDataItem("dcl 1 T float decimal(15);"),
                getPLIDataItem("dcl 1 U bit(3);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unitTU.getAlignmentRequirement());
        assertEquals(9, unitTU.getByteLength());
        assertEquals(0, unitTU.getPadding());
        assertEquals(0, unitTU.getOffset());

        StructureMappingUnit unitS = new StructureMappingUnit(
                unitTU,
                getPLIDataItem("dcl 1 V char(1);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unitS.getAlignmentRequirement());
        assertEquals(10, unitS.getByteLength());
        assertEquals(0, unitS.getPadding());
        assertEquals(0, unitS.getOffset());

        StructureMappingUnit unitNS = new StructureMappingUnit(
                unitN,
                unitS);
        assertEquals(AlignmentRequirement.DOUBLEWORD, unitNS.getAlignmentRequirement());
        assertEquals(22, unitNS.getByteLength());
        assertEquals(0, unitNS.getPadding());
        assertEquals(4, unitNS.getOffset());

        StructureMappingUnit unitM = new StructureMappingUnit(
                unitNS,
                getPLIDataItem("dcl 1 W fixed bin(31);"));
        assertEquals(AlignmentRequirement.DOUBLEWORD, unitM.getAlignmentRequirement());
        assertEquals(28, unitM.getByteLength());
        assertEquals(2, unitM.getPadding());
        assertEquals(4, unitM.getOffset());
    }

    /**
     * Try sample on the wiki.
     */
    public void testWikiSample() {
        StructureMappingUnit unitD = new StructureMappingUnit(
                getPLIDataItem("dcl 1 E char(1);"),
                getPLIDataItem("dcl 1 F fixed bin(31);"));
        assertEquals("[name : [E & F], alignment : FULLWORD, offset : 3, length : 5, padding : 0]", unitD.toString());

        StructureMappingUnit unitBC = new StructureMappingUnit(
                getPLIDataItem("dcl 1 A char(1);"),
                getPLIDataItem("dcl 1 C fixed bin(15);"));
        assertEquals("[name : [A & C], alignment : HALFWORD, offset : 1, length : 3, padding : 0]", unitBC.toString());

        StructureMappingUnit unitA = new StructureMappingUnit(
                unitBC,
                unitD);
        assertEquals("[name : [[A & C] & [E & F]], alignment : FULLWORD, offset : 3, length : 9, padding : 1]",
                unitA.toString());
    }
}
