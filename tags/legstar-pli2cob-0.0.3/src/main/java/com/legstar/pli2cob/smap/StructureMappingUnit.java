package com.legstar.pli2cob.smap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.pli2cob.model.PLIDataItem;
import com.legstar.pli2cob.util.ASTUtils;

/**
 * Represents an aggregate of consecutive data items as described in the PLI structure
 * mapping algorithm.
 * <p/>
 *
 */
public class StructureMappingUnit implements IMappable {

    /** Offset of this unit from a doubleword boundary. */
    private int _offset = 0;

    /** Alignment requirement of this unit.*/
    private PLIDataItem.AlignmentRequirement _alignmentRequirement = PLIDataItem.AlignmentRequirement.BYTE;

    /** Size in bytes of this unit. */
    private int _byteLength;

    /** Amount of padding that was added.*/
    private int _padding;
    
    /** An identifier built from the elements pair names.*/
    private String _name;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Constructor used when renaming an existing mapping unit.
     * <p/>
     * Helps keep the class immutable..
     * @param name the mapping unit new name
     * @param mappingUnit the orginal mapping unit
     */
    public StructureMappingUnit(final String name, final IMappable mappingUnit) {
        _offset = mappingUnit.getOffset();
        _alignmentRequirement = mappingUnit.getAlignmentRequirement();
        _byteLength = mappingUnit.getByteLength();
        _padding = 0;
        _name = name;
    }

    /**
     * Constructor used when a mapping unit is built from a single data item.
     * <p/>
     * For instance a structure containing a single elementary item.
     * @param singleElement the single data item
     */
    public StructureMappingUnit(final IMappable singleElement) {
        this(singleElement.getName(), singleElement);
    }

    /**
     * Pairs an element with the next element.
     * <p>
     * After this process has been completed, any padding between the two elements
     * has been minimized and does not change throughout the rest of the operation.
     * The pair is now a unit of fixed length and alignment requirement; its length
     * is the sum of the two lengths plus padding, and its alignment requirement is
     * the higher of the two alignment requirements (if they differ).
     * @param firstElement first data item in the pair
     * @param secondElement second data item in the pair
     */
    public StructureMappingUnit(final IMappable firstElement, final IMappable secondElement) {
        
        _name = "[" + firstElement.getName() + " & " + secondElement.getName() + "]";

        /* Begin the first element of the pair on a doubleword boundary; or,
         * if the element is a minor structure that has already been mapped,
         * offset it from the doubleword boundary by the amount indicated.*/
        int unitLength = firstElement.getOffset() + firstElement.getByteLength();

        /* Begin the second element of the pair at the first valid position
         * following the end of the first element. This position depends on
         * the alignment requirement of the second element. (If the second
         * element is a minor structure, its alignment requirement will
         * have already been determined.) */
        int alignLen = ASTUtils.getLength(secondElement.getAlignmentRequirement());
        _padding = (unitLength % alignLen == 0) ? 0 : alignLen - (unitLength % alignLen);
        
        /* Offset of the second element contributes to padding (it is used for shifting)*/
        _padding += secondElement.getOffset();

        /* Shift the first element towards the second element as far as the
         * alignment requirement of the first allows. The amount of shift
         * determines the offset of this pair from a doubleword boundary.*/
        alignLen = ASTUtils.getLength(firstElement.getAlignmentRequirement());
        _offset = alignLen * (_padding / alignLen);
        _padding -= _offset;
        
        /* contribute the offset of the first element to the unit offset */
        _offset += firstElement.getOffset();
        /* offsets are from doubleword boundaries*/
        _offset = _offset % 8;

        _byteLength = firstElement.getByteLength() + _padding + secondElement.getByteLength();
        
        
        /* A unit alignment requirement is the highest for each pair element */
        if (secondElement.getAlignmentRequirement().ordinal()
                < firstElement.getAlignmentRequirement().ordinal()) {
            _alignmentRequirement = firstElement.getAlignmentRequirement();
        } else {
            _alignmentRequirement = secondElement.getAlignmentRequirement();
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Pairing " + firstElement.getName() + " with " + secondElement.getName());
            _log.debug("Pairing result: " + toString());
        }
    }

    /**
     * @return the offset of this unit from a doubleword boundary
     */
    public int getOffset() {
        return _offset;
    }

    /**
     * @return the alignment requirement of this unit
     */
    public PLIDataItem.AlignmentRequirement getAlignmentRequirement() {
        return _alignmentRequirement;
    }

    /**
     * @return the size in bytes of this unit
     */
    public int getByteLength() {
        return _byteLength;
    }

    /**
     * @return the amount of padding that was added
     */
    public int getPadding() {
        return _padding;
    }

    /**
     * @return an identifier built from the elements pair names
     */
    public String getName() {
        return _name;
    }


    /** {@inheritDoc}*/
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append("name : " + getName());
        sb.append(", ");
        sb.append("alignment : " + getAlignmentRequirement());
        sb.append(", ");
        sb.append("offset : " + getOffset());
        sb.append(", ");
        sb.append("length : " + getByteLength());
        sb.append(", ");
        sb.append("padding : " + getPadding());
        sb.append("]");
        return sb.toString();
    }

}
