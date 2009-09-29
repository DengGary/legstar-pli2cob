package com.legstar.pli2cob.smap;

import com.legstar.pli2cob.model.PLIDataItem;

/**
 * Mappable elements are elementary data items or units resulting from
 * pairing data items.
 *
 */
public interface IMappable {

    /**
     * @return element name
     */
    String getName();
    
    /**
     * @return the alignment requirement of this unit
     */
    PLIDataItem.AlignmentRequirement getAlignmentRequirement();
    /**
     * @return the size in bytes of this unit
     */
    int getByteLength();

    /**
     * @return the offset of this unit from a doubleword boundary
     */
    int getOffset();

}
