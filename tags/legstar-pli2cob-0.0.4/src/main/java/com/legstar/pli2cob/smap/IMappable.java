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
