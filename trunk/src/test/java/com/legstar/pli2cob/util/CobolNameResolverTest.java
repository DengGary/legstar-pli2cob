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
package com.legstar.pli2cob.util;

import junit.framework.TestCase;

/**
 * Test CobolNameResolver. Produces valid COBOL names.
 *
 */
public class CobolNameResolverTest extends TestCase {

    /**
     * Simplest case.
     * @throws Exception if test fails
     */
    public void testSimple() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("var", cnr.getName("var"));
    }

    /**
     * Check that special chars are properly transformed.
     * @throws Exception if test fails
     */
    public void testInvalidChars() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("varTO75-TO01", cnr.getName("9__var@TO75_TO01__"));
    }

    /**
     * Check truncation.
     * @throws Exception if test fails
     */
    public void testTruncation() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("T12345678901234567890123456789", cnr.getName("T123456789012345678901234567890"));
        assertEquals("T1234567890123456789012345678", cnr.getName("T1234567890123456789012345678-0"));
    }

    /**
     * Check reserved words handling.
     * @throws Exception if test fails
     */
    public void testReservedWord() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("R-count", cnr.getName("count"));
    }

    /**
     * Check handling of name conflicts. Hints are different but since they
     * are longer than 30 they would result in the same COBOL name.
     * @throws Exception if test fails
     */
    public void testMakeUnique() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("Var123456789012345678901234567", cnr.getName("Var1234567890123456789012345671"));
        assertEquals("Var123456789012345678901234560", cnr.getName("Var1234567890123456789012345672"));
        assertEquals("Var123456789012345678901234561", cnr.getName("Var1234567890123456789012345673"));
        assertEquals("Var123456789012345678901234567", cnr.getName("Var1234567890123456789012345671"));
    }

    /**
     * Check handling of name conflicts.
     * @throws Exception if test fails
     */
    public void testMakeUniqueWithLongNames() throws Exception {
        CobolNameResolver cnr = new CobolNameResolver();
        assertEquals("T12345678901234567890123456780", cnr.getName("T12345678901234567890123456780"));
        assertEquals("T12345678901234567890123456781", cnr.getName("T12345678901234567890123456781"));
        assertEquals("T12345678901234567890123456782", cnr.getName("T12345678901234567890123456782"));
        assertEquals("T12345678901234567890123456783", cnr.getName("T12345678901234567890123456783"));
        assertEquals("T12345678901234567890123456784", cnr.getName("T12345678901234567890123456784"));
        assertEquals("T12345678901234567890123456785", cnr.getName("T12345678901234567890123456785"));
        assertEquals("T12345678901234567890123456786", cnr.getName("T12345678901234567890123456786"));
        assertEquals("T12345678901234567890123456787", cnr.getName("T12345678901234567890123456787"));
        assertEquals("T12345678901234567890123456788", cnr.getName("T12345678901234567890123456788"));
        assertEquals("T12345678901234567890123456789", cnr.getName("T12345678901234567890123456789"));
        assertEquals("T12345678901234567890123456710", cnr.getName("T12345678901234567890123456780A"));
    }
    
}
