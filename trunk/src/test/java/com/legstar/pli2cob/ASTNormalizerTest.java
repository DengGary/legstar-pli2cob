package com.legstar.pli2cob;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;

/**
 * Test ASTNormalizer class.
 *
 */
public class ASTNormalizerTest extends AbstractTester {

    /**
     * Check what happens if nothing passed.
     */
    public void testInstantiation() {
        ASTNormalizer normalizer = new ASTNormalizer();
        Tree ast = normalizer.normalize(null);
        assertTrue(ast == null);
        ast = normalizer.normalize(parse(""));
        assertTrue(ast == null);
    }

    /**
     * An elementary item (single node tree).
     */
    public void testNotAStructure() {
        normalizeCheck(
                "Declare 1 Last char(20);",

                "  n0 -> n1 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n1 -> n2 // \"LEVEL\" -> \"1\""
                + "  n0 -> n3 // \"DATA_ITEM\" -> \"NAME\""
                + "  n3 -> n4 // \"NAME\" -> \"Last\""
                + "  n0 -> n5 // \"DATA_ITEM\" -> \"STRING\""
                + "  n5 -> n6 // \"STRING\" -> \"CHARACTER\""
                + "  n5 -> n7 // \"STRING\" -> \"LENGTH\""
                + "  n7 -> n8 // \"LENGTH\" -> \"20\""
        );
    }

    /**
     * A tree with a single branch..
     */
    public void testOneBranch() {
        normalizeCheck(
                "Declare 1 Payroll, 4 Name, 5 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"4\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n12 -> n13 // \"LEVEL\" -> \"5\""
                + "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
                + "  n14 -> n15 // \"NAME\" -> \"Last\""
                + "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
                + "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
                + "  n16 -> n18 // \"STRING\" -> \"LENGTH\""
                + "  n18 -> n19 // \"LENGTH\" -> \"20\""
        );
    }

    /**
     * A tree with multiple branches.
     */
    public void testMultipleBranches() {
        normalizeCheck(
                "declare 1 Payroll, /* major structure name */"
                + " 2 Name, /* minor structure name */"
                + "     3 Last char(20), /* elementary name */"
                + "     3 First char(15),"
                + " 2 Hours,"
                + "     3 Regular fixed dec(5,2),"
                + "     3 Overtime fixed dec(5,2),"
                + " 2 Rate,"
                + "     3 Regular fixed dec(3,2),"
                + "     3 Overtime fixed dec(3,2);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n11 -> n12 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n12 -> n13 // \"LEVEL\" -> \"3\""
                + "  n11 -> n14 // \"DATA_ITEM\" -> \"NAME\""
                + "  n14 -> n15 // \"NAME\" -> \"Last\""
                + "  n11 -> n16 // \"DATA_ITEM\" -> \"STRING\""
                + "  n16 -> n17 // \"STRING\" -> \"CHARACTER\""
                + "  n16 -> n18 // \"STRING\" -> \"LENGTH\""
                + "  n18 -> n19 // \"LENGTH\" -> \"20\""
                + "  n6 -> n20 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n20 -> n21 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n21 -> n22 // \"LEVEL\" -> \"3\""
                + "  n20 -> n23 // \"DATA_ITEM\" -> \"NAME\""
                + "  n23 -> n24 // \"NAME\" -> \"First\""
                + "  n20 -> n25 // \"DATA_ITEM\" -> \"STRING\""
                + "  n25 -> n26 // \"STRING\" -> \"CHARACTER\""
                + "  n25 -> n27 // \"STRING\" -> \"LENGTH\""
                + "  n27 -> n28 // \"LENGTH\" -> \"15\""
                + "  n1 -> n29 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n29 -> n30 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n30 -> n31 // \"LEVEL\" -> \"2\""
                + "  n29 -> n32 // \"DATA_ITEM\" -> \"NAME\""
                + "  n32 -> n33 // \"NAME\" -> \"Hours\""
                + "  n29 -> n34 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n34 -> n35 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n35 -> n36 // \"LEVEL\" -> \"3\""
                + "  n34 -> n37 // \"DATA_ITEM\" -> \"NAME\""
                + "  n37 -> n38 // \"NAME\" -> \"Regular\""
                + "  n34 -> n39 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n39 -> n40 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n39 -> n41 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n39 -> n42 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n42 -> n43 // \"PRECISION\" -> \"5\""
                + "  n42 -> n44 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n44 -> n45 // \"SCALING_FACTOR\" -> \"2\""
                + "  n29 -> n46 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n46 -> n47 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n47 -> n48 // \"LEVEL\" -> \"3\""
                + "  n46 -> n49 // \"DATA_ITEM\" -> \"NAME\""
                + "  n49 -> n50 // \"NAME\" -> \"Overtime\""
                + "  n46 -> n51 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n51 -> n52 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n51 -> n53 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n51 -> n54 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n54 -> n55 // \"PRECISION\" -> \"5\""
                + "  n54 -> n56 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n56 -> n57 // \"SCALING_FACTOR\" -> \"2\""
                + "  n1 -> n58 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n58 -> n59 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n59 -> n60 // \"LEVEL\" -> \"2\""
                + "  n58 -> n61 // \"DATA_ITEM\" -> \"NAME\""
                + "  n61 -> n62 // \"NAME\" -> \"Rate\""
                + "  n58 -> n63 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n63 -> n64 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n64 -> n65 // \"LEVEL\" -> \"3\""
                + "  n63 -> n66 // \"DATA_ITEM\" -> \"NAME\""
                + "  n66 -> n67 // \"NAME\" -> \"Regular\""
                + "  n63 -> n68 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n68 -> n69 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n68 -> n70 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n68 -> n71 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n71 -> n72 // \"PRECISION\" -> \"3\""
                + "  n71 -> n73 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n73 -> n74 // \"SCALING_FACTOR\" -> \"2\""
                + "  n58 -> n75 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n75 -> n76 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n76 -> n77 // \"LEVEL\" -> \"3\""
                + "  n75 -> n78 // \"DATA_ITEM\" -> \"NAME\""
                + "  n78 -> n79 // \"NAME\" -> \"Overtime\""
                + "  n75 -> n80 // \"DATA_ITEM\" -> \"ARITHMETIC\""
                + "  n80 -> n81 // \"ARITHMETIC\" -> \"FIXED\""
                + "  n80 -> n82 // \"ARITHMETIC\" -> \"DECIMAL\""
                + "  n80 -> n83 // \"ARITHMETIC\" -> \"PRECISION\""
                + "  n83 -> n84 // \"PRECISION\" -> \"3\""
                + "  n83 -> n85 // \"PRECISION\" -> \"SCALING_FACTOR\""
                + "  n85 -> n86 // \"SCALING_FACTOR\" -> \"2\""
        );
    }

    /**
     * Multiple trees (a forest).
     */
    public void testMultipleTrees() {
        normalizeCheck(
                "Declare 1 Payroll, 4 Name char(15);dcl 1 Last char(20);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"Payroll\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"4\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"Name\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
                + "  n11 -> n12 // \"STRING\" -> \"CHARACTER\""
                + "  n11 -> n13 // \"STRING\" -> \"LENGTH\""
                + "  n13 -> n14 // \"LENGTH\" -> \"15\""
                + "  n0 -> n15 // \"\" -> \"DATA_ITEM\""
                + "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n16 -> n17 // \"LEVEL\" -> \"1\""
                + "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
                + "  n18 -> n19 // \"NAME\" -> \"Last\""
                + "  n15 -> n20 // \"DATA_ITEM\" -> \"STRING\""
                + "  n20 -> n21 // \"STRING\" -> \"CHARACTER\""
                + "  n20 -> n22 // \"STRING\" -> \"LENGTH\""
                + "  n22 -> n23 // \"LENGTH\" -> \"20\""
        );
    }

    /**
     * Alignment attribute inheritance.
     */
    public void testAlignmentInheritance() {
        normalizeCheck(
                "Declare 1 S,"
                + " 2 X bit(2),        /* Unaligned by default */"
                + " 2 A aligned,       /* Aligned explicitly   */"
                + "   3 B,             /* Aligned from A       */"
                + "   3 C unaligned,   /* Unaligned explicitly */"
                + "     4 D,           /* Unaligned from C     */"
                + "     4 E aligned,   /* Aligned explicitly   */"
                + "     4 F,           /* Unaligned from C     */"
                + "   3 G,             /* Aligned from A       */"
                + " 2 H;               /* Aligned by default   */",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"S\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n6 -> n7 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n7 -> n8 // \"LEVEL\" -> \"2\""
                + "  n6 -> n9 // \"DATA_ITEM\" -> \"NAME\""
                + "  n9 -> n10 // \"NAME\" -> \"X\""
                + "  n6 -> n11 // \"DATA_ITEM\" -> \"STRING\""
                + "  n11 -> n12 // \"STRING\" -> \"BIT\""
                + "  n11 -> n13 // \"STRING\" -> \"LENGTH\""
                + "  n13 -> n14 // \"LENGTH\" -> \"2\""
                + "  n1 -> n15 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n15 -> n16 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n16 -> n17 // \"LEVEL\" -> \"2\""
                + "  n15 -> n18 // \"DATA_ITEM\" -> \"NAME\""
                + "  n18 -> n19 // \"NAME\" -> \"A\""
                + "  n15 -> n20 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n20 -> n21 // \"ALIGNMENT\" -> \"ALIGNED\""
                + "  n15 -> n22 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n22 -> n23 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n23 -> n24 // \"LEVEL\" -> \"3\""
                + "  n22 -> n25 // \"DATA_ITEM\" -> \"NAME\""
                + "  n25 -> n26 // \"NAME\" -> \"B\""
                + "  n22 -> n27 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n27 -> n28 // \"ALIGNMENT\" -> \"ALIGNED\""
                + "  n15 -> n29 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n29 -> n30 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n30 -> n31 // \"LEVEL\" -> \"3\""
                + "  n29 -> n32 // \"DATA_ITEM\" -> \"NAME\""
                + "  n32 -> n33 // \"NAME\" -> \"C\""
                + "  n29 -> n34 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n34 -> n35 // \"ALIGNMENT\" -> \"UNALIGNED\""
                + "  n29 -> n36 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n36 -> n37 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n37 -> n38 // \"LEVEL\" -> \"4\""
                + "  n36 -> n39 // \"DATA_ITEM\" -> \"NAME\""
                + "  n39 -> n40 // \"NAME\" -> \"D\""
                + "  n36 -> n41 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n41 -> n42 // \"ALIGNMENT\" -> \"UNALIGNED\""
                + "  n29 -> n43 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n43 -> n44 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n44 -> n45 // \"LEVEL\" -> \"4\""
                + "  n43 -> n46 // \"DATA_ITEM\" -> \"NAME\""
                + "  n46 -> n47 // \"NAME\" -> \"E\""
                + "  n43 -> n48 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n48 -> n49 // \"ALIGNMENT\" -> \"ALIGNED\""
                + "  n29 -> n50 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n50 -> n51 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n51 -> n52 // \"LEVEL\" -> \"4\""
                + "  n50 -> n53 // \"DATA_ITEM\" -> \"NAME\""
                + "  n53 -> n54 // \"NAME\" -> \"F\""
                + "  n50 -> n55 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n55 -> n56 // \"ALIGNMENT\" -> \"UNALIGNED\""
                + "  n15 -> n57 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n57 -> n58 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n58 -> n59 // \"LEVEL\" -> \"3\""
                + "  n57 -> n60 // \"DATA_ITEM\" -> \"NAME\""
                + "  n60 -> n61 // \"NAME\" -> \"G\""
                + "  n57 -> n62 // \"DATA_ITEM\" -> \"ALIGNMENT\""
                + "  n62 -> n63 // \"ALIGNMENT\" -> \"ALIGNED\""
                + "  n1 -> n64 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n64 -> n65 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n65 -> n66 // \"LEVEL\" -> \"2\""
                + "  n64 -> n67 // \"DATA_ITEM\" -> \"NAME\""
                + "  n67 -> n68 // \"NAME\" -> \"H\""
        );
    }

    /**
     * A union.
     */
    public void testUnion() {
        normalizeCheck(
                "dcl 1 * union, 2 b3 bit(32), 2 b4 bit(16);",

                "  n0 -> n1 // \"\" -> \"DATA_ITEM\""
                + "  n1 -> n2 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n2 -> n3 // \"LEVEL\" -> \"1\""
                + "  n1 -> n4 // \"DATA_ITEM\" -> \"NAME\""
                + "  n4 -> n5 // \"NAME\" -> \"*\""
                + "  n1 -> n6 // \"DATA_ITEM\" -> \"UNION\""
                + "  n1 -> n7 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n7 -> n8 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n8 -> n9 // \"LEVEL\" -> \"2\""
                + "  n7 -> n10 // \"DATA_ITEM\" -> \"NAME\""
                + "  n10 -> n11 // \"NAME\" -> \"b3\""
                + "  n7 -> n12 // \"DATA_ITEM\" -> \"STRING\""
                + "  n12 -> n13 // \"STRING\" -> \"BIT\""
                + "  n12 -> n14 // \"STRING\" -> \"LENGTH\""
                + "  n14 -> n15 // \"LENGTH\" -> \"32\""
                + "  n1 -> n16 // \"DATA_ITEM\" -> \"REDEFINES\""
                + "  n16 -> n17 // \"REDEFINES\" -> \"b3\""
                + "  n1 -> n18 // \"DATA_ITEM\" -> \"DATA_ITEM\""
                + "  n18 -> n19 // \"DATA_ITEM\" -> \"LEVEL\""
                + "  n19 -> n20 // \"LEVEL\" -> \"2\""
                + "  n18 -> n21 // \"DATA_ITEM\" -> \"NAME\""
                + "  n21 -> n22 // \"NAME\" -> \"b4\""
                + "  n18 -> n23 // \"DATA_ITEM\" -> \"STRING\""
                + "  n23 -> n24 // \"STRING\" -> \"BIT\""
                + "  n23 -> n25 // \"STRING\" -> \"LENGTH\""
                + "  n25 -> n26 // \"LENGTH\" -> \"16\""
                + "  n18 -> n27 // \"DATA_ITEM\" -> \"REDEFINES\""
                + "  n27 -> n28 // \"REDEFINES\" -> \"b3\""
        );
    }
    /**
     * A generic test helper that takes a source fragment and checks the result.
     * @param source the source fragment
     * @param expected the expected sub graph
     */
    private void normalizeCheck(final String source, final String expected) {
        CommonTree ast = parseAndNormalize(source);
        String graph = getGraph(ast);
        assertEquals(expected, getSubGraph(graph));
    }

}
