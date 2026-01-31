/**
 * Test to verify named property shapes generation
 * Run with: deno run --allow-all core/src/shacl/test-named-shapes.ts
 */

import { SHACLShape, SHACLPropertyShape } from "./SHACLShape.ts";

console.log("Testing named property shapes generation...\n");

// Create a simple shape for a Recipe class
const recipeShape = new SHACLShape("recipe://RecipeShape", "recipe://Recipe");

// Add property with name field (simulating decorator output)
const nameProperty: SHACLPropertyShape = {
    name: "name",
    path: "recipe://name",
    datatype: "xsd://string",
    minCount: 1,
    maxCount: 1
};

const ingredientsProperty: SHACLPropertyShape = {
    name: "ingredients",
    path: "recipe://ingredients",
    datatype: "xsd://string",
    // No maxCount = collection
};

recipeShape.addProperty(nameProperty);
recipeShape.addProperty(ingredientsProperty);

// Convert to links
const links = recipeShape.toLinks();

console.log("Generated links:");
console.log(JSON.stringify(links, null, 2));

// Verify named URIs are generated
const namedShapeLinks = links.filter(l => 
    l.source === "recipe://RecipeShape" && 
    l.predicate === "sh://property" &&
    !l.target.startsWith("_:")  // Should NOT be blank node
);

console.log(`\nNamed property shape links found: ${namedShapeLinks.length}`);
namedShapeLinks.forEach(link => {
    console.log(`  ✓ ${link.target}`);
});

// Verify expected URIs
const expectedNameURI = "recipe://Recipe.name";
const expectedIngredientsURI = "recipe://Recipe.ingredients";

const nameLink = links.find(l => l.target === expectedNameURI);
const ingredientsLink = links.find(l => l.target === expectedIngredientsURI);

if (nameLink) {
    console.log(`✅ Found expected name property URI: ${expectedNameURI}`);
} else {
    console.error(`❌ Missing expected name property URI: ${expectedNameURI}`);
}

if (ingredientsLink) {
    console.log(`✅ Found expected ingredients property URI: ${expectedIngredientsURI}`);
} else {
    console.error(`❌ Missing expected ingredients property URI: ${expectedIngredientsURI}`);
}

// Check for blank nodes (should not exist with named shapes)
const blankNodeLinks = links.filter(l => l.target.startsWith("_:"));
if (blankNodeLinks.length > 0) {
    console.error(`\n❌ Found ${blankNodeLinks.length} blank nodes (should be 0):`);
    blankNodeLinks.forEach(l => console.error(`  ${l.target}`));
} else {
    console.log("\n✅ No blank nodes found - all property shapes have named URIs!");
}

console.log("\n" + "=".repeat(60));
if (nameLink && ingredientsLink && blankNodeLinks.length === 0) {
    console.log("✅ TEST PASSED: Named property shapes working correctly!");
} else {
    console.log("❌ TEST FAILED: Issues detected");
    Deno.exit(1);
}
