# Extractor

The extractor parses and analyses code to extract graph representations used in the other parts of the assessment pipeline. It currently supports Processing and Java projects and builds a *TypeGraph* with nodes for classes, methods, fields etc. and their relations as edges.

## Usage

For Processing projects: ensure that the Processing code is already converted to Java using the [pre-process script](../pre_process/pde_to_java.py). If the conversion process complains about missing libraries, install them in the Processing IDE via *Sketch > Import Library > Manage Libraries*. The Processing extractor module requires a reference to all libraries, for which two environment variables are required:

- `PROCESSING_CORELIB` with the path to Processing's *core/library/core.jar* library, usually located in the directory where Processing is installed, and
- `PROCESSING_LIBRARIES` with the folder where libraries added in the Processing IDE are installed. This likely is *~/sketchbook/libraries* on Linux and *Documents\Processing\libraries* on Windows. 

When using `nix develop`, these variables are set automatically. Then load the `Processing` module in a Rascal REPL.

For Java projects: ensure that all dependencies are available as .jar files in the project folder and load the `Java` module in a Rascal REPL.

Finally, for both Processing and Java, call `writeProjectTypeGraphs` with the absolute path to a folder containing project folders. The TypeGraph JSON files will be written to *source/graph/typegraph.json* in the project folder.

## Technical details

The extractors are ultimately quite simple, because they rely on the M^3^ model provided by Rascal to build the TypeGraph. The M^3^ model is a set of relations mostly between locations, for example uses, which relates every usage of a name to its declaration, or containment, which relates e.g. classes to the methods they contain. The TypeGraph turns this set of relations into a triple relation, `rel[&Node from, &Label label, &Node to]`. The nodes in the TypeGraph are all locations and labels represent the type of relation, like containment, class inheritence, method invocation etc.

To store properties of a single node, the TypeGraph uses loops (self-edges), which we call annotations. There are four annotations: *name class*, which is used to extract commonly used parts of names for types declared in the project (for example *Factory* to help recognize that pattern); *modifier*, which contains modifiers like *public* or *private*; *inProjectDecl* for elements that are declared in the project, containing the type of declaration (like *class*, *method* or *field*); and *externalDecl* for elements declared in a library outside the project, containing the full location of that element.

Encoding node labels as edge labels like this simplifies all code that works with graphs, since it only needs to consider edge labels. In Rascal, this means we can use the built-in `LGraph` type and functions and in the subgraph matching algorithm, we can use the same edge label logic for connections between nodes as well as properties of nodes.
