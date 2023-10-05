module graphs::Convert

// Conversion from directed edge-labeled graphs in Rascal (LGraph) to graph
// formats understood by other tools.

import IO;
import List;
import Map;
import Relation;
import Set;
import String;
import ValueIO;
import analysis::graphs::LabeledGraph;
import util::Math;

private &T id(&T x) = x;

private str serialize(&T x) = toBase64(iprintToString(x));
private &T deserialize(type[&T] resultType, str s) = readTextValueString(resultType, fromBase64(s));

private test bool serializeDeserializeReturnsValue(value x) = deserialize(#value, serialize(x)) == x;

str writeGssCsv(LGraph[&Node, &Edge] g, &Label (&Edge) label = id) {
    // https://github.com/ciaranm/glasgow-subgraph-solver#file-formats
    return (
        "" 
        | it + serialize(from) + "\>" + serialize(to) + "," + serialize(label(edge)) + "\n" 
        | <from, edge, to> <- g
    );
}

set[rel[&PatternNode, &GraphNode]] readGssResult(str gssResult, type[&PatternNode] patternNodeType, type[&GraphNode] graphNodeType) {
    return { 
        { <deserialize(patternNodeType, left), deserialize(graphNodeType, right)> | 
            pair <- split(") (", substring(mapping, size("mapping = "), size(mapping) - 1)), 
            [ left, right ] := split(" -\> ", pair) } 
    | 
        mapping <- split("\n", gssResult), 
        startsWith(mapping, "mapping = ") 
    };
}

tuple[str spmfGraph, map[&Label, int] mapping] writeSpmfGraph(LGraph[&Node, &Edge] g, &Label (&Edge) label = id, map[&Label, int] mapping = ()) {
    // https://www.philippe-fournier-viger.com/spmf/TKG.php
    // A graph is defined by a few lines of text that follow the following format:
    // - t # N    This is the first line of a graph. It indicates that this is the N-th graph in the file
    // - v M L     This line defines the M-th vertex of the current graph, which has a label L
    // - e P Q L   This line defines an edge, which connects the P-th vertex with the Q-th vertex. This edge has the label L
    
    set[&Node] nodes = g<from> + g<to>;
    map[&Node, int] nodeIndices = Set::index(nodes);

    str spmfNodes = "";
    for (\node <- nodes) {
        // We don't have node labels, so all nodes get the same label
        spmfNodes += "v " + toString(nodeIndices[\node]) + " 0\n";
    }

    str spmfEdges = "";
    for (<from, edge, to> <- g) {
        &Label l = label(edge);
        if (l notin mapping) {
            mapping[l] = size(mapping);
        }

        spmfEdges += "e " + toString(nodeIndices[from]) + " " + toString(nodeIndices[to]) + " " + toString(mapping[l]) + "\n";
    }

    return <spmfNodes + spmfEdges, mapping>;
}

tuple[str spmfGraphList, map[&Label, int] mapping] writeSpmfGraphList(list[LGraph[&Node, &Edge]] graphs, &Label (&Edge) label = id, map[&Label, int] mapping = ()) {
    str spmfGraphList = "";
    int index = 0;
    for (LGraph[&Node, &Edge] g <- graphs) {
        tuple[str spmfGraph, map[&Label, int] mapping] result = writeSpmfGraph(g, label = label, mapping = mapping);
        spmfGraphList += "t # " + toString(index) + "\n" + result.spmfGraph + "\n";
        mapping = result.mapping;
        index += 1;
    }
    return <spmfGraphList, mapping>;
}

LGraph[int, &Label] readSpmfSubgraph(str spmfSubgraph, map[&Label, int] mapping) {
    // https://www.philippe-fournier-viger.com/spmf/TKG.php
    // A frequent subgraph is defined by a few lines of text that follow the following format:
    // - t # N * Z    This is the first line of a subgraph. It indicates that this is the N-th subgraph in the file and that its support is Z.
    // - v M L     This line defines the M-th vertex of the current subgraph, which has a label L
    // - e P Q L   This line defines an edge, which connects the P-th vertex with the Q-th vertex. This edge has the label L
    // - x X1 X2 ... This lines lists the identifiers of all the graphs X1, X2 ... that contains the current subgraph.
    map[int, &Label] labels = invertUnique(mapping);
    list[str] lines = split("\n", spmfSubgraph);

    edges = for (str line <- lines) {
        if (startsWith(line, "e")) {
            list[str] ls = split(" ", line);
            int from = toInt(ls[1]);
            int to = toInt(ls[2]);
            int labelIndex = toInt(ls[3]);
            append <from, labels[labelIndex], to>;
        }
    }

    return toSet(edges);
}

lrel[LGraph[int, &EdgeLabel] subgraph, list[int] graphsContaining] readSpmfSubgraphList(str spmfSubgraphList, map[&Label, int] mapping) {
    list[str] spmfSubgraphs = [ sgt | sg <- split("t #", spmfSubgraphList), sgt := trim(sg), sgt != "" ];
    lrel[LGraph[&NodeLabel, &EdgeLabel], list[int]] result = [];
    for (str spmfSubgraph <- spmfSubgraphs) {
        list[str] lines = split("\n", spmfSubgraph);
        list[int] graphsContaining = [];
        for (str line <- lines) {
            if (startsWith(line, "x")) {
                list[str] ls = split(" ", line);
                graphsContaining = [ toInt(l) | l <- ls[1..] ];
            }
        }
        result += <readSpmfSubgraph(spmfSubgraph, mapping), graphsContaining>;
    }
    return result;
}
