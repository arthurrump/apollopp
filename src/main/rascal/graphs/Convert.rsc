module graphs::Convert

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
data LabelMapping(map[&NodeLabel, int] nodeLabels = (), map[&EdgeLabel, int] edgeLabels = ()) = LabelMapping();

private str serialize(&T x) = toBase64(iprintToString(x));
private &T deserialize(type[&T] resultType, str s) = readTextValueString(resultType, fromBase64(s));

private test bool serializeDeserializeReturnsValue(value x) = deserialize(#value, serialize(x)) == x;

str writeGssCsv(LGraph[&Node, &Edge] g, &NodeLabel (&Node) labelNode = id, &EdgeLabel (&Edge) labelEdge = id) {
    // https://github.com/ciaranm/glasgow-subgraph-solver#file-formats
    str edges = ("" | it + serialize(from) + "\>" + serialize(to) + "," + serialize(labelEdge(edge)) + "\n" | <from, edge, to> <- g);
    str nodeLabels = ("" | it + serialize(\node) + ",," + serialize(labelNode(\node)) + "\n" | \node <- (g<from> + g<to>));
    return edges + nodeLabels;
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

tuple[str spmfGraph, LabelMapping mapping] writeSpmfGraph(LGraph[&Node, &Edge] g, &NodeLabel (&Node) labelNode = id, &EdgeLabel (&Edge) labelEdge = id, LabelMapping baseMapping = LabelMapping()) {
    // https://www.philippe-fournier-viger.com/spmf/TKG.php
    // A graph is defined by a few lines of text that follow the following format:
    // - t # N    This is the first line of a graph. It indicates that this is the N-th graph in the file
    // - v M L     This line defines the M-th vertex of the current graph, which has a label L
    // - e P Q L   This line defines an edge, which connects the P-th vertex with the Q-th vertex. This edge has the label L
    
    set[&Node] nodes = g<from> + g<to>;
    map[&Node, int] nodeIndices = index(nodes);

    map[&NodeLabel, int] nodeLabels = baseMapping.nodeLabels;
    str spmfNodes = "";
    for (\node <- nodes) {
        &NodeLabel label = labelNode(\node);
        if (label notin nodeLabels) {
            nodeLabels[label] = size(nodeLabels);
        }

        spmfNodes += "v " + toString(nodeIndices[\node]) + " " + toString(nodeLabels[label]) + "\n";
    }

    map[&EdgeLabel, int] edgeLabels = baseMapping.edgeLabels;
    str spmfEdges = "";
    for (<from, edge, to> <- g) {
        &EdgeLabel label = labelEdge(edge);
        if (label notin edgeLabels) {
            edgeLabels[label] = size(edgeLabels);
        }

        spmfEdges += "e " + toString(nodeIndices[from]) + " " + toString(nodeIndices[to]) + " " + toString(edgeLabels[label]) + "\n";
    }

    return <spmfNodes + spmfEdges, LabelMapping(nodeLabels = nodeLabels, edgeLabels = edgeLabels)>;
}

tuple[str spmfGraphList, LabelMapping mapping] writeSpmfGraphList(list[LGraph[&Node, &Edge]] graphs, &NodeLabel (&Node) labelNode = id, &EdgeLabel (&Edge) labelEdge = id, LabelMapping baseMapping = LabelMapping()) {
    str spmfGraphList = "";
    LabelMapping mapping = baseMapping;
    int index = 0;
    for (LGraph[&Node, &Edge] g <- graphs) {
        tuple[str spmfGraph, LabelMapping mapping] result = writeSpmfGraph(g, labelNode = labelNode, labelEdge = labelEdge, baseMapping = mapping);
        spmfGraphList += "t # " + toString(index) + "\n" + result.spmfGraph + "\n";
        mapping = result.mapping;
    }
    return <spmfGraphList, mapping>;
}

LGraph[&NodeLabel, &EdgeLabel] readSpmfSubgraph(str spmfSubgraph, LabelMapping mapping) {
    // https://www.philippe-fournier-viger.com/spmf/TKG.php
    // A frequent subgraph is defined by a few lines of text that follow the following format:
    // - t # N * Z    This is the first line of a subgraph. It indicates that this is the N-th subgraph in the file and that its support is Z.
    // - v M L     This line defines the M-th vertex of the current subgraph, which has a label L
    // - e P Q L   This line defines an edge, which connects the P-th vertex with the Q-th vertex. This edge has the label L
    // - x X1 X2 ... This lines lists the identifiers of all the graphs X1, X2 ... that contains the current subgraph.
    map[int, &NodeLabel] nodeLabels = invertUnique(mapping.nodeLabels);
    map[int, &EdgeLabel] edgeLabels = invertUnique(mapping.edgeLabels);
    list[str] lines = split("\n", spmfSubgraph);

    nodes = for (str line <- lines) {
        if (startsWith(line, "v")) {
            list[str] ls = split(" ", line);
            int nodeIndex = toInt(ls[1]);
            int nodeLabelIndex = toInt(ls[2]);
            append <nodeIndex, nodeLabels[nodeLabelIndex]>;
        }
    }

    edges = for (str line <- lines) {
        if (startsWith(line, "e")) {
            list[str] ls = split(" ", line);
            int fromIndex = toInt(ls[1]);
            int toIndex = toInt(ls[2]);
            int edgeLabelIndex = toInt(ls[3]);
            append <nodes[fromIndex], edgeLabels[edgeLabelIndex], nodes[toIndex]>;
        }
    }

    return toSet(edges);
}

lrel[LGraph[&NodeLabel, &EdgeLabel] subgraph, list[int] graphsContaining] readSpmfSubgraphList(str spmfSubgraphList, LabelMapping mapping) {
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
