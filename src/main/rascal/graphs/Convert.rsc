module graphs::Convert

// Conversion from directed edge-labeled graphs in Rascal (LGraph) to graph
// formats understood by other tools.

import IO;
import List;
import String;
import ValueIO;
import graphs::LLGraph;

private str serialize(&T x) = toBase64(iprintToString(x));
private &T deserialize(type[&T] resultType, str s) = readTextValueString(resultType, fromBase64(s));

private test bool serializeDeserializeReturnsValue(value x) = deserialize(#value, serialize(x)) == x;

str writeGssCsv(LLGraph[&Vertex, &VertexLabel, &EdgeLabel] g) {
    // https://github.com/ciaranm/glasgow-subgraph-solver#file-formats
    edges = (
        "" 
        | it + serialize(from.id) + "\>" + serialize(to.id) + "," + serialize(edge) + "\n" 
        | <from, edge, to> <- g
    );

    vertexLabels = (
        ""
        | it + serialize(vertex.id) + ",," + serialize(vertex.labels) + "\n"
        | vertex <- g<from> + g<to>
    );

    return edges + vertexLabels;
}

set[rel[&SubVertexId, &GraphVertexId]] readGssResult(str gssResult, type[&SubVertexId] subVertexIdType, type[&GraphVertexId] graphVertexIdType) {
    return { 
        { <deserialize(subVertexIdType, left), deserialize(graphVertexIdType, right)> | 
            pair <- split(") (", substring(mapping, size("mapping = "), size(mapping) - 1)), 
            [ left, right ] := split(" -\> ", pair) } 
    | 
        mapping <- split("\n", gssResult), 
        startsWith(mapping, "mapping = ")
    };
}
