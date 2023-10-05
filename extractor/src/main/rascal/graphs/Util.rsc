module graphs::Util

import analysis::graphs::LabeledGraph;

LGraph[&Node, set[&Edge]] condenseMultigraph(LGraph[&Node, &Edge] g) {
    return { <from, (g<from, to, label>[from, to]), to> | <from, to> <- g<from, to> };
}

test bool testCondenseMultiGraph() {
    return 
        condenseMultigraph({ <"a", 1, "b">, <"a", 3, "b">, <"b", 2, "c">, <"b", 1, "a"> }) ==
        { <"a", {1, 3}, "b">, <"b", {2}, "c">, <"b", {1}, "a"> };
}
