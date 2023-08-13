module graphs::LLGraph

import analysis::graphs::LabeledGraph;

data Vertex[&Id, &VertexLabel] 
    = Vertex(&Id id, set[&VertexLabel] labels = {})
    ;

alias LLGraph[&VertexId, &VertexLabel, &EdgeLabel] 
    = LGraph[Vertex[&VertexId, &VertexLabel], &EdgeLabel]
    ;
