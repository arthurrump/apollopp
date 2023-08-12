module graphs::LLGraph

import analysis::graphs::LabeledGraph;

data Vertex[&Id, &VertexLabel] 
    = Vertex(&Id id, set[&VertexLabel] labels = {})
    ;

alias LLGraph[&VertexId, &VertexLabel, &EdgeLabel] 
    = LGraph[Vertex[&VertexId, &VertexLabel], &EdgeLabel]
    ;

data Pattern[&T] 
    = Required(&T required)
    | Forbidden(&T forbidden)
    ;

alias PatternLLGraph[&VertexId, &VertexLabel, &EdgeLabel] 
    = LGraph[Pattern[Vertex[&VertexId, Pattern[&VertexLabel]]], Pattern[&EdgeLabel]]
    ;
