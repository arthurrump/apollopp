# Graphmatcher

This part checks patterns against graphs extracted from code projects.

## Usage

```
USAGE: apollopp-graphmatcher.exe [--help] --criteria <path> --targets <path> [--graph-path <path>] [--skip <n>]
                                 [--limit <n>] [<subcommand> [<options>]]

SUBCOMMANDS:

    configure <options>   Interactively configure patterns
    run <options>         Run patterns on targets

    Use 'apollopp-graphmatcher.exe <subcommand> --help' for additional information.

OPTIONS:

    --criteria, -c <path> Path to the directory containing JSON criterion files
    --targets, -t <path>  Path to the directory containing target projects
    --graph-path <path>   Path to the directory within a project containing JSON graph files, defaults to source/graph
    --skip <n>            Skip the first n targets
    --limit <n>           Limit the number of targets to n
    --help                display this list of options.
```

The graphmatcher works with a directory containing target projects, each project in a subfolder. The graphs should be in the *source/graph* subfolder of each project (or an alternate location specified through the `--graph-path` option). The `--skip` and `--limit` options can be used to use only a subset of projects for configuration, for example.

The *run* subcommand runs all criteria patterns on all target projects and prints the results to stdout or to separate files per project in a directory specified through the `--output` option. *configure* watches the folder containing criteria for changes and runs the patterns on the specified target projects each time a criterion changes or is added. The results are printed to stdout or a file specified through the `--output` option.

Criteria can be defined as JSON files, containing the textual criterion and one or more pattern trees, or as a JavaScript file containing a variable `criterion` with the same object structure. The latter can be used to generate many alternative patterns that only vary based on the name of a variable or method, for example.

A pattern tree consists of a *verdict*, a *pattern* and a list of *children*. The verdict can be *positive*, *negative* or *neutral* and the pattern is a subgraph defined as a list of `[from, edge, to]` labels. The children are again pattern trees, each with their own verdict, pattern and possibly children. Children extend the pattern of their parent, so the pattern of a child in the tree is combined with the pattern of its parent to define the full subgraph that is used to find a match. Only the deepest match(es) in the tree is (are) returned as a match, so a child with the verdict neutral can, for example, be used to make an exception to a rule. If that child matches, the result will be neutral and thus effectively ignored in the assessment.

## Technical details

This program defines two graph representations: 

- as a set of edges, which are tuples `'node * 'edge * 'node`. This is the same as Rascal's [`LGraph[&T,&L]`](https://www.rascal-mpl.org/docs/Library/analysis/graphs/LabeledGraph/#analysis-graphs-LabeledGraph-LGraph).
- as a two-dimensional array of sets, representing a directed adjacency matrix where each cell contains the set of edge labels that connect two nodes. This representation is used in the graph matching algorithm to more quickly find neighbouring nodes.

The subgraph matching algorithm used here is based on the [SuMGra](https://doi.org/10.1007/978-3-319-44403-1_24) algorithm for multigraphs, but adjusted to work on loopy, directed multigraphs with potentially disconnected query graphs. SuMGra works by incrementally growing a mapping, trying to map the next node of the query to all possible candidates from the target. If no candidate is available, the mapping is discarded. To do this efficiently, the algorithm does three things: map the next query node in a specific order, use an index to quickly find initial candidates and use an index to find candidates to extend the mapping. We will briefly discuss each of these and how they are adapted to work with directed, loopy graphs.

The query order is determined by first selecting the most connected node and then selecting the next node based on the maximal connectivity to the already selected nodes. This helps to discard many potential mappings early on, so the search space is pruned early in the process. To supported directed, loopy graphs, we count both incoming and outgoing edges as connections, while discarding loops.

Initial candidates are selected from a *signature index*. Each node in the target graph is given a signature, a feature vector with features like the number of connecting edges, the number neighbouring nodes (those are not always equal in a multigraph) or the number of unique edge labels connected to the node. Crucially, the signature of a query node is smaller than the signature of each potential target node: the target node needs at least as many neighbours as the query node, or a mapping would not be possible. These signatures are used as keys in an R-Tree, by turning them into an (n-dimensional) rectangle with a point on the origin and the other at the feature vector. We can then search this R-Tree for all rectangles that contain the point given by the signature of a query node to find all potential target nodes.

To support directed graphs, we changed the features to incorporate the directionality of edges: the number of neighbours with incoming edges, the number of neighbours with outgoing edges, the total number of incoming edges, the total number of outgoing edges, the number of unique edge labels on all incoming and outgoing edges and the maximum number of edges (incoming or outgoing) with any neighbour. We changed the algorithm to support disconnected queries by reusing this index when the next query node is not connected to the previous query nodes.

When a partial mapping is available, next candidates are selected based on the *neighbourhood index*. For each node in the target graph, this index contains a trie keyed by edge labels, storing the neighbours that share edges with those labels. If an edge with labels *a*, *b* and *c* connects node *1* with node *2*, the trie for node *1* has an entry with key *{a,b,c}* storing value *2*. The trie can be queried to find all superset keys, so a query for *{b,c}* would also yield value *2* (and possibly other values). This enables us to find nodes in the target graph that have at least the same edges as the current query node. For efficiency, the trie only stores neighbours and not all nodes. When querying with the empty set, this means that the result will be incomplete, because all nodes have at least zero connections with any node, so really all nodes should be returned.

For this to work with directed graphs, we store two tries per node in the index, one for incoming edges and one for outgoing edges. When searching, we can simply take the intersection of the results for both tries, because we are interested in nodes that include both the requested incoming and outgoing edges. As noted, an empty query should return all nodes, so as an optimization we can avoid taking the intersection and just query one trie if the requested incoming or outgoing edges are the empty set.

Since the assessment rules are written as a pattern tree, where each child extends the pattern of its parent, it would be wasteful to search a mapping for each node in the child pattern all over again, if we already found a mapping for the pattern in a parent node. Similarly, if no mappings were found for the parent, we can be sure that the extended graph of the child will not have a valid mapping either. To efficiently extend an existing mapping for an extended query graph, we made a few changes in the algorithm: we added another way to order the nodes in the query graph and we added an entrypoint to the search that accepts an existing mapping.

Given a query graph *Q* and we can determine the optimal order for *Q* according to the rules described before. If we extend this query with additional nodes and/or edges into query graph *Q'* to search for extended mappings using the results we already found for *Q*, we need to ensure that the nodes that are already mapped from *Q* are also the first in the mapping order for *Q'*. This can be done by simply copying over the original order and using the rules described before for the additional nodes.

Since the algorithm works by recursively extending an existing mapping, working with an extended query is relatively simple: just pass the original mapping and extended query into the recursive function. There is a catch, however: the extensions to the query may have invalidated the original mapping. If the original query graph contained an edge *a* from node *1* to node *2* and the extended query graph adds an edge *b* from node *2* to node *1*, the original mapping may no longer be valid if the target graph does not contain that edge *b*. For this reason, the entrypoint that accepts an existing mapping first verifies the validity of that mapping with the given query and target. If the mapping is invalid, an empty list of mappings is returned, because there are no valid mappings that extend the given mapping.

