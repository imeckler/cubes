function objEmpty(x){
  for (var p in x) {
    return false;
  }
  return true;
}

function isolatedNodes(g) {
  g = g[1].graph;
  var nodes = g.nodes();
  var adjs  = g.getAllNeighborsIndex();
  var n     = nodes.length;
  //var res   = [];
  var isolatedCount = 0;

  var node;
  window.adjs = adjs;
  for (var i = 0; i < n; ++i) {
    node = nodes[i].id;
    if (objEmpty(adjs[node])) {
      ++isolatedCount;
    }
  }
  return isolatedCount;
}

function weaklyConnected(g) {
  if (g.getNodesCount() === 0) {
    return true;
  }

  var adjs      = g.getAllNeighborsIndex();

  window.adjs = adjs;
  var seenCount = 0;
  var seen      = {};
  var stack     = [g.nodes()[0].id];
  var node;

  while (stack.length > 0) {
    node = stack.pop();
    if (seen[node]){
      continue;
    }

    seen[node] = true;
    ++seenCount;

    for (var node2 in adjs[node]) {
      stack.push(node2)
    }
  }

  // console.log(seenCount, g.getNodesCount());

  return (seenCount === g.getNodesCount())
}
