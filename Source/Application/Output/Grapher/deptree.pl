/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> DEPTREE
Interactive HTML dependency graph visualisation for portage-ng packages.
Generates a self-contained HTML file with a layered tree graph showing
transitive dependencies per dep type (BDEPEND, DEPEND, RDEPEND, PDEPEND, etc.),
node cards with installed status, hover tooltips with metadata, collapsible
subtrees, pan/zoom, and dep-type tab switching.
*/

:- module(deptree, []).

% =============================================================================
%  DEPTREE declarations
% =============================================================================


% -----------------------------------------------------------------------------
%  Entry point
% -----------------------------------------------------------------------------

%! deptree:graph(+Target)
%
% Generate a dependency graph HTML document for Target to current output stream.
% Collects trees for all dep types, then emits HTML.

deptree:graph(Repository://Entry) :-
    DepTypes = [bdepend, cdepend, depend, idepend, rdepend, pdepend],
    collect_all_types(DepTypes, Repository, Entry, TypeTrees),
    deptree:emit_html(Repository://Entry, TypeTrees).


% -----------------------------------------------------------------------------
%  Data collection - tree traversal
% -----------------------------------------------------------------------------

:- thread_local dep_visited/1.

%! deptree:collect_all_types(+Types, +Repo, +Entry, -TypeTrees)
%
% Collect dependency trees for each dep type. TypeTrees is a list of
% deptree(Type, Nodes, Edges) terms. Empty trees are included with [] nodes.

collect_all_types([], _, _, []).
collect_all_types([Type|Types], Repo, Entry, [deptree(Type, Nodes, Edges)|Rest]) :-
    catch(
        collect_tree(Type, Repo, Entry, Nodes, Edges),
        _,
        (Nodes = [], Edges = [])
    ),
    collect_all_types(Types, Repo, Entry, Rest).


%! deptree:collect_tree(+Type, +Repo, +Entry, -Nodes, -Edges)
%
% BFS traversal of the dependency tree for a given dep type. Tracks visited
% nodes to handle DAGs. Produces flat lists of node/6 and edge/3 terms.

collect_tree(Type, Repo, Entry, Nodes, Edges) :-
    retractall(dep_visited(_)),
    collect_tree_bfs([Repo://Entry], Type, Repo, [], [], NodesRev, EdgesRev),
    retractall(dep_visited(_)),
    reverse(NodesRev, Nodes),
    reverse(EdgesRev, Edges0),
    sort(Edges0, Edges).

collect_tree_bfs([], _, _, Nodes, Edges, Nodes, Edges).
collect_tree_bfs([Repo://Id|Queue], Type, RootRepo, NodesAcc, EdgesAcc, NodesOut, EdgesOut) :-
    (   dep_visited(Repo://Id)
    ->  collect_tree_bfs(Queue, Type, RootRepo, NodesAcc, EdgesAcc, NodesOut, EdgesOut)
    ;   assertz(dep_visited(Repo://Id)),
        make_node(Repo, Id, Node),
        Statement =.. [Type, DS],
        (   query:search(all(Statement), Repo://Id)
        ->  true
        ;   DS = []
        ),
        findall(edge(Id, ChildId, Strength),
            (   member(D, DS),
                dep_choice(D, Repo, ChildId, Strength)
            ),
            NewEdges),
        findall(Repo://ChildId, member(edge(_, ChildId, _), NewEdges), Children),
        append(Queue, Children, Queue1),
        append(NodesAcc, [Node], NodesAcc1),
        append(EdgesAcc, NewEdges, EdgesAcc1),
        collect_tree_bfs(Queue1, Type, RootRepo, NodesAcc1, EdgesAcc1, NodesOut, EdgesOut)
    ).


%! deptree:dep_choice(+DepTerm, +Repo, -ChildEntry, -Strength)
%
% Extract a concrete package choice from a dependency term, matching the
% grapher handle logic for full tree graphs (groups are skipped).

dep_choice(package_dependency(_, Strength, Cat, Name, Cmpr, Ver, _, _),
           _Repo, Choice, Strength) :-
    query:search([name(Name), category(Cat), select(version, Cmpr, Ver)],
                 _://Choice), !.

dep_choice(Group, _, _, _) :-
    functor(Group, F, _),
    member(F, [use_conditional_group, any_of_group, all_of_group,
               exactly_one_of_group, at_most_one_of_group]),
    !, fail.


%! deptree:make_node(+Repo, +Entry, -Node)
%
% Build a node/6 term with category, name, version, slot, and installed status.

make_node(Repo, Entry, node(Entry, Cat, Name, Ver, Slot, Installed)) :-
    (   cache:ordered_entry(Repo, Entry, Cat, Name, Version)
    ->  version_domain:display_atom(Version, Ver)
    ;   Cat = '', Name = Entry, Ver = ''
    ),
    (   cache:entry_metadata(Repo, Entry, slot, slot(S))
    ->  atom_string(S, Slot)
    ;   Slot = "0"
    ),
    (   cache:ordered_entry(pkg, Entry, _, _, _)
    ->  Installed = true
    ;   Installed = false
    ).


% -----------------------------------------------------------------------------
%  Per-node metadata collection
% -----------------------------------------------------------------------------

%! deptree:node_meta(+Repo, +Entry, -Desc, -Homepage, -UseFlags)
%
% Retrieve description, homepage, and USE flags for a node.

node_meta(Repo, Entry, Desc, Homepage, UseFlags) :-
    (   cache:entry_metadata(Repo, Entry, description, D)
    ->  Desc = D
    ;   Desc = ''
    ),
    (   cache:entry_metadata(Repo, Entry, homepage, H)
    ->  Homepage = H
    ;   Homepage = ''
    ),
    (   gantt:pkg_use_flags(Repo, Entry, Flags)
    ->  UseFlags = Flags
    ;   UseFlags = []
    ).


% -----------------------------------------------------------------------------
%  Version navigation helpers
% -----------------------------------------------------------------------------

%! deptree:version_neighbours(+Repo, +Entry, -Newer, -Newest, -Older, -Oldest)
%
% Find adjacent versions for navigation links.

version_neighbours(Repo, Entry, Newer, Newest, Older, Oldest) :-
    query:search([category(C), name(N), version(V)], Repo://Entry),
    findall(E, query:search([name(N), category(C), select(version, greater, V)], Repo://E), EgList),
    findall(E, query:search([name(N), category(C), select(version, smaller, V)], Repo://E), EsList),
    (last(EgList, Newer0)       -> Newer = Newer0   ; Newer = ''),
    (EgList = [Newest0|_]       -> Newest = Newest0  ; Newest = ''),
    (EsList = [Older0|_]        -> Older = Older0    ; Older = ''),
    (last(EsList, Oldest0)      -> Oldest = Oldest0  ; Oldest = ''),
    !.
version_neighbours(_, _, '', '', '', '').


% -----------------------------------------------------------------------------
%  HTML emission - main
% -----------------------------------------------------------------------------

%! deptree:emit_html(+Target, +TypeTrees)
%
% Emit a complete self-contained HTML document to the current output stream.

deptree:emit_html(Target, TypeTrees) :-
    Target = Repo://Entry,
    cache:ordered_entry(Repo, Entry, Cat, Name, Version),
    version_domain:display_atom(Version, Ver),
    format(atom(Title), '~w/~w-~w &mdash; Dependency Graph', [Cat, Name, Ver]),
    navtheme:emit_doctype,
    navtheme:emit_head_open(Title, '../'),
    navtheme:emit_head_close,
    navtheme:emit_body_open('page-deptree'),
    navtheme:emit_top_bar('../', Repo, Cat, Name, Ver),
    navtheme:emit_main_open,
    navtheme:emit_page_head_open,
    emit_subtitle_placeholder,
    version_neighbours(Repo, Entry, Newer, Newest, Older, Oldest),
    navtheme:emit_nav_bar(Repo, Entry, Cat, Name, deptree, Newer, Newest, Older, Oldest, Ver),
    navtheme:emit_page_head_close,
    navtheme:emit_term_open('portage-ng deptree'),
    emit_dep_tabs,
    emit_graph_container,
    emit_legend,
    navtheme:emit_term_close,
    emit_tooltip_container,
    emit_script(Repo, Entry, TypeTrees),
    navtheme:emit_main_close,
    navtheme:emit_theme_script,
    navtheme:emit_body_close.


% -----------------------------------------------------------------------------
%  HTML emission - document structure
% -----------------------------------------------------------------------------

emit_subtitle_placeholder :-
    write('<p class="subtitle" id="subtitle"></p>'), nl.


% -----------------------------------------------------------------------------
%  HTML emission - dep type tabs
% -----------------------------------------------------------------------------

emit_dep_tabs :-
    write('<div class="dep-tabs">'), nl,
    write('  <div class="filter-row">'), nl,
    write('    <button class="dep-tab off" data-type="bdepend" onclick="switchDepType(\'bdepend\')">BDEPEND</button>'), nl,
    write('    <button class="dep-tab off" data-type="cdepend" onclick="switchDepType(\'cdepend\')">CDEPEND</button>'), nl,
    write('    <button class="dep-tab off" data-type="depend" onclick="switchDepType(\'depend\')">DEPEND</button>'), nl,
    write('    <button class="dep-tab off" data-type="idepend" onclick="switchDepType(\'idepend\')">IDEPEND</button>'), nl,
    write('    <button class="dep-tab active" data-type="rdepend" onclick="switchDepType(\'rdepend\')">RDEPEND</button>'), nl,
    write('    <button class="dep-tab off" data-type="pdepend" onclick="switchDepType(\'pdepend\')">PDEPEND</button>'), nl,
    write('  </div>'), nl,
    write('  <div class="filter-toolbar">'), nl,
    write('    <button class="action-btn" onclick="expandAll()">Expand All</button>'), nl,
    write('    <button class="action-btn" onclick="collapseAll()">Collapse All</button>'), nl,
    write('    <button class="action-btn" onclick="resetView()">Reset</button>'), nl,
    write('  </div>'), nl,
    write('</div>'), nl.


% -----------------------------------------------------------------------------
%  HTML emission - graph and tooltip containers
% -----------------------------------------------------------------------------

emit_graph_container :-
    write('<div class="graph-container" id="graph-container">'), nl,
    write('  <svg id="graph-svg" xmlns="http://www.w3.org/2000/svg" preserveAspectRatio="xMidYMid meet"></svg>'), nl,
    write('  <div class="zoom-controls">'), nl,
    write('    <button class="zoom-btn" onclick="zoomIn()">+</button>'), nl,
    write('    <span class="zoom-level" id="zoom-level">100%</span>'), nl,
    write('    <button class="zoom-btn" onclick="zoomOut()">&minus;</button>'), nl,
    write('    <button class="zoom-btn" onclick="resetView()" title="Fit to view">&#8634;</button>'), nl,
    write('  </div>'), nl,
    write('</div>'), nl.

emit_tooltip_container :-
    write('<div class="tooltip" id="tooltip"></div>'), nl.


% -----------------------------------------------------------------------------
%  HTML emission - legend
% -----------------------------------------------------------------------------

emit_legend :-
    write('<div class="legend">'), nl,
    write('  <div class="legend-item"><div class="legend-swatch" style="background:#fff;border-color:var(--node-root-border);border-width:2px"></div>root package</div>'), nl,
    write('  <div class="legend-item"><div class="legend-swatch" style="background:#fff;border-color:#ccc;border-left:3px solid var(--node-installed)"></div>installed</div>'), nl,
    write('  <div class="legend-item"><div class="legend-swatch" style="background:#fff;border-color:#ccc"></div>not installed</div>'), nl,
    write('  <div class="legend-item"><svg width="30" height="12"><line x1="0" y1="6" x2="24" y2="6" stroke="var(--edge-normal)" stroke-width="1.5"/><polygon points="24,3 30,6 24,9" fill="var(--edge-normal)"/></svg>dependency</div>'), nl,
    write('  <div class="legend-item"><svg width="30" height="12"><line x1="0" y1="6" x2="24" y2="6" stroke="var(--edge-weak)" stroke-width="1.5" stroke-dasharray="4,2"/><polygon points="24,3 30,6 24,9" fill="var(--edge-weak)"/></svg>weak blocker</div>'), nl,
    write('  <div class="legend-item"><svg width="30" height="12"><line x1="0" y1="6" x2="24" y2="6" stroke="var(--edge-strong)" stroke-width="1.5" stroke-dasharray="2,2"/><polygon points="24,3 30,6 24,9" fill="var(--edge-strong)"/></svg>strong blocker</div>'), nl,
    write('</div>'), nl.


% -----------------------------------------------------------------------------
%  HTML emission - JavaScript
% -----------------------------------------------------------------------------

emit_script(Repo, RootEntry, TypeTrees) :-
    write('<script>'), nl,
    emit_js_tree_data(Repo, RootEntry, TypeTrees),
    emit_js_engine(RootEntry),
    write('</script>'), nl.


%! deptree:emit_js_tree_data(+Repo, +RootEntry, +TypeTrees)
%
% Emit the treeData JavaScript object containing nodes, edges, and metadata
% for each dep type.

emit_js_tree_data(Repo, RootEntry, TypeTrees) :-
    write('const treeData = {'), nl,
    emit_js_type_entries(Repo, RootEntry, TypeTrees),
    write('};'), nl.

emit_js_type_entries(_, _, []).
emit_js_type_entries(Repo, RootEntry, [deptree(Type, Nodes, Edges)|Rest]) :-
    format('  ~w: {~n', [Type]),
    write('    nodes: ['), nl,
    emit_js_nodes(Repo, Nodes),
    write('    ],'), nl,
    write('    edges: ['), nl,
    emit_js_edges(Edges),
    write('    ],'), nl,
    format('    root: "~w"~n', [RootEntry]),
    (   Rest == []
    ->  write('  }'), nl
    ;   write('  },'), nl
    ),
    emit_js_type_entries(Repo, RootEntry, Rest).


%! deptree:emit_js_nodes(+Repo, +Nodes)
%
% Emit JavaScript node objects with embedded metadata.

emit_js_nodes(_, []).
emit_js_nodes(Repo, [node(Entry, Cat, Name, Ver, Slot, Installed)|Rest]) :-
    node_meta(Repo, Entry, Desc, Homepage, UseFlags),
    navtheme:js_escape_atom(Entry, EEntry),
    navtheme:js_escape_atom(Cat, ECat),
    navtheme:js_escape_atom(Name, EName),
    navtheme:js_escape_atom(Ver, EVer),
    navtheme:js_escape_atom(Slot, ESlot),
    navtheme:js_escape_atom(Desc, EDesc),
    navtheme:js_escape_atom(Homepage, EHomepage),
    (Installed == true -> InsStr = "true" ; InsStr = "false"),
    format('      {id:"~w",cat:"~w",name:"~w",ver:"~w",slot:"~w",installed:~w,desc:"~w",homepage:"~w",use:[',
           [EEntry, ECat, EName, EVer, ESlot, InsStr, EDesc, EHomepage]),
    emit_js_use_flags(UseFlags),
    (   Rest == []
    ->  write(']}'), nl
    ;   write(']},'), nl
    ),
    emit_js_nodes(Repo, Rest).

emit_js_use_flags([]).
emit_js_use_flags([flag(Name, OnOff)|Rest]) :-
    (OnOff == on -> Prefix = "+" ; Prefix = "-"),
    navtheme:js_escape_atom(Name, EName),
    format('"~w~w"', [Prefix, EName]),
    (Rest \== [] -> write(',') ; true),
    emit_js_use_flags(Rest).


%! deptree:emit_js_edges(+Edges)
%
% Emit JavaScript edge objects.

emit_js_edges([]).
emit_js_edges([edge(From, To, Strength)|Rest]) :-
    navtheme:js_escape_atom(From, EFrom),
    navtheme:js_escape_atom(To, ETo),
    format('      {from:"~w",to:"~w",type:"~w"}', [EFrom, ETo, Strength]),
    (   Rest == []
    ->  nl
    ;   write(','), nl
    ),
    emit_js_edges(Rest).


%! deptree:emit_js_engine(+RootEntry)
%
% Emit the JavaScript layout engine, rendering, and interaction code.

emit_js_engine(_RootEntry) :-
    write('let currentType = "rdepend";'), nl,
    write('let collapsed = new Set();'), nl,
    write('let viewBox = {x:0, y:0, w:0, h:0};'), nl,
    write('let zoom = 1;'), nl,
    write('let isPanning = false, panStart = {x:0,y:0}, vbStart = {x:0,y:0};'), nl,
    write('let autoCollapsed = false;'), nl,
    write('const CARD_W = 130, CARD_H = 44, LAYER_H = 90, LAYER_W = 210, MIN_GAP = 16;'), nl,
    write('const MIN_SCALE = 0.75, MAX_SCALE = 1.5;'), nl,
    write('const AUTO_COLLAPSE_THRESHOLD = 60;'), nl,
    write('const svg = document.getElementById("graph-svg");'), nl,
    write('const container = document.getElementById("graph-container");'), nl,
    write('const tooltip = document.getElementById("tooltip");'), nl,
    write('const ns = "http://www.w3.org/2000/svg";'), nl,
    write('function getTree() { return treeData[currentType]; }'), nl,
    emit_js_auto_collapse,
    emit_js_build_layout,
    emit_js_resolve_overlaps,
    emit_js_precompute_descendants,
    emit_js_fit_view,
    emit_js_render,
    emit_js_tooltip,
    emit_js_controls,
    emit_js_pan_zoom,
    write('render();'), nl.

emit_js_auto_collapse :-
    write('function autoCollapseIfLarge() {'), nl,
    write('  const tree = getTree();'), nl,
    write('  if (!tree || tree.nodes.length <= AUTO_COLLAPSE_THRESHOLD) return;'), nl,
    write('  const childMap = {};'), nl,
    write('  tree.nodes.forEach(n => childMap[n.id] = []);'), nl,
    write('  tree.edges.forEach(e => { if (childMap[e.from]) childMap[e.from].push(e.to); });'), nl,
    write('  const directChildren = new Set(childMap[tree.root]||[]);'), nl,
    write('  tree.nodes.forEach(n => {'), nl,
    write('    if ((childMap[n.id]||[]).length > 0 && n.id !== tree.root && !directChildren.has(n.id)) collapsed.add(n.id);'), nl,
    write('  });'), nl,
    write('  autoCollapsed = true;'), nl,
    write('}'), nl,
    write('autoCollapseIfLarge();'), nl.

emit_js_build_layout :-
    write('function pickRankdir(layers, maxDepth) {'), nl,
    write('  let maxLayer = 1;'), nl,
    write('  Object.values(layers).forEach(ids => { if (ids.length > maxLayer) maxLayer = ids.length; });'), nl,
    write('  const nLevels = maxDepth + 1;'), nl,
    write('  const tdW = Math.max(maxLayer * (CARD_W + MIN_GAP) - MIN_GAP, CARD_W);'), nl,
    write('  const tdH = Math.max(nLevels * LAYER_H, CARD_H);'), nl,
    write('  const lrW = Math.max(nLevels * LAYER_W, CARD_W);'), nl,
    write('  const lrH = Math.max(maxLayer * (CARD_H + MIN_GAP) - MIN_GAP, CARD_H);'), nl,
    write('  const cW = container.clientWidth || tdW, cH = container.clientHeight || tdH;'), nl,
    write('  const tdScale = Math.min(cW / tdW, cH / tdH, 1);'), nl,
    write('  const lrScale = Math.min(cW / lrW, cH / lrH, 1);'), nl,
    write('  return lrScale > tdScale + 0.05 ? "lr" : "tb";'), nl,
    write('}'), nl,
    write('function buildLayout() {'), nl,
    write('  const empty = {positions:{},hiddenSet:new Set(),childMap:{},nodeMap:{},rankdir:"tb",maxDepth:0};'), nl,
    write('  const tree = getTree();'), nl,
    write('  if (!tree || !tree.nodes.length) return empty;'), nl,
    write('  const childMap = {}, nodeMap = {};'), nl,
    write('  tree.nodes.forEach(n => { nodeMap[n.id] = n; childMap[n.id] = []; });'), nl,
    write('  tree.edges.forEach(e => { if (childMap[e.from]) childMap[e.from].push(e.to); });'), nl,
    write('  const hiddenSet = new Set();'), nl,
    write('  function markHidden(id) { (childMap[id]||[]).forEach(c => { hiddenSet.add(c); markHidden(c); }); }'), nl,
    write('  collapsed.forEach(id => markHidden(id));'), nl,
    write('  const layers = {};'), nl,
    write('  const visited = new Set();'), nl,
    write('  function bfs(startId) {'), nl,
    write('    const queue = [{id: startId, depth: 0}];'), nl,
    write('    visited.add(startId);'), nl,
    write('    while (queue.length) {'), nl,
    write('      const {id, depth} = queue.shift();'), nl,
    write('      if (!layers[depth]) layers[depth] = [];'), nl,
    write('      layers[depth].push(id);'), nl,
    write('      (childMap[id]||[]).forEach(c => {'), nl,
    write('        if (!visited.has(c) && !hiddenSet.has(c)) { visited.add(c); queue.push({id:c, depth:depth+1}); }'), nl,
    write('      });'), nl,
    write('    }'), nl,
    write('  }'), nl,
    write('  bfs(tree.root);'), nl,
    write('  const positions = {};'), nl,
    write('  const maxDepth = Math.max(0, ...Object.keys(layers).map(Number));'), nl,
    write('  const rankdir = pickRankdir(layers, maxDepth);'), nl,
    write('  const axis = rankdir === "lr" ? "y" : "x";'), nl,
    write('  const size = rankdir === "lr" ? CARD_H : CARD_W;'), nl,
    write('  const layerSpan = {};'), nl,
    write('  for (let d = maxDepth; d >= 0; d--) {'), nl,
    write('    const ids = layers[d] || [];'), nl,
    write('    layerSpan[d] = ids.length * size + Math.max(0, ids.length - 1) * MIN_GAP;'), nl,
    write('  }'), nl,
    write('  const maxSpan = Math.max(...Object.values(layerSpan), size);'), nl,
    write('  for (let d = maxDepth; d >= 0; d--) {'), nl,
    write('    const ids = layers[d] || [];'), nl,
    write('    if (ids.length === 0) continue;'), nl,
    write('    const total = ids.length * size + Math.max(0, ids.length - 1) * MIN_GAP;'), nl,
    write('    const start = (maxSpan - total) / 2;'), nl,
    write('    ids.forEach((id, i) => {'), nl,
    write('      const along = start + i * (size + MIN_GAP) + size / 2;'), nl,
    write('      positions[id] = rankdir === "lr"'), nl,
    write('        ? {x: d * LAYER_W + CARD_W / 2 + 24, y: along}'), nl,
    write('        : {x: along, y: d * LAYER_H + CARD_H / 2 + 24};'), nl,
    write('    });'), nl,
    write('  }'), nl,
    write('  for (let pass = 0; pass < 4; pass++) {'), nl,
    write('    for (let d = maxDepth; d >= 0; d--) {'), nl,
    write('      const ids = layers[d] || [];'), nl,
    write('      ids.forEach(id => {'), nl,
    write('        const children = (childMap[id]||[]).filter(c => !hiddenSet.has(c) && positions[c]);'), nl,
    write('        if (children.length > 0) {'), nl,
    write('          positions[id][axis] = children.reduce((s, c) => s + positions[c][axis], 0) / children.length;'), nl,
    write('        }'), nl,
    write('      });'), nl,
    write('      spreadLayer(ids, positions, axis, size);'), nl,
    write('    }'), nl,
    write('    for (let d = 0; d <= maxDepth; d++) {'), nl,
    write('      const ids = layers[d] || [];'), nl,
    write('      ids.forEach(id => {'), nl,
    write('        const children = (childMap[id]||[]).filter(c => !hiddenSet.has(c) && positions[c]);'), nl,
    write('        if (children.length > 0) {'), nl,
    write('          positions[id][axis] = children.reduce((s, c) => s + positions[c][axis], 0) / children.length;'), nl,
    write('        }'), nl,
    write('      });'), nl,
    write('      spreadLayer(ids, positions, axis, size);'), nl,
    write('    }'), nl,
    write('  }'), nl,
    write('  return {positions, hiddenSet, childMap, nodeMap, rankdir, maxDepth};'), nl,
    write('}'), nl.

emit_js_resolve_overlaps :-
    write('function spreadLayer(ids, positions, axis, size) {'), nl,
    write('  if (!ids || ids.length <= 1) return;'), nl,
    write('  for (let i = 1; i < ids.length; i++) {'), nl,
    write('    const prev = positions[ids[i-1]], curr = positions[ids[i]];'), nl,
    write('    const min = prev[axis] + size + MIN_GAP;'), nl,
    write('    if (curr[axis] < min) curr[axis] = min;'), nl,
    write('  }'), nl,
    write('}'), nl.

emit_js_precompute_descendants :-
    write('function precomputeDescendants(childMap) {'), nl,
    write('  const counts = {};'), nl,
    write('  const memo = {};'), nl,
    write('  function dfs(id) {'), nl,
    write('    if (memo[id] !== undefined) return memo[id];'), nl,
    write('    memo[id] = 0;'), nl,
    write('    let c = 0;'), nl,
    write('    (childMap[id]||[]).forEach(ch => { c += 1 + dfs(ch); });'), nl,
    write('    memo[id] = c;'), nl,
    write('    return c;'), nl,
    write('  }'), nl,
    write('  Object.keys(childMap).forEach(id => { counts[id] = dfs(id); });'), nl,
    write('  return counts;'), nl,
    write('}'), nl.

emit_js_render :-
    write('function render() {'), nl,
    write('  const tree = getTree();'), nl,
    write('  if (!tree || !tree.nodes.length) { svg.innerHTML = ""; document.getElementById("subtitle").textContent = "No dependencies"; return; }'), nl,
    write('  const {positions, hiddenSet, childMap, nodeMap, rankdir, maxDepth} = buildLayout();'), nl,
    write('  const descCounts = precomputeDescendants(childMap);'), nl,
    write('  fitView(positions);'), nl,
    write('  const parts = [];'), nl,
    write('  parts.push("<defs>");'), nl,
    write('  ["normal","weak","strong"].forEach(t => {'), nl,
    write('    const col = t === "weak" ? "var(--edge-weak)" : t === "strong" ? "var(--edge-strong)" : "var(--edge-normal)";'), nl,
    write('    parts.push(`<marker id="arrow-${t}" markerWidth="7" markerHeight="5" refX="7" refY="2.5" orient="auto"><polygon points="0 0,7 2.5,0 5" fill="${col}"/></marker>`);'), nl,
    write('  });'), nl,
    write('  parts.push("</defs><g class=\\"edge-layer\\">");'), nl,
    emit_js_render_edges,
    write('  parts.push("</g><g class=\\"node-layer\\">");'), nl,
    emit_js_render_nodes,
    write('  parts.push("</g>");'), nl,
    write('  svg.innerHTML = parts.join("");'), nl,
    emit_js_render_stats,
    write('}'), nl,
    emit_js_event_delegation.

emit_js_fit_view :-
    write('function fitView(positions) {'), nl,
    write('  let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;'), nl,
    write('  Object.values(positions).forEach(p => {'), nl,
    write('    const x0 = p.x - CARD_W / 2, x1 = p.x + CARD_W / 2;'), nl,
    write('    const y0 = p.y - CARD_H / 2, y1 = p.y + CARD_H / 2;'), nl,
    write('    if (x0 < minX) minX = x0; if (x1 > maxX) maxX = x1;'), nl,
    write('    if (y0 < minY) minY = y0; if (y1 > maxY) maxY = y1;'), nl,
    write('  });'), nl,
    write('  if (minX === Infinity) { minX = 0; minY = 0; maxX = 400; maxY = 200; }'), nl,
    write('  const pad = 36;'), nl,
    write('  const contentW = Math.max(maxX - minX, CARD_W) + pad * 2;'), nl,
    write('  const contentH = Math.max(maxY - minY, CARD_H) + pad * 2;'), nl,
    write('  const cW = container.clientWidth || contentW, cH = container.clientHeight || contentH;'), nl,
    write('  let scale = Math.min(cW / contentW, cH / contentH);'), nl,
    write('  if (!isFinite(scale) || scale <= 0) scale = 1;'), nl,
    write('  scale = Math.max(MIN_SCALE, Math.min(MAX_SCALE, scale));'), nl,
    write('  const viewW = cW / scale, viewH = cH / scale;'), nl,
    write('  const cx = (minX + maxX) / 2, cy = (minY + maxY) / 2;'), nl,
    write('  viewBox = {x: cx - viewW / 2, y: cy - viewH / 2, w: viewW, h: viewH};'), nl,
    write('  zoom = 1;'), nl,
    write('  svg.setAttribute("viewBox", `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`);'), nl,
    write('  const zl = document.getElementById("zoom-level");'), nl,
    write('  if (zl) zl.textContent = "100%";'), nl,
    write('}'), nl.

emit_js_render_edges :-
    write('  tree.edges.forEach(e => {'), nl,
    write('    if (hiddenSet.has(e.to) || !positions[e.from] || !positions[e.to]) return;'), nl,
    write('    const fp = positions[e.from], tp = positions[e.to];'), nl,
    write('    let d;'), nl,
    write('    if (rankdir === "lr") {'), nl,
    write('      const x1 = fp.x + CARD_W / 2, y1 = fp.y, x2 = tp.x - CARD_W / 2, y2 = tp.y;'), nl,
    write('      const mx = (x1 + x2) / 2;'), nl,
    write('      d = `M${x1},${y1} C${mx},${y1} ${mx},${y2} ${x2},${y2}`;'), nl,
    write('    } else {'), nl,
    write('      const x1 = fp.x, y1 = fp.y + CARD_H / 2, x2 = tp.x, y2 = tp.y - CARD_H / 2;'), nl,
    write('      const my = (y1 + y2) / 2;'), nl,
    write('      d = `M${x1},${y1} C${x1},${my} ${x2},${my} ${x2},${y2}`;'), nl,
    write('    }'), nl,
    write('    const col = e.type === "weak" ? "var(--edge-weak)" : e.type === "strong" ? "var(--edge-strong)" : "var(--edge-normal)";'), nl,
    write('    const mt = e.type === "no" ? "normal" : e.type;'), nl,
    write('    let extra = "";'), nl,
    write('    if (e.type === "weak") extra = '' stroke-dasharray="5,3"'';'), nl,
    write('    if (e.type === "strong") extra = '' stroke-dasharray="3,2"'';'), nl,
    write('    parts.push(`<path d="${d}" stroke="${col}" stroke-width="1.3" fill="none" marker-end="url(#arrow-${mt})" opacity="0.6"${extra}/>`);'), nl,
    write('  });'), nl.

emit_js_render_nodes :-
    write('  tree.nodes.forEach(n => {'), nl,
    write('    if (hiddenSet.has(n.id) || !positions[n.id]) return;'), nl,
    write('    const pos = positions[n.id], isRoot = n.id === tree.root;'), nl,
    write('    const isColl = collapsed.has(n.id);'), nl,
    write('    const cc = (childMap[n.id]||[]).length;'), nl,
    write('    const dc = descCounts[n.id]||0;'), nl,
    write('    const x = pos.x - CARD_W / 2, y = pos.y - CARD_H / 2;'), nl,
    write('    const cursor = cc > 0 ? "pointer" : "default";'), nl,
    write('    let s = `<g class="node-g" data-id="${n.id}" style="cursor:${cursor}">`;'), nl,
    write('    if (n.installed) {'), nl,
    write('      s += `<rect x="${x}" y="${y}" width="3" height="${CARD_H}" fill="var(--node-installed)" rx="2"/>`;'), nl,
    write('    }'), nl,
    write('    const rx = n.installed ? x + 3 : x, rw = n.installed ? CARD_W - 3 : CARD_W;'), nl,
    write('    const sc = isRoot ? "var(--node-root-border)" : "var(--node-border)";'), nl,
    write('    const sw = isRoot ? 2 : 1.5;'), nl,
    write('    const op = isColl ? '' opacity="0.7"'' : "";'), nl,
    write('    s += `<rect x="${rx}" y="${y}" width="${rw}" height="${CARD_H}" rx="5" ry="5" fill="var(--node-bg)" stroke="${sc}" stroke-width="${sw}"${op}/>`;'), nl,
    write('    s += `<text x="${pos.x}" y="${pos.y - 4}" text-anchor="middle" font-size="11" font-weight="600" fill="var(--text)" font-family="Helvetica Neue,Helvetica,sans-serif">${n.name}</text>`;'), nl,
    write('    const vt = n.ver + (n.slot !== "0" ? "  :" + n.slot : "");'), nl,
    write('    s += `<text x="${pos.x}" y="${pos.y + 10}" text-anchor="middle" font-size="10" fill="var(--text2)" font-family="Helvetica Neue,Helvetica,sans-serif">${vt}</text>`;'), nl,
    write('    if (cc > 0) {'), nl,
    write('      const fc = isColl ? "var(--rdepend)" : "var(--text2)";'), nl,
    write('      const fw = isColl ? "600" : "normal";'), nl,
    write('      const txt = isColl ? "+" + dc : "\\u25BE";'), nl,
    write('      s += `<text x="${pos.x + CARD_W/2 - 6}" y="${pos.y + CARD_H/2 - 3}" text-anchor="end" font-size="9" fill="${fc}" font-weight="${fw}" font-family="Helvetica Neue,Helvetica,sans-serif">${txt}</text>`;'), nl,
    write('    }'), nl,
    write('    s += "</g>";'), nl,
    write('    parts.push(s);'), nl,
    write('  });'), nl.

emit_js_render_stats :-
    write('  let visN = 0, visE = 0;'), nl,
    write('  tree.nodes.forEach(n => { if (!hiddenSet.has(n.id) && positions[n.id]) visN++; });'), nl,
    write('  tree.edges.forEach(e => { if (!hiddenSet.has(e.to) && positions[e.from] && positions[e.to]) visE++; });'), nl,
    write('  const levels = Object.keys(positions).length ? maxDepth + 1 : 0;'), nl,
    write('  const total = tree.nodes.length;'), nl,
    write('  let sub = `${visN} nodes \\u00b7 ${visE} edges \\u00b7 ${levels} levels`;'), nl,
    write('  if (visN < total) sub += ` (${total} total)`;'), nl,
    write('  document.getElementById("subtitle").textContent = sub;'), nl.

emit_js_event_delegation :-
    write('const nodeIndex = {};'), nl,
    write('Object.values(treeData).forEach(t => t.nodes.forEach(n => nodeIndex[n.id] = n));'), nl,
    write('let lastChildMap = null;'), nl,
    write('function getChildMap() {'), nl,
    write('  if (lastChildMap && lastChildMap._type === currentType) return lastChildMap;'), nl,
    write('  const tree = getTree(), cm = {};'), nl,
    write('  tree.nodes.forEach(n => cm[n.id] = []);'), nl,
    write('  tree.edges.forEach(e => { if (cm[e.from]) cm[e.from].push(e.to); });'), nl,
    write('  cm._type = currentType; lastChildMap = cm; return cm;'), nl,
    write('}'), nl,
    write('svg.addEventListener("click", ev => {'), nl,
    write('  const g = ev.target.closest(".node-g"); if (!g) return;'), nl,
    write('  const id = g.dataset.id, cm = getChildMap();'), nl,
    write('  if (!(cm[id]||[]).length) return;'), nl,
    write('  if (collapsed.has(id)) collapsed.delete(id); else collapsed.add(id);'), nl,
    write('  render();'), nl,
    write('});'), nl,
    write('svg.addEventListener("mouseover", ev => {'), nl,
    write('  const g = ev.target.closest(".node-g"); if (!g) return;'), nl,
    write('  const n = nodeIndex[g.dataset.id]; if (!n) return;'), nl,
    write('  showTooltip(ev, n);'), nl,
    write('});'), nl,
    write('svg.addEventListener("mousemove", ev => {'), nl,
    write('  if (ev.target.closest(".node-g")) moveTooltip(ev); else hideTooltip();'), nl,
    write('});'), nl,
    write('svg.addEventListener("mouseout", ev => {'), nl,
    write('  const g = ev.target.closest(".node-g");'), nl,
    write('  if (g && !g.contains(ev.relatedTarget)) hideTooltip();'), nl,
    write('});'), nl.

emit_js_tooltip :-
    write('function showTooltip(ev, n) {'), nl,
    write('  const useHtml = (n.use||[]).map(u => {'), nl,
    write('    const on = u.startsWith("+");'), nl,
    write('    return `<span class="use-flag ${on?"on":"off"}">${u}</span>`;'), nl,
    write('  }).join("");'), nl,
    write('  tooltip.innerHTML = `'), nl,
    write('    <div class="tt-title">${n.cat}/${n.name}-${n.ver}</div>'), nl,
    write('    ${n.desc ? `<div class="tt-desc">${n.desc}</div>` : ""}'), nl,
    write('    <div class="tt-row"><span class="tt-label">Slot</span><span class="tt-value">${n.slot}</span></div>'), nl,
    write('    <div class="tt-row"><span class="tt-label">Status</span><span class="${n.installed?"tt-installed":"tt-not-installed"}">${n.installed?"Installed":"Not installed"}</span></div>'), nl,
    write('    ${n.homepage ? `<div class="tt-row"><span class="tt-label">Home</span><a class="tt-link" href="${n.homepage}" target="_blank">${n.homepage.replace(/https?:\\/\\//,"").substring(0,40)}...</a></div>` : ""}'), nl,
    write('    ${useHtml ? `<div style="margin-top:4px"><span class="tt-label">USE</span><div class="use-flags">${useHtml}</div></div>` : ""}'), nl,
    write('  `; tooltip.style.display = "block"; moveTooltip(ev);'), nl,
    write('}'), nl,
    write('function moveTooltip(ev) {'), nl,
    write('  const pad = 12; let x = ev.clientX + pad, y = ev.clientY + pad;'), nl,
    write('  const tw = tooltip.offsetWidth, th = tooltip.offsetHeight;'), nl,
    write('  if (x + tw > window.innerWidth - pad) x = ev.clientX - tw - pad;'), nl,
    write('  if (y + th > window.innerHeight - pad) y = ev.clientY - th - pad;'), nl,
    write('  tooltip.style.left = x + "px"; tooltip.style.top = y + "px";'), nl,
    write('}'), nl,
    write('function hideTooltip() { tooltip.style.display = "none"; }'), nl.

emit_js_controls :-
    write('function switchDepType(type) {'), nl,
    write('  if (!treeData[type]) return;'), nl,
    write('  currentType = type; collapsed.clear(); lastChildMap = null;'), nl,
    write('  autoCollapseIfLarge();'), nl,
    write('  document.querySelectorAll(".dep-tab").forEach(b => {'), nl,
    write('    b.classList.toggle("active", b.dataset.type === type);'), nl,
    write('    b.classList.toggle("off", b.dataset.type !== type);'), nl,
    write('  }); render();'), nl,
    write('}'), nl,
    write('function expandAll() {'), nl,
    write('  const tree = getTree();'), nl,
    write('  if (tree && tree.nodes.length > 500 && !confirm("This tree has " + tree.nodes.length + " nodes. Expanding all may be slow. Continue?")) return;'), nl,
    write('  collapsed.clear(); lastChildMap = null; render();'), nl,
    write('}'), nl,
    write('function collapseAll() {'), nl,
    write('  const tree = getTree(); if (!tree) return;'), nl,
    write('  const cm = getChildMap();'), nl,
    write('  collapsed.clear();'), nl,
    write('  tree.nodes.forEach(n => { if ((cm[n.id]||[]).length > 0) collapsed.add(n.id); });'), nl,
    write('  lastChildMap = null; render();'), nl,
    write('}'), nl,
    write('function resetView() {'), nl,
    write('  collapsed.clear(); lastChildMap = null; autoCollapsed = false;'), nl,
    write('  autoCollapseIfLarge();'), nl,
    write('  render();'), nl,
    write('}'), nl,
    write('function zoomIn() { zoom = Math.min(zoom * 1.25, 4); applyZoom(); }'), nl,
    write('function zoomOut() { zoom = Math.max(zoom / 1.25, 0.25); applyZoom(); }'), nl,
    write('function applyZoom() {'), nl,
    write('  const cx = viewBox.x + viewBox.w / 2, cy = viewBox.y + viewBox.h / 2;'), nl,
    write('  const nw = viewBox.w / zoom, nh = viewBox.h / zoom;'), nl,
    write('  svg.setAttribute("viewBox", `${cx - nw/2} ${cy - nh/2} ${nw} ${nh}`);'), nl,
    write('  document.getElementById("zoom-level").textContent = Math.round(zoom * 100) + "%";'), nl,
    write('}'), nl.

emit_js_pan_zoom :-
    write('container.addEventListener("mousedown", e => {'), nl,
    write('  if (e.target.closest(".node-g")) return;'), nl,
    write('  isPanning = true; panStart = {x: e.clientX, y: e.clientY};'), nl,
    write('  const vb = svg.getAttribute("viewBox").split(" ").map(Number);'), nl,
    write('  vbStart = {x: vb[0], y: vb[1]}; container.style.cursor = "grabbing";'), nl,
    write('});'), nl,
    write('container.addEventListener("mousemove", e => {'), nl,
    write('  if (!isPanning) return;'), nl,
    write('  const vb = svg.getAttribute("viewBox").split(" ").map(Number);'), nl,
    write('  const scale = vb[2] / container.clientWidth;'), nl,
    write('  const dx = (e.clientX - panStart.x) * scale, dy = (e.clientY - panStart.y) * scale;'), nl,
    write('  svg.setAttribute("viewBox", `${vbStart.x - dx} ${vbStart.y - dy} ${vb[2]} ${vb[3]}`);'), nl,
    write('});'), nl,
    write('container.addEventListener("mouseup", () => { isPanning = false; container.style.cursor = ""; });'), nl,
    write('container.addEventListener("mouseleave", () => { isPanning = false; container.style.cursor = ""; });'), nl,
    write('container.addEventListener("wheel", e => {'), nl,
    write('  e.preventDefault();'), nl,
    write('  const factor = e.deltaY > 0 ? 0.9 : 1.1;'), nl,
    write('  zoom *= factor; zoom = Math.max(0.25, Math.min(4, zoom));'), nl,
    write('  const vb = svg.getAttribute("viewBox").split(" ").map(Number);'), nl,
    write('  const rect = container.getBoundingClientRect();'), nl,
    write('  const mx = (e.clientX - rect.left) / rect.width, my = (e.clientY - rect.top) / rect.height;'), nl,
    write('  const nw = vb[2] / factor, nh = vb[3] / factor;'), nl,
    write('  const nx = vb[0] + (vb[2] - nw) * mx, ny = vb[1] + (vb[3] - nh) * my;'), nl,
    write('  svg.setAttribute("viewBox", `${nx} ${ny} ${nw} ${nh}`);'), nl,
    write('  document.getElementById("zoom-level").textContent = Math.round(zoom * 100) + "%";'), nl,
    write('}, {passive: false});'), nl,
    write('let resizeTimer = 0;'), nl,
    write('window.addEventListener("resize", () => {'), nl,
    write('  clearTimeout(resizeTimer);'), nl,
    write('  resizeTimer = setTimeout(render, 120);'), nl,
    write('});'), nl.
