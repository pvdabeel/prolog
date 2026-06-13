/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> CONFIG
Configuration for portage-ng
*/

:- module(config, []).

% =============================================================================
%  CONFIG declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  General
% -----------------------------------------------------------------------------

%! config:name(?Name)
%
% Declares the name of this program.

config:name('portage-ng-dev').


%! config:hostname(?Hostname)
%
% Declares the hostname this program is running on.

config:hostname(Hostname) :- socket:gethostname(Hostname).


%! config:installation_dir(?FullPath)
%
% Declaration of the installation directory of the application source code.
% Needs to be a full path. We serialise some Prolog code to this directory.
% Needs to be passed to prolog as a system flag (See portage-ng.pl)

config:installation_dir(Dir) :-
  file_search_path(portage,Dir),!.


%! config:working_dir(+FullPath)
%
% Declares the current working directory

config:working_dir(Dir) :-
  config:installation_dir(Dir).


% -----------------------------------------------------------------------------
%  Repository
% -----------------------------------------------------------------------------

%! config:trust_metadata(?Bool)
%
% When set to false, we regenerate locally the cache entries
% after syncing with the remote repository. This is expensive,
% so by default we trust and regenerate only for locally changed
% ebuilds or ebuilds with missing cache.

config:trust_metadata(true).


%! config:write_metadata(?Bool)
%
% When ebuilds are changed locally or new ebuilds are created locally
% this will be detected during syncing, and portage will parse the
% ebuild metadata after running it through ebuild.sh.
%
% Prolog facts are updated with the updated metadata.
% If this variable is set to true, we will also create an on-disk
% cache entry for these ebuilds. This cache entry may be overwritten
% by a sync later on by remotely generated cache.

config:write_metadata(true).


% -----------------------------------------------------------------------------
%  Binary package consumption ($PKGDIR/Packages, gpkg multi-instance)
% -----------------------------------------------------------------------------
%
% These knobs control whether the builder consumes matching binpkgs from a
% locally-registered binpkg repository (instead of building from source).
% The check is gated on `cache:repository(binpkg)` being present, so all
% knobs are effectively no-ops on hosts that haven't registered a binpkg
% repository in their Source/Config/<host>.local.pl.

%! config:use_binpkg(?Bool)
%
% Master switch for binpkg consumption. When `true`, the builder asks
% `binpkg_exec:available_for/4` for a USE-matching variant before falling
% back to the source-build phase sequence in `ebuild_exec`. Default: true.

config:use_binpkg(true).


%! config:binpkg_respect_use(?Mode)
%
% Mirrors emerge's `--binpkg-respect-use=y|n`. When `strict`, only binpkgs
% whose stored `USE:` exactly matches the planner's resolved USE (after
% IUSE_EFFECTIVE projection) are eligible. When `relaxed`, USE-mismatched
% binpkgs may still be selected (a warning is logged). Default: strict.

config:binpkg_respect_use(strict).


%! config:binpkg_changed_deps(?Mode)
%
% Mirrors emerge's `--binpkg-changed-deps=y|n`. When `skip`, binpkgs whose
% recorded RDEPEND differs from the current ebuild's RDEPEND are excluded
% from the candidate set. When `warn`, they are still eligible (a warning
% is logged). Default: warn (less restrictive; binpkg-multi-instance
% already isolates USE drift).

config:binpkg_changed_deps(warn).


%! config:binpkg_refresh(?Policy)
%
% Controls when the in-memory binpkg index is refreshed against the
% on-disk `Packages` file:
%
%   manual : never auto-refresh; the index reflects the last `kb:sync(binpkg)`
%            run. Cheapest, fully predictable.
%   mtime  : on each `binpkg_exec:available_for/4` call, stat the index
%            file and re-run sync(kb) if its mtime is newer than the last
%            load. Useful when external producers are adding binpkgs
%            concurrently with portage-ng builds.
%
% Default: mtime so concurrent binpkg producers (tinderbox-ng shared
% cache) are visible without restarting portage-ng.

config:binpkg_refresh(mtime).


% -----------------------------------------------------------------------------
%  Gentoo profile (for Portage parity)
% -----------------------------------------------------------------------------
%
% This is the Gentoo profile path *relative to the Portage tree profiles dir*.
% Example: /usr/portage/profiles/<profile>.
%
% Used by `profile.pl` to generate `preference:profile_use/1` terms.
%
% NOTE: This is intentionally in config (host/system configuration), not in
% preference (user/package preferences).

config:gentoo_profile('default/linux/amd64/23.0/split-usr/no-multilib').

% -----------------------------------------------------------------------------
%  Profile loading strategy
% -----------------------------------------------------------------------------
%
% Controls whether profile data (USE, masks, package.use, license groups)
% is parsed live from the Portage tree at startup, or loaded from a
% pre-serialized cache file (Knowledge/profile.qlf) generated during --sync.
%
% config:profile_loading(+Mode, +Strategy)
%   Mode:     standalone | daemon | worker | client | server
%   Strategy: cached     — load from Knowledge/profile.qlf (fast; requires --sync first)
%             live       — parse profile tree on every preference:init (slower)
%
% When strategy is 'cached' but Knowledge/profile.qlf is missing, falls back to 'live'.

config:profile_loading(standalone, cached).
config:profile_loading(daemon,     cached).
config:profile_loading(worker,     cached).
config:profile_loading(client,     live).
config:profile_loading(server,     cached).

% Materialized preference state (Knowledge/preference.qlf).  Requires kb:load
% before preference:init.  Stamp covers kb.qlf, profile.qlf, /etc/portage,
% world/set files, use.desc, and USE/ACCEPT_* environment variables.

config:preference_cache(standalone, cached).
config:preference_cache(daemon,     cached).
config:preference_cache(worker,     cached).
config:preference_cache(client,     live).
config:preference_cache(server,     cached).

% -----------------------------------------------------------------------------
%  /etc/portage configuration directory
% -----------------------------------------------------------------------------
%
% Path to the Gentoo /etc/portage configuration directory.  userconfig:load/0
% reads make.conf, package.use, package.mask, package.unmask,
% package.accept_keywords, and package.license from this directory.
%
% On a Gentoo system, set this to '/etc/portage'.
% For development, use the bundled templates in Source/Domain/Gentoo/Preference/UserConfig.
% Comment out to disable file-based configuration entirely (falls back to the
% fallback:env/2 and fallback:package_*/1-2 facts in Preference/fallback.pl).

% config:portage_confdir('/etc/portage').

% Fallback environment defaults, package masks, and per-package USE overrides
% are in Source/Domain/Gentoo/Preference/fallback.pl.  They are only consulted
% when config:portage_confdir/1 is not set (no real /etc/portage configured).


% -----------------------------------------------------------------------------
%  Pkg directory
% -----------------------------------------------------------------------------

%! config:pkg_directory(?FullPath)
%
% Declaration of the pkg db directory on a system. This holds metadata
% for all packages installed on a system (`/var/db/pkg` on a standard
% Gentoo system).
%
% Host-specific: defined in Source/Config/<host>.local.pl (the file loaded
% via config:systemconfig/1), NOT here. Callers guard with
% current_predicate/1 so hosts without a clause fail cleanly.


% -----------------------------------------------------------------------------
%  World file
% -----------------------------------------------------------------------------

%! config:world_file(Path)
%
% Declaration of the world file path. The world file holds the requirements
% for installed packages.

config:world_file(Filename) :-
  config:installation_dir(Dir),
  config:hostname(Hostname),
  os:compose_path([Dir,'Source/Knowledge/Sets/world',Hostname],Filename).


%! config:set_dir(-Dir)
%
% Directory containing named set files (one entry per line).
% Each file @Name becomes a preference:local_set('@Name', Entries) fact.

config:set_dir(Dir) :-
  config:installation_dir(Base),
  os:compose_path([Base, 'Source/Knowledge/Sets'], Dir).


% -----------------------------------------------------------------------------
%  Snapshots
% -----------------------------------------------------------------------------

%! config:snapshot_dir(-Dir)
%
% Returns the base directory for snapshots on the current host.

config:snapshot_dir(Dir) :-
  config:installation_dir(Base),
  config:hostname(Hostname),
  os:compose_path([Base, 'Source/Snapshots', Hostname], Dir).


%! config:snapshot_enabled
%
% When asserted, --snapshot is automatically active for every merge.
% Disabled by default. Enable in per-machine config files.

:- dynamic config:snapshot_enabled/0.


% -----------------------------------------------------------------------------
%  System
% -----------------------------------------------------------------------------

%! config:systemconfig(?Filename)
%
% Declares the systemconfig for the host this program is running. This file
% contains system specific configuration settings. The standard setting
% looks whether <hostname>.pl exists in the Source/Config directory. If not
% then Source/Config/default.pl is used. Typically this is used to define the
% different repositories / overlays per hostname.

config:systemconfig(Filename) :-
  config:installation_dir(Dir),
  config:hostname(Hostname),
  os:compose_path([Dir,'Source/Config','default'],Default),
  os:compose_path([Dir,'Source/Config',Hostname],Configuration),
  system:file_name_extension(Configuration,'pl',ConfigFile),
  ( system:exists_file(ConfigFile)
    -> Filename = ConfigFile
    ;  Filename = Default ).


%! config:number_of_cpus(?Count)
%
% This application parallellizes parsing, proving, planning and building.
% SWI prolog is automatically able to determine the maximum cpu count, but
% sometimes we may want to change this to a value of our choice.

config:number_of_cpus(C) :- current_prolog_flag(cpu_count,C).


%! config:verbosity(?Level)
%
% Different verbosity levels may be configured for printing runtime information

config:verbosity(debug).


% -----------------------------------------------------------------------------
%  Passwords
% -----------------------------------------------------------------------------

% The private passwords file is optional. On a fresh checkout (where
% Source/Config/Private/template_passwords.pl has not been copied to
% passwords.pl yet) we fall back to the same empty defaults the template
% ships with, instead of aborting the entire config load.

:- if(exists_source(portage('Source/Config/Private/passwords'))).
:- include(portage('Source/Config/Private/passwords')).
:- else.

%! config:certificate_password(?Key,?Pass)
%
% Default (empty) passwords for the SSL client/server certificates.
% Override by creating Source/Config/Private/passwords.pl from the template.

config:certificate_password(server,'').
config:certificate_password(client,'').


%! config:digest_password(?User,?Pass)
%
% Default (empty) password for digest user authentication.

config:digest_password('portage-ng','').


%! config:digest_realm(?Realm)
%
% Default realm for digest user authentication.

config:digest_realm('portage-ng').

:- endif.


% -----------------------------------------------------------------------------
%  Network
% -----------------------------------------------------------------------------

%! config:bonjour_service(?Service)
%
% The mDNS service used to advertise and discover the server on the network.

config:bonjour_service('_portage-ng._tcp.').


%! config:bonjour_worker_service(?Service)
%
% The mDNS service used by workers to advertise compute capacity.

config:bonjour_worker_service('_portage-ng-worker._tcp.').


%! config:server_host(?Url)
%
% Declares the server url, including protocol (https) and port the client needs
% to connect on. This can be overridden using the --host option in the interface.

config:server_host('mac-pro.local').


%! config:server_port(?Number)
%
% Declares the port on which the server needs to be launched.
% Always launches on localhost, using https/ssl. This can be overridden
% using the --port option in the interface.

config:server_port(4000).


%! config:server_workers(?Count)
%
% Number of HTTP worker threads the server spawns to handle requests.

config:server_workers(32).


%! config:server_keep_alive_timeout(?Seconds)
%
% How long an idle keep-alive HTTP connection is kept open before the
% server closes it.

config:server_keep_alive_timeout(2).


%! config:cluster_result_timeout(?Seconds)
%
% How long a single result poll blocks while collecting distributed
% proving results. On expiry the collector checks for stale in-flight
% jobs (and re-queues them) before polling again.

config:cluster_result_timeout(60).


%! config:cluster_global_deadline(?Seconds)
%
% Overall deadline for collecting all results of a distributed proving
% batch. When reached, cluster:wait/1 returns the results collected so
% far and warns about missing ones instead of waiting forever.

config:cluster_global_deadline(3600).


%! config:cluster_worker_timeout(?Seconds)
%
% A worker is presumed dead when it has not contacted the server for
% this many seconds; its in-flight jobs are then re-queued for other
% workers. Should exceed the longest expected single prove job, since a
% busy single-threaded worker only heartbeats between jobs.

config:cluster_worker_timeout(900).


%! config:server_chunk(?Chunksize)
%
% When set to false, when working in client-server mode, one RPC call is
% performed per solution. When generating large number of solutions, such as
% backtracking over a goal, it is best to transfer multiple solutions in one
% chunk over one rpc call.

% Setting this to a higher value speeds up searching in client-server mode.
% Shouldn't be higher than the total number of entries in your repositories.

config:server_chunk(50000).


%! config:client_auto_import_vdb(?Bool)
%
% When true, client mode automatically (re-)imports the local VDB into the
% server before executing a command (--pretend, --merge, --search, ...):
% the import runs when no import record exists for the target server, or
% when the local VDB changed since the last import (cheap mtime check over
% the VDB root and its category directories). When false, the VDB is only
% shipped explicitly via --import-vdb. See knowledgebase:vdb_repository/1.

config:client_auto_import_vdb(true).


% -----------------------------------------------------------------------------
%  Certificates
% -----------------------------------------------------------------------------

%! config:initialize_cacert
%
% Initializes the certificate authority. We use certificates in our
% client-server communication.

config:initialize_cacert :-
  config:certificate('cacert.pem',Fullpath),
  create_prolog_flag(system_cacert_filename,Fullpath,[access(read_only)]).


%! config:certificate(+Certificate,-Fullpath)
%
% Returns an absolute path for a given certificate name

config:certificate(Certificate,Fullpath) :-
  config:installation_dir(Dir),
  os:compose_path([Dir,'Certificates',Certificate],Fullpath).


%! config:certificate(+Hostname,+Certificate,-Fullpath)
%
% Return an absolute path for a given hostname certificate name

config:certificate(Hostname,Certificate,FullPath) :-
  atomic_list_concat([Hostname,Certificate],'.',HostCertificate),
  config:certificate(HostCertificate,FullPath).


%! config:digest_passwordfile(?File)
%
% Declares the password file for http(s) digest user authentication

config:digest_passwordfile(Filename) :-
  config:installation_dir(Dir),
  os:compose_path([Dir,'Certificates/passwordfile'],Filename).


% -----------------------------------------------------------------------------
%  Graphing
% -----------------------------------------------------------------------------

% Interface can dynamically override graphing behavior for a single run.
% (Used by CLI flags like --graph-modified / --graph-full.)

:- dynamic config:interface_graph_modified_only/1.

%! config:graph_directory(?FullPath)
%
% This application is capable of writing Graphviz dot files and will turn
% them into interactive scalable vector graphics (svg) to enable you to
% browse through a dependency graph.
%
% Directory doesn't need a trailing '/'.
%
% Host-specific: defined in Source/Config/<host>.local.pl (the file loaded
% via config:systemconfig/1), NOT here. Callers guard with
% current_predicate/1 so hosts without a clause fail cleanly.


%! config:graph_modified_only(?Bool)
%
% Set when you want Graphviz dot file to be created for new ebuilds only

% Note: call sites often query this as `config:graph_modified_only(true)` in an
% if-then-else. Therefore this predicate must *not* have an unconditional
% `.../1` clause that makes `config:graph_modified_only(true)` succeed when the
% effective value is false.
%
% Resolution order:
% - CLI/runtime override (dynamic) via config:interface_graph_modified_only/1
% - Default (can be changed by editing config:graph_modified_only_default/1)

config:graph_modified_only(Bool) :-
  ( config:interface_graph_modified_only(Bool0) ->
      Bool = Bool0
  ; config:graph_modified_only_default(Bool)
  ).

% Default graphing behavior (when CLI didn't override):
% true = graph only modified/new ebuilds
% false = graph everything

config:graph_modified_only_default(true).


% -----------------------------------------------------------------------------
%  Emerge file generation (--graph emerge)
% -----------------------------------------------------------------------------

% Interface can dynamically force full regeneration for a single run.
% (Used by `--graph emerge full`.)

:- dynamic config:force_emerge_regen/1.

%! config:emerge_vp_path(?Path)
%
% Per-host path to the `emerge-vp` wrapper used by `--graph emerge` to
% generate `.emerge` files alongside the `.merge` files in the graph
% directory. The wrapper invokes Gentoo's traditional `emerge -vp` so
% portage-ng can render side-by-side comparisons in HTML.
%
% Host-specific: defined in Source/Config/<host>.local.pl (the file loaded
% via config:systemconfig/1), NOT here. When the host config defines no
% clause, `--graph emerge` prints a clear warning instead of writing
% `.emerge` files.


%! config:emerge_vp_timeout(?Seconds)
%
% Per-ebuild timeout for emerge-vp invocations. Mirrors the legacy
% generate-emerge-files.sh default.

config:emerge_vp_timeout(120).


%! config:emerge_vp_concurrency(?N)
%
% Number of parallel emerge-vp workers spawned by `--graph emerge`.
% Defaults to 1 (matches the legacy shell). Increase cautiously: emerge-vp
% inside a gentoo-prefix is fork-heavy and concurrent runs can saturate I/O.

config:emerge_vp_concurrency(1).


%! config:graph_include_emerge(?Bool)
%
% When true, the default `--graph` run also generates `.emerge` files at
% the end of the pipeline (after writing proof files). False by default;
% use `--graph emerge` for explicit regeneration. Set to true on hosts
% where the gentoo-prefix emerge-vp wrapper is available and you want
% one-command refresh of the entire graph directory.

config:graph_include_emerge(false).


%! config:graph_html_type(?List)
%
% Defines all HTML graph types produced by --graph.  Each type maps to a
% self-contained interactive HTML file per ebuild.

config:graph_html_type([detail,deptree,gantt,merge,fetchonly,info,emerge]).


%! config:graph_dependency_type(?List)
%
% Legacy: dependency types for which DOT graphs can be produced.
% No longer part of the default --graph output (superseded by deptree + detail).

config:graph_dependency_type([detail,
                              bdepend,
                              cdepend,
                              depend,
                              idepend,
                              rdepend,
                              pdepend]).


%! config:graph_proof_type(?List)
%
% Legacy: proof types for which DOT graphs can be produced.
% No longer part of the default --graph output (superseded by terminal HTML).

config:graph_proof_type([merge,fetchonly,info]).


%! config:graph_legacy_type(?List)
%
% Legacy: legacy types for which DOT graphs can be produced.
% No longer part of the default --graph output (superseded by terminal HTML).

config:graph_legacy_type([emerge]).


% -----------------------------------------------------------------------------
%  Cycle / SCC printing
% -----------------------------------------------------------------------------

%! config:print_prover_cycles(?Bool)
%
% Whether to print cycle-break explanations (DFS/BFS cycle paths) for
% prover cycle-break assumptions in plan output.  These assumptions
% already appear in the plan as "(assumed running)" / "(assumed
% installed)" line items, so the tree visualisation is redundant.
% Expensive to compute and can produce very large output.

config:print_prover_cycles(false).


%! config:print_prover_cycles_style(?Style)
%
% Display style for the cycle breaks section:
%   - `off`      : do not print cycle breaks section
%   - `flat`     : one cycle break per line, compact list (default)
%   - `detailed` : each cycle break on two lines with "- Cycle break:" header

config:print_prover_cycles_style(flat).


%! config:print_prover_cycles_max_total(?N)
%
% Maximum number of prover cycle-break assumptions for which a cycle
% explanation tree is printed.  Additional cycle breaks are summarised
% as "(… N more cycle breaks omitted)".

config:print_prover_cycles_max_total(10).


%! config:print_prover_cycles_max_depth(?N)
%
% Maximum DFS/BFS search depth when constructing prover cycle paths.
% Lower values produce shorter (but possibly incomplete) cycle trees;
% higher values find longer cycles at the cost of more search time.

config:print_prover_cycles_max_depth(25).


%! config:print_scc(?Bool)
%
% Whether to print the scheduler's SCC (strongly connected component)
% decomposition in plan output.  Shows which packages form cyclic
% merge-sets in the planner remainder and the linearization order
% chosen by the scheduler.

config:print_scc(false).


%! config:print_scc_max_members(?N)
%
% Maximum number of SCC members to display per component.
% Components larger than this show only the first N members with
% a summary count.

config:print_scc_max_members(50).


%! config:print_blockers(?Style)
%
% How to display blocker assumptions in plan output.
% Blockers are separated from other domain assumptions into their own
% section.
%
% Styles:
%   off    - no blocker section printed
%   gentoo - compact Portage-like dark-gray lines
%   fancy  - detailed multi-line assumption format with provenance

config:print_blockers(gentoo).


% -----------------------------------------------------------------------------
%  Graphing: static assets
% -----------------------------------------------------------------------------


% When generating HTML (index pages + proofs) we copy a few static assets into the
% repository graph directory (the dir created by repository:prepare_directory/1).
%
% Targets (fixed names in output directory):
%
% - .portage-ng.css (shared CSS for all HTML graph pages)
% - .proof.css   (used by Scripts/*/print-aha to render proof output)
% - .meslo.ttf   (font used by proof rendering)
%
% Sources are configurable here (defaults are in Documentation/Assets/Graph/).


%! config:graph_asset_source(+Key, -SourcePath)
%
% Returns the source path for a given asset key

config:graph_asset_source(portage_ng_css, Source) :-
  config:installation_dir(Dir),
  os:compose_path([Dir,'Documentation/Assets/Graph/Stylesheets/portage-ng.css'], Source).

config:graph_asset_source(proof_css, Source) :-
  config:installation_dir(Dir),
  os:compose_path([Dir,'Documentation/Assets/Graph/Stylesheets/proof.css'], Source).

config:graph_asset_source(meslo_ttf, Source) :-
  config:installation_dir(Dir),
  os:compose_path([Dir,'Documentation/Assets/Graph/Fonts/meslo.ttf'], Source).


% -----------------------------------------------------------------------------
%  Distfiles / mirrors (analysis + future downloader)
% -----------------------------------------------------------------------------


%! config:mirror_root(?Path)
%
% Default paths used by mirror:test_stats/1 and related tooling.
% These can be overridden at call-site (mirror:test_stats/2), but having a single
% source of truth here makes behavior consistent across CLI and scripts.
%
% - mirror_root/1: hashed distfiles mirror root (GLEP 75 layout.conf aware)
% - Local distfiles directory is configured per machine via distfiles repository

config:mirror_root('/Volumes/Storage/Distfiles/distfiles').


%! config:mirror_url(?URL) is multi.
%
% HTTP base URL(s) of distfiles mirrors. The mirror has the same GLEP 75
% directory layout as mirror_root/1, served over HTTP. Multiple facts may
% be present; the downloader will try them in declaration order, then fall
% back to the SRC_URI chain (mirror://+thirdpartymirror+direct upstream).
%
% Order: list the fastest / most-complete mirror first. mac-pro.local is a
% private LAN mirror that retains older distfiles which Gentoo's main
% mirror has pruned.

config:mirror_url('http://mac-pro.local/distfiles').
config:mirror_url('https://distfiles.gentoo.org/distfiles').


%! config:mirror_verify_hashes_default(?Policy)
%
% Default hash verification policy for mirror:test_stats:
%
% - none: only check existence + size
% - sample(N): verify hashes for the first N unique distfiles (fast sanity)
% - all: verify hashes for all unique distfiles (expensive)

config:mirror_verify_hashes_default(none).


%! config:mirror_layout_cache_ttl(?Seconds)
%
% TTL (in seconds) for the on-disk cache of the GLEP 75 layout.conf
% reply, written under the active distdir (see download:disk_cache_path/1).
% Mirror layout almost never changes, but clusters of short-lived
% portage-ng processes (external compare matrices, server worker
% pool) would otherwise re-fetch layout.conf for every invocation,
% which both wastes a curl per session (~1s) and adds a real failure
% mode if the mirror flaps. Default 24h is conservative; bump it for
% offline / network-restricted runs.

config:mirror_layout_cache_ttl(86400).


%! config:bugzilla_url(?URL)
%
% Base URL of the Bugzilla instance for --search-bugs (e.g. bugs.gentoo.org).
% Used to query the REST API at <URL>/rest/bug?quicksearch=<term>.

config:bugzilla_url('https://bugs.gentoo.org').


%! config:bugzilla_user_agent(?UA) is det.
%
% User-Agent string for Bugzilla API requests.

config:bugzilla_user_agent('portage-ng/2026 (https://github.com/pvdabeel/portage-ng)').


% -----------------------------------------------------------------------------
%  Proving
% -----------------------------------------------------------------------------

%! config:time_limit(?Limit)
%
% When parsing, proving or planning, use the specified time limit to
% automatically stop computation if it takes too long.
%In seconds.

config:time_limit(300).


%! config:proving_target(?Target)
%
% Fact which controls the test target for prover, planner, printer and builder
% Set to either:
%
%  - 'install' : Proof using compile-time dependencies only
%  - 'run': Proof using compile- and run-time dependencies

config:proving_target(run).


%! config:reprove_max_retries(?Count)
%
% Maximum number of iterative reprove retries.
%
% This controls the bounded retry loop in prover:prove/9 that handles
% prover_reprove(Info) exceptions.
%
% Important distinction from normal Prolog backtracking:
%
% - Normal backtracking explores alternatives *within one proof attempt* and
%   does not persist learned conflict information once it unwinds.
% - Reprove retries are *iterative refinement*: each retry restarts
%   proving from the target, but keeps scoped no-goods learned from earlier
%   failed attempts (e.g. reject candidate X; then retry and potentially add Y,
%   yielding X+Y on the next attempt).
%
% In short: this is a bounded "learn + restart" loop, not plain in-branch
% backtracking.
%
% Tuning:
%
% - 0 disables iterative retries (single pass only).
% - Higher values allow more refinement but can increase runtime.
%
% Recommended default: 20 (Portage-like retry budget)

config:reprove_max_retries(20).


%! config:avoid_reinstall(?Bool)
%
% If a package is already installed, when this config item is set to true,
% we will verify installation, rather than reinstall the package.

config:avoid_reinstall(false).


% -----------------------------------------------------------------------------
%  Printing
% -----------------------------------------------------------------------------

% Interface can dynamically adjust the verbosity

:- dynamic config:verbose/1.

config:verbose(false).


% Color output is enabled by default. Retract to disable ANSI color/style.

:- dynamic config:color_output/0.
:- dynamic config:cli_jobs/1.
:- dynamic config:cli_load_average/1.
:- dynamic config:excluded_atom/1.
:- dynamic config:skip_atom/1.
:- dynamic config:usepkg_exclude_atom/1.
:- dynamic config:usepkg_include_atom/1.
:- dynamic config:dep_favour/1.
:- dynamic config:dep_avoid/1.
:- dynamic config:dep_preset/1.
:- dynamic config:dep_hide/1.
:- dynamic config:dep_early/1.
:- dynamic config:dep_late/1.
:- dynamic config:continue_on_failure/1.
:- dynamic config:build_live_phases/1.
:- dynamic config:show_use_descriptions/1.
:- dynamic config:show_build_logs/1.

config:color_output.


% Color palette for USE flag display in plan output.
%
% easy (default) -- Classic Portage style:
%   Enabled flags:  red bold      (matches emerge -vp output)
%   Disabled flags: blue bold     (matches emerge -vp output)
%   Assumed flags:  orange
%   Forced/masked:  green bold ()
%
% full -- Detailed reason-based coloring:
%   Enabled  preference (env):     green bold + *
%   Enabled  preference (non-env): red bold
%   Enabled  profile forced:       green bold ()
%   Enabled  package_use:          red bold
%   Enabled  ebuild default:       red italic
%   Disabled preference (env):     green bold + *
%   Disabled preference (non-env): blue bold
%   Disabled profile masked:       green bold ()
%   Disabled package_use:          blue bold
%   Disabled ebuild default:       lightblue italic
%   Disabled default:              darkgray italic

:- dynamic config:color_palette/1.

config:color_palette(full).


%! config:output_tty is semidet.
%
% Succeeds when the ultimate output destination is a real TTY.
% In daemon/IPC mode the daemon's user_output is a socket, but
% the client's stdout may be a TTY -- daemon:client_is_tty is
% asserted when the client reports a TTY.

:- dynamic config:output_tty_cached/1.

config:output_tty :-
  ( config:output_tty_cached(Val)
  -> Val == true
  ; ( ( stream_property(user_output, tty(true))
      ; catch(daemon:client_is_tty, _, fail)
      )
    -> asserta(config:output_tty_cached(true))
    ;  asserta(config:output_tty_cached(false)),
       fail
    )
  ).


%! config:powerline_bubbles is semidet.
%
% Succeeds when Powerline private-use bubble caps (U+E0B6/U+E0B4)
% are appropriate for the output terminal. Fails on the Linux console
% (TERM=linux), where angle-bracket bubbles are used instead.

:- dynamic config:powerline_bubbles_cached/1.

config:powerline_bubbles :-
  ( config:powerline_bubbles_cached(Val)
  -> Val == true
  ; ( config:powerline_bubbles_env_ok
    -> asserta(config:powerline_bubbles_cached(true))
    ;  asserta(config:powerline_bubbles_cached(false)),
       fail
    )
  ).


%! config:terminal_env_term(-Term) is semidet.
%
% TERM from the client-forwarded environment (daemon/IPC) or the
% local process environment.

config:terminal_env_term(Term) :-
  ( catch(daemon:client_env('TERM', Term), _, fail)
  -> true
  ;  catch(system:getenv('TERM', Term), _, fail)
  ).


config:powerline_bubbles_env_ok :-
  ( catch(config:terminal_env_term(Term), _, fail)
  -> Term \== linux
  ;  true
  ).


% Interface can dynamically set the printing style

:- dynamic config:interface_printing_style/1.


% The default printing style

config:default_printing_style('fancy').


%! confg:printing_style(?Setting)
%
% Retrieves the printing style ('short', 'column' or 'fancy')
%
% 1. Running as a server, use client style

config:printing_style(Style) :-
  pengine_self(M),
  !,
  M:printing_style(Style).

% 2. Not running as a server, use interface style when specified,
%    otherwise default

config:printing_style(Style) :-
  config:interface_printing_style(Style),!.

config:printing_style(Style) :-
  \+(config:interface_printing_style(_)),
  config:default_printing_style(Style).


%! config:printing_tty_size(?H,?W)
%
% Retrieves the tty_size to use for printing, applying client-server
% "context routing": in pengines (server) mode the connected client's
% terminal is used, in ipc mode the daemon uses the size forwarded by the
% ipc client, and otherwise the local terminal is used.
%
% 1. Running as a server, use client tty_size

config:printing_tty_size(H,W) :-
  pengine_self(M),
  !,
  M:printing_tty_size(H,W).

% 1b. Running as daemon, use client-provided tty_size

config:printing_tty_size(H,W) :-
  catch(daemon:client_tty_size(H,W), _, fail),
  !.

% 2. Otherwise use the local terminal size

config:printing_tty_size(H,W) :-
  config:local_tty_size(H,W).


%! config:local_tty_size(-H,-W) is det.
%
% The local process terminal size, falling back to a sane default when
% stdout is not a tty (tty_size/2 may throw outside a real terminal).
%
% This performs no client-server context routing, so it is safe for the
% lightweight ipc client to call without pulling in library(pengines). It
% also backs the local fallback clause of config:printing_tty_size/2.

config:local_tty_size(H,W) :-
  ( catch(tty_size(H,W), _, fail) -> true ; H = 80, W = 160 ).


%! config:print_expand_use(?Bool)
%
% Defines whether we print information that is normally not printed

config:print_expand_use(false).


%! config:use_expand_hidden(?Prefix) is nondet.
%
% USE_EXPAND prefixes hidden from printer output.

config:use_expand_hidden('abi_mips').
config:use_expand_hidden('abi_ppc').
config:use_expand_hidden('abi_riscv').
config:use_expand_hidden('abi_s390').
config:use_expand_hidden('abi_x86').
config:use_expand_hidden('cpu_flags_arm').
config:use_expand_hidden('cpu_flags_ppc').


%! config:printable_metadata(?List)
%
% Defines which elements in which order to print out when printing ebuild
% information. Use 'blank' to put blank lines in output. Use 'hl' to output
% a horizontal line.

config:printable_metadata([blank,
                           hl, % -------------
                           description,
                           homepage,
                           maintainer,
                           license,
                           eapi,
                           slot,
  		                     subslot,
                           hl, % -------------
                           iuse,
                           required_use,
                           keywords,
                           properties,
                           eclasses,
                           defined_phases,
                           installed,
                           hl, % -------------
                           src_uri,
                           hl, % -------------
                           bdepend,
                           blank,
                           depend,
                           blank,
                           idepend,
                           blank,
                           rdepend,
                           blank,
                           pdepend
                          ]).


% -----------------------------------------------------------------------------
%  Testing
% -----------------------------------------------------------------------------

%! config:test_style(?Style)
%
% Sets the default test style for executing tests. Can be either:
% - single_verbose
% - parallel_verbose
% - parallel_fast

config:test_style(parallel_verbose).


%! config:test_stats_top_n(?N)
%
% How many items to show in "Top cycle mentions" in test statistics output.
%
% Note: must be a non-negative integer.

config:test_stats_top_n(25).


%! config:test_stats_table_width(?Width)
%
% Total table width (characters) including the 2-space left indent.

config:test_stats_table_width(80).


%! config:test_stats_label_col_width(?Width)
%
% Width of the leftmost label/metric column.

config:test_stats_label_col_width(34).


%! config:test_stats_pct_col_width(?Width)
%
% Width of a percentage column.

config:test_stats_pct_col_width(10).


%! config:test_stats_rank_col_width(?Width)
%
% Width of the rank-number column in ranked tables.

config:test_stats_rank_col_width(4).


%! config:test_stats_count_col_width(?Width)
%
% Width of a count column (aligned with pct columns at 10 chars).

config:test_stats_count_col_width(10).


%! config:bugreport_drafts_enabled(?Bool)
%
% Whether to print "Bug report drafts (Gentoo Bugzilla)" in the warnings section
% when domain assumptions are present.
%
config:bugreport_drafts_enabled(true).


%! config:bugreport_drafts_max_assumptions(?N)
%
% Only print bug report drafts when the number of domain assumptions is small,
% to avoid overwhelming output for bulk runs.
%
% Note: must be a non-negative integer.
%
config:bugreport_drafts_max_assumptions(25).


%! config:failsilenton(?Key)
%
% Fails silently (i.e. without messaging failure on the termimal.
% Key can be:
% - version : fails silently when trying to parse versions from git repositories

config:failsilenton(version).


% -----------------------------------------------------------------------------
%  Building
% -----------------------------------------------------------------------------

%! config:build_live_phases(?Phases)
%
% List of ebuild phases that are actually executed during --build.
% Phases not in this list are stubbed (displayed but not run).
% An empty list means fully stubbed (equivalent to old dry_run_build(true)).
% The full sequence [clean,setup,unpack,prepare,configure,compile,test,install,merge]
% means fully live (equivalent to old dry_run_build(false)).
% When --buildpkg is active, the `package` phase appears between
% `install` and `merge` (added dynamically by ebuild_exec:build_phases/1).
%
% Default is fully live: `--build` behaves like `emerge` and actually
% installs each ebuild into the live filesystem and VDB. Without merge,
% downstream packages cannot find their build deps in /usr (e.g.
% apr-util's configure cannot locate apr-1-config). To dry-run a build
% without touching the live FS, use `--pretend` instead.
%
% Note: ebuild_exec:build_phases/1 is responsible for omitting the
% `test` phase based on FEATURES (see config:features_test_enabled).
% Compute_live_prefix stops at the first phase NOT in this list, so
% removing intermediate phases here would also stub install/merge.

config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install, merge]).


%! config:build_serial_retry(?Bool)
%
% When true, a failed parallel-make-sensitive phase (compile, test,
% install) is retried once with MAKEOPTS=-j1 before the build is
% declared failed (portage-ng#25). Environment variables override
% make.conf in Portage's config stack, so the retry forces a serial
% make without touching the user's configuration. Make resumes
% incrementally, so the retry is cheap: it only changes the outcome
% when the original failure was a parallel-make scheduling race
% (e.g. net-analyzer/nsat's link step running before its objects are
% compiled). Deterministic failures fail again immediately.

config:build_serial_retry(true).


%! config:build_transient_retry(?Bool)
%
% When true, a failed phase whose log tail carries the bash PID-reuse
% signature ("wait: pid N is not a child of this shell") is retried
% once before the build is declared failed (portage-ng#76). The
% signature comes from Portage helpers (ecompress, ...) that `wait`
% on a process-substitution pid which bash has lost track of due to
% PID recycling under high fork rates (Gentoo bug #965423, fixed
% upstream in Nov 2025). The failure is environmental and transient,
% not a property of the ebuild, so a single retry of the same phase
% recovers it. Deterministic failures don't match the signature and
% fail immediately, keeping their original semantics.

config:build_transient_retry(true).


%! config:features_test_enabled is semidet.
%
% True iff FEATURES (env > make.conf > fallback) contains 'test' with
% positive sense, applying Portage's incremental left-to-right semantics
% ('-test' removes, 'test' adds, last occurrence wins). Returns false if
% FEATURES is unset or preference:getenv is unavailable.

config:features_test_enabled :-
  catch(
    ( preference:getenv('FEATURES', Atom),
      Atom \== '',
      atomic_list_concat(Tokens, ' ', Atom),
      config:features_resolve_test(Tokens, false, true)
    ),
    _,
    fail
  ).

config:features_resolve_test([], State, State).
config:features_resolve_test([test | Rest], _, Out) :-
  !, config:features_resolve_test(Rest, true, Out).
config:features_resolve_test(['-test' | Rest], _, Out) :-
  !, config:features_resolve_test(Rest, false, Out).
config:features_resolve_test([_ | Rest], State, Out) :-
  config:features_resolve_test(Rest, State, Out).


%! config:dry_run_build(?Bool)
%
% Derived predicate for backward compatibility.
% Succeeds with true when build_live_phases is empty (fully stubbed).

config:dry_run_build(true) :-
  config:build_live_phases([]).


%! config:time_limit_build(?Limit)
%
% When executing a plan (i.e. building) use the specified time limit to
% automatically stop the build process if it takes too long. Note this limit
% applies to an entire step in a plan.
% In seconds.

config:time_limit_build(6000).


%! config:ebuild_command(?Command)
%
% Path or name of the `ebuild` CLI binary (from sys-apps/portage).
% Used by ebuild_exec:run_phases/3 to execute build phases.
% Override in per-machine config files if ebuild lives outside PATH
% (e.g. Gentoo Prefix installations).

:- dynamic config:ebuild_command/1.

config:ebuild_command(ebuild).


%! config:build_root(?Path)
%
% Root directory for build work (PORTAGE_TMPDIR equivalent).
% Portage defaults to /var/tmp/portage; override here or per-machine.

config:build_root('/var/tmp/portage').


%! config:build_log_dir(?Path)
%
% Directory where per-package build logs are stored. Each build action
% produces a log file that captures all phase stdout/stderr output.
% Override per-machine if desired.

config:build_log_dir('/var/tmp/portage-ng/logs').


% -----------------------------------------------------------------------------
%  Large Language Models
% -----------------------------------------------------------------------------

%! config:llm_api_key(?LLM,?Key)
%
% Declares the private API key for each large language model.
%
% The private api_key file is optional. On a fresh checkout (where
% Source/Config/Private/template_api_key.pl has not been copied to
% api_key.pl yet) we fall back to empty keys; llm:check_api_key/2 then
% prints a clear instruction when an LLM service is actually used.

:- if(exists_source(portage('Source/Config/Private/api_key'))).
:- include(portage('Source/Config/Private/api_key')).
:- else.

config:llm_api_key(grok,     '').
config:llm_api_key(chatgpt,  '').
config:llm_api_key(claude,   '').
config:llm_api_key(gemini,   '').
config:llm_api_key(llama,    '').
config:llm_api_key(ollama,   '').

:- endif.


%! config:llm_capability(+Name,-Capability)
%
% Declares prompts to be passed to the LLM

config:llm_capability(context,Capability) :-
  Description="You are interacting with portage-ng, an alternative dependency resolver
               for Gentoo Linux written in SWI-Prolog. portage-ng reads the same Portage
               tree (metadata/md5-cache) and package database (var/db/pkg) as traditional
               Gentoo emerge, but it resolves dependencies using its own Prolog-based
               prover, planner, and scheduler. It is NOT the standard Portage emerge tool.
               When diagnosing issues, keep in mind: (1) the Portage tree, installed
               packages, USE flags, keywords, and profiles are identical to a real Gentoo
               system; (2) dependency resolution errors come from portage-ng's solver, not
               from emerge; (3) domain assumptions indicate deps that portage-ng could not
               satisfy (e.g. missing metadata, USE flag mismatches, keyword restrictions);
               (4) portage-ng uses terms like 'grouped_package_dependency', 'selected_cn',
               'cn_domain', and 'prover:learned' which are internal solver concepts, not
               standard Portage terminology. Do not suggest running emerge commands to fix
               portage-ng issues. Focus on the dependency metadata and constraints instead.",
  normalize_space(string(Capability),Description).

config:llm_capability(chat,Capability) :-
  Description="When formulating a response, you may optionally enclose a message
               (e.g., a question) in <call:chatgpt>, <call:gemini>, <call:ollama>,
               or <call:claude> tags to send it to the respective LLM. The response
               is automatically returned to you, with each LLM maintaining its own
               history of your queries.",
  normalize_space(string(Capability),Description).

config:llm_capability(code,Capability) :-
  Description="When asked to write SWI-Prolog code, you may optionally enclose the
               code in <call:swi_prolog> XML tags. Any code within these tags will
               be executed locally in a temporary module, with the output
               automatically returned to you. Do not mention the XML tags unless
               you include SWI-Prolog code between them. Write the code as if it
               were loaded from a separate source file, including triggering
               execution of your main function using a :- directive, such as
               :- main. The temporary module is destroyed after execution.",
  normalize_space(string(Capability),Description).


%! config:llm_support(-Prompt)
%
% Declares prompts to be passed to the LLM in case of merge failure

config:llm_support(Capability) :-
  Description="portage-ng (a Prolog-based alternative dependency resolver for Gentoo)
               failed to resolve the following target against a standard Portage tree.
               This is NOT the traditional emerge tool. The package may be missing from
               the Portage tree, renamed, moved to a different category, or have empty
               metadata cache entries. Please help identify the correct package atom or
               suggest what might be wrong. Give a short answer. Here is what was
               requested: ",
  normalize_space(string(Capability),Description).


%! config:llm_use_tools(?Bool)
%
% Declares whether or not to enable to code execution integration

config:llm_use_tools(true).


%! config:llm_max_tokens(?Max)
%
% Declares the maximum tokens returned by an LLM

config:llm_max_tokens(4096).


%! config:llm_temperature(?Temperature)
%
% Declares how creative or predictable the LLM should be

config:llm_temperature(0.7).


%! config:llm_sandboxed_execution(?Bool)
%
% Declares whether to turn on or turn off the LLM code execution sandbox

config:llm_sandboxed_execution(true).


%! config:llm_default(?Service)
%
% Declares the default LLM service used by explainer:explain/2.

config:llm_default(claude).


%! config:llm_model(?LLM,?Key)
%
% Declares which version of each large language model to use.

config:llm_model(grok,       'grok-4-1-fast-reasoning').
config:llm_model(chatgpt,    'gpt-4o').
config:llm_model(claude,     'claude-sonnet-4-6').
config:llm_model(gemini ,    'gemini-3-pro-preview').
config:llm_model(llama ,     'Llama-4-Maverick-17B-128E-Instruct-FP8').
config:llm_model(ollama ,    'llama3.2').


%! config:llm_endpoint(?LLM,?Endpoint)
%
% Declares the https endpoint url for each large language model.

config:llm_endpoint(grok,    'https://api.x.ai/v1/chat/completions').
config:llm_endpoint(chatgpt, 'https://api.openai.com/v1/chat/completions').
config:llm_endpoint(claude,  'https://api.anthropic.com/v1/messages').
config:llm_endpoint(gemini,  'https://generativelanguage.googleapis.com/v1beta/chat/completions').
config:llm_endpoint(llama,   'https://api.llama.com/v1/chat/completions').
config:llm_endpoint(ollama,  'http://localhost:11434/v1/chat/completions').


%! config:llm_local(?LLM)
%
% Declares which large language models run locally and therefore need no API
% key. The chat driver (llm:check_api_key/2) skips the key requirement for
% these services.

config:llm_local(ollama).


% -----------------------------------------------------------------------------
%  Semantic search (natural-language package search via embeddings)
% -----------------------------------------------------------------------------

%! config:semantic_search_enabled(?Bool) is det.
%
% Enable or disable natural-language semantic search. When false,
% --search falls back to structured queries only and --train-model
% is a no-op.

config:semantic_search_enabled(true).


%! config:semantic_model(?Model) is det.
%
% The Ollama model used for generating embeddings.

config:semantic_model('nomic-embed-text').


%! config:semantic_endpoint(?Endpoint) is det.
%
% The Ollama embeddings API endpoint.

config:semantic_endpoint('http://localhost:11434/api/embed').


%! config:semantic_batch_size(?Size) is det.
%
% Number of texts embedded per HTTP request when building the semantic
% index. Ollama's /api/embed accepts an array of inputs, so batching
% avoids one round-trip per package.

config:semantic_batch_size(64).


%! config:semantic_top_n(?N) is det.
%
% Number of results returned by a semantic search query.

config:semantic_top_n(10).


% -----------------------------------------------------------------------------
%  Build time estimation
% -----------------------------------------------------------------------------

%! config:emerge_log_path(?Path) is semidet.
%
% Path to emerge.log for reading historical build durations.
% Comment out or remove to disable emerge.log parsing.

config:emerge_log_path('/var/log/emerge.log').


%! config:buildtime_enabled(?Bool) is det.
%
% Enable or disable build time estimation in plan output.

config:buildtime_enabled(true).


% -----------------------------------------------------------------------------
%  Daemon (ultralight mode)
% -----------------------------------------------------------------------------

%! config:daemon_runtime_dir(-Dir) is det.
%
% Directory holding the daemon socket and PID file. Uses XDG_RUNTIME_DIR
% if available, otherwise a per-user directory under /tmp. Pure accessor:
% creation, ownership verification, and permission enforcement (0700) are
% performed by daemon:ensure_runtime_dir/1 at daemon startup.

config:daemon_runtime_dir(Dir) :-
  ( getenv('XDG_RUNTIME_DIR', Dir)
  -> true
  ;  getenv('USER', User),
     atomic_list_concat(['/tmp/portage-ng-', User], Dir)
  ).


%! config:daemon_socket_path(-Path) is det.
%
% Path to the Unix domain socket used by the ultralight daemon.

config:daemon_socket_path(Path) :-
  config:daemon_runtime_dir(Dir),
  atomic_list_concat([Dir, '/portage-ng.sock'], Path).


%! config:daemon_pid_path(-Path) is det.
%
% Path to the PID file co-located with the daemon socket.

config:daemon_pid_path(Path) :-
  config:daemon_runtime_dir(Dir),
  atomic_list_concat([Dir, '/portage-ng.pid'], Path).


%! config:daemon_inactivity_timeout(?Seconds) is det.
%
% Seconds of inactivity after which the daemon auto-shuts down.
% 0 means never auto-shutdown.

config:daemon_inactivity_timeout(1800).


%! config:daemon_autostart(?Bool) is det.
%
% When true (default), --mode ipc auto-starts a background daemon if
% none is running. When false, prints an error instead.

config:daemon_autostart(true).