#!/bin/bash
# Copies the executor modules the probe type-checks into src/, verbatim from
# rust-executor/src at commit 425311398 (origin/dev on 2026-11-19).
# src/lib.rs (the stubs) is committed; the copied files are in .gitignore.
# Env: AD4M_REPO = an ad4m git checkout that has the commit (default: this repo).
set -euo pipefail
D=$(cd "$(dirname "$0")" && pwd)
REPO=${AD4M_REPO:-$(git -C "$D" rev-parse --show-toplevel)}
REV=425311398
FILES=(
  agent/capabilities/types.rs
  languages/error.rs
  languages/literal.rs
  perspectives/hardwired_sdna/auto_processor.json
  perspectives/hardwired_sdna/container.json
  perspectives/hardwired_sdna/flow_instance.json
  perspectives/hardwired_sdna/flow_transition_proposal.json
  perspectives/hardwired_sdna/interpretation_overlay.json
  perspectives/hardwired_sdna/interpretation_run.json
  perspectives/hardwired_sdna/mentioning.json
  perspectives/hardwired_sdna/text_content.json
  perspectives/model_query/abstract_classes.rs
  perspectives/model_query/abstract_classes_tests.rs
  perspectives/model_query/builtin_classes.rs
  perspectives/model_query/collection_provenance_tests.rs
  perspectives/model_query/containment.rs
  perspectives/model_query/eval_transform.rs
  perspectives/model_query/filtering.rs
  perspectives/model_query/getters.rs
  perspectives/model_query/hydration.rs
  perspectives/model_query/integration_tests.rs
  perspectives/model_query/link_author.rs
  perspectives/model_query/link_author_tests/flow_rules.rs
  perspectives/model_query/link_author_tests/mod.rs
  perspectives/model_query/link_author_tests/nested.rs
  perspectives/model_query/link_author_tests/refusals.rs
  perspectives/model_query/link_author_tests/side_by_side.rs
  perspectives/model_query/link_status_relation_tests.rs
  perspectives/model_query/link_status_tests.rs
  perspectives/model_query/links.rs
  perspectives/model_query/mod.rs
  perspectives/model_query/page_before_resolve_tests.rs
  perspectives/model_query/projection.rs
  perspectives/model_query/proof_valid_tests.rs
  perspectives/model_query/query.rs
  perspectives/model_query/relations.rs
  perspectives/model_query/resolve_selection_tests.rs
  perspectives/model_query/round_trip_tests.rs
  perspectives/model_query/selection_guard_tests/link_status.rs
  perspectives/model_query/selection_guard_tests/mod.rs
  perspectives/model_query/selection_guard_tests/walks.rs
  perspectives/model_query/selection_guard_tests/where_and_count.rs
  perspectives/model_query/shape.rs
  perspectives/model_query/sparql_builder.rs
  perspectives/model_query/test_helpers.rs
  perspectives/model_query/timestamp_probe_tests.rs
  perspectives/model_query/types.rs
  perspectives/model_query/union.rs
  perspectives/model_query/union_tests.rs
  perspectives/model_query/utils.rs
  perspectives/ordering/linked_list.rs
  perspectives/ordering/mod.rs
  perspectives/ordering/tests.rs
  perspectives/ordering/types.rs
  perspectives/shacl_parser.rs
  perspectives/shacl_parser/role_gate_keys.rs
  perspectives/sparql_store.rs
  perspectives/subject_classes_of.rs
  perspectives/write_log.rs
  secret.rs
  types/core.rs
  types/domain.rs
  types/mod.rs
)
for f in "${FILES[@]}"; do
  mkdir -p "$D/src/$(dirname "$f")"
  git -C "$REPO" show "$REV:rust-executor/src/$f" > "$D/src/$f"
done
echo "copied ${#FILES[@]} files from rust-executor/src@$REV into $D/src"
