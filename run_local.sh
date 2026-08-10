#!/usr/bin/env bash
# run_local.sh — run pmart standalone locally with MAP_ACTIVE=TRUE
#
# Usage:
#   ./run_local.sh                        # plain local run (no pre-loaded data)
#   ./run_local.sh --single UUID          # preload one MAP object UUID (any supported omics)
#   ./run_local.sh --dual-lipids "UUID1&UUID2" # preload two lipid UUIDs (lipid-only dual mode)
#   ./run_local.sh --list                 # list all objects in minio (UUID, name, type)
#
# Typical 2-lipid testing workflow:
#   1. Prepare test lipid files from example_data/:
#      - example_data/test_lipid_pos_edata.csv
#      - example_data/test_lipid_neg_edata.csv
#      - example_data/test_lipid_fdata.csv
#      (optional metadata: example_data/test_pep_emeta.csv)
#   2. Upload the datasets to minio via MAP (or directly via mapDataAccess).
#      NOTE: --single/--dual-lipids only work with objects that already exist in minio.
#   3. Run --list to see all stored objects and copy the two lipidomics UUIDs
#   4. Run --dual-lipids "UUID1&UUID2" to launch pmart with both datasets pre-loaded
#
# Prerequisites (checked automatically on every run):
#   1. minio on localhost:9000
#      - Checked via TCP connect. If unreachable, the script looks for map-app/docker-compose.yml
#        one directory up and runs 'docker compose up -d minio', then waits up to 20 s.
#        Fails with instructions if map-app is not found or minio does not start.
#   2. mapDataAccess R package
#      - Checked via requireNamespace(). If missing, installed automatically from the pinned
#        GitLab commit used in the Dockerfile. Requires GITLAB_PAT if the repo is private.
#   3. Python venv with minio-py (path read from cfg/minio_config_local.yml)
#      - Checked by importing minio inside the venv. If missing or broken, a new venv is
#        created at the configured path using the system python3 and requirements.txt is
#        installed. The python_venv path in minio_config_local.yml is updated automatically.
#
# Override the minio config path:
#   MAP_CONFIG=/path/to/config.yml ./run_local.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

RED='\033[0;31m'; GRN='\033[0;32m'; YLW='\033[1;33m'; NC='\033[0m'
ok()   { echo -e "${GRN}[OK]${NC}  $*"; }
warn() { echo -e "${YLW}[WARN]${NC} $*"; }
fail() { echo -e "${RED}[ERR]${NC} $*" >&2; exit 1; }
info() { echo -e "      $*"; }

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
PRELOAD_UUIDS=""
PRELOAD_MODE="none"
LIST_MODE=false
while [[ $# -gt 0 ]]; do
  case "$1" in
    --single)
      PRELOAD_UUIDS="${2:-}"
      PRELOAD_MODE="single"
      shift 2
      ;;
    --dual-lipids)
      PRELOAD_UUIDS="${2:-}"
      PRELOAD_MODE="dual-lipids"
      shift 2
      ;;
    --list)
      LIST_MODE=true
      shift
      ;;
    *)
      echo "Unknown argument: $1" >&2
      echo "Usage: $0 [--single UUID | --dual-lipids UUID1&UUID2] [--list]" >&2
      exit 1
      ;;
  esac
done

if [[ "$PRELOAD_MODE" == "single" && "$PRELOAD_UUIDS" == *"&"* ]]; then
  fail "--single expects one UUID. Use --dual-lipids \"UUID1&UUID2\" for two lipid UUIDs."
fi

if [[ "$PRELOAD_MODE" == "dual-lipids" ]]; then
  IFS='&' read -r uuid1 uuid2 uuid_extra <<< "$PRELOAD_UUIDS"
  if [[ -z "${uuid1:-}" || -z "${uuid2:-}" || -n "${uuid_extra:-}" ]]; then
    fail "--dual-lipids expects exactly two UUIDs joined by &: UUID1&UUID2"
  fi
fi

# ---------------------------------------------------------------------------
# Config path
# ---------------------------------------------------------------------------
export MAP_CONFIG="${MAP_CONFIG:-${SCRIPT_DIR}/cfg/minio_config_local.yml}"
[[ -f "$MAP_CONFIG" ]] || fail "Minio config not found: $MAP_CONFIG"

# Read python_venv from the yml (simple grep — no yq required)
PYTHON_VENV="$(grep -E '^python_venv:' "$MAP_CONFIG" | awk '{print $2}' | tr -d '"' | tr -d "'")"
PYTHON_VENV="${PYTHON_VENV:-${SCRIPT_DIR}/venv}"

# Resolve relative paths from the repo root, and avoid unusable root-level defaults.
if [[ "$PYTHON_VENV" != /* ]]; then
  PYTHON_VENV="${SCRIPT_DIR}/${PYTHON_VENV#./}"
fi
if [[ "$PYTHON_VENV" == "/" || "$PYTHON_VENV" == "/venv" ]]; then
  warn "python_venv in $MAP_CONFIG points to an unsafe path ($PYTHON_VENV); using ${SCRIPT_DIR}/venv instead."
  PYTHON_VENV="${SCRIPT_DIR}/venv"
fi

# ---------------------------------------------------------------------------
# Prerequisite 1: minio reachable on localhost:9000
# ---------------------------------------------------------------------------
echo ""
echo "--- Checking prerequisites ---"

check_minio() {
  # Try a plain TCP connect; nc or curl both work
  if command -v nc &>/dev/null; then
    nc -z localhost 9000 &>/dev/null
  else
    curl -sf --max-time 2 http://localhost:9000/minio/health/live &>/dev/null
  fi
}

if check_minio; then
  ok "minio is reachable on localhost:9000"
else
  warn "minio not reachable on localhost:9000 — attempting to start it..."

  # Look for map-app docker-compose one directory up, then two directories up
  MAP_APP_DIR=""
  for candidate in "${SCRIPT_DIR}/../map-app" "${SCRIPT_DIR}/.."; do
    if [[ -f "${candidate}/docker-compose.yml" ]]; then
      MAP_APP_DIR="$(cd "$candidate" && pwd)"
      break
    fi
  done

  if [[ -n "$MAP_APP_DIR" ]]; then
    info "Found docker-compose at: $MAP_APP_DIR"
    docker compose -f "${MAP_APP_DIR}/docker-compose.yml" up -d minio \
      || fail "Failed to start minio via docker compose."

    info "Waiting up to 20 s for minio..."
    for i in $(seq 1 20); do
      sleep 1
      if check_minio; then
        ok "minio started successfully."
        break
      fi
      if [[ $i -eq 20 ]]; then
        fail "minio did not become ready in 20 s. Start it manually:\n  docker compose -f ${MAP_APP_DIR}/docker-compose.yml up -d minio"
      fi
    done
  else
    fail "Could not find map-app docker-compose.yml. Start minio manually:\n  docker compose up -d minio"
  fi
fi

# ---------------------------------------------------------------------------
# Prerequisite 2: mapDataAccess R package
# ---------------------------------------------------------------------------
MAPDATAACCESS_COMMIT="gitlab@code.emsl.pnl.gov::multiomics-analyses/mapdataaccess-lib@7225058a1563944d2f20b10655cb2b92ae23ed71"

if Rscript -e "if (!requireNamespace('mapDataAccess', quietly=TRUE)) quit(status=1)" &>/dev/null; then
  ok "mapDataAccess is installed"
else
  warn "mapDataAccess not found — installing via renv..."
  Rscript -e "renv::install('${MAPDATAACCESS_COMMIT}')" \
    || fail "Failed to install mapDataAccess. Check your GITLAB_PAT or network access."
  ok "mapDataAccess installed"
fi

# ---------------------------------------------------------------------------
# Prerequisite 3: Python venv with minio-py
# ---------------------------------------------------------------------------
PYTHON_BIN="${PYTHON_VENV}/bin/python"

if [[ -x "$PYTHON_BIN" ]] && "$PYTHON_BIN" -c "import minio, numpy" &>/dev/null; then
  ok "Python venv OK: $PYTHON_VENV"
else
  warn "Python venv not found or missing required packages (minio, numpy) at: $PYTHON_VENV"
  info "Creating venv and installing requirements..."

  # Find a system python3
  SYSTEM_PYTHON="$(command -v python3 || command -v python || true)"
  [[ -n "$SYSTEM_PYTHON" ]] || fail "No python3 found on PATH. Install Python 3 first."

  # Prefer stdlib venv to avoid global pip installs (e.g., PEP 668 environments on macOS/Homebrew).
  "$SYSTEM_PYTHON" -m venv "$PYTHON_VENV" \
    || fail "Failed to create venv at $PYTHON_VENV."
  "${PYTHON_VENV}/bin/pip" install --quiet "minio==7.0.2" "numpy" \
    || fail "Failed to install required Python packages into venv (minio, numpy)."

  ok "Python venv created: $PYTHON_VENV"

  # Update minio_config_local.yml to point to the newly created venv
  if ! grep -q "^python_venv:" "$MAP_CONFIG"; then
    echo "python_venv: ${PYTHON_VENV}" >> "$MAP_CONFIG"
  else
    if sed --version >/dev/null 2>&1; then
      sed -i "s|^python_venv:.*|python_venv: ${PYTHON_VENV}|" "$MAP_CONFIG"
    else
      sed -i '' "s|^python_venv:.*|python_venv: ${PYTHON_VENV}|" "$MAP_CONFIG"
    fi
  fi
  info "Updated python_venv in $MAP_CONFIG"
fi

# ---------------------------------------------------------------------------
# --list: print all minio objects and exit (no app launch)
# ---------------------------------------------------------------------------
if $LIST_MODE; then
  ok "Listing all objects in minio..."
  echo ""
  Rscript --vanilla -e "
    library(mapDataAccess)
    con <- map_data_connection('${MAP_CONFIG}')
    ids <- get_all_data_ids(con)
    if (length(ids) == 0) { cat('No objects found in minio.\n'); quit(status=0) }
    rows <- lapply(ids, function(id) {
      t <- tryCatch(get_tags(con, id), error = function(e) list())
      list(
        uuid    = id,
        name    = if (!is.null(t\$ProjectName)) t\$ProjectName else '',
        object  = if (!is.null(t\$ObjectType))  t\$ObjectType  else '',
        dtype   = if (!is.null(t\$DataType))    paste(t\$DataType, collapse=', ') else ''
      )
    })
    # Column widths based on content (UUID is always 36 chars)
    w_name   <- max(nchar('ProjectName'),  sapply(rows, function(r) nchar(r\$name)))
    w_object <- max(nchar('ObjectType'),   sapply(rows, function(r) nchar(r\$object)))
    w_dtype  <- max(nchar('DataType'),     sapply(rows, function(r) nchar(r\$dtype)))
    fmt <- sprintf('%%-%ds  %%-%ds  %%-%ds  %%s\n', w_name, w_object, w_dtype)
    cat(sprintf(fmt, 'ProjectName', 'ObjectType', 'DataType', 'UUID'))
    cat(strrep('-', w_name + w_object + w_dtype + 38 + 6), '\n')
    for (r in rows) cat(sprintf(fmt, r\$name, r\$object, r\$dtype, r\$uuid))
  "
  exit 0
fi

# ---------------------------------------------------------------------------
# Set environment variables
# ---------------------------------------------------------------------------
echo ""
echo "--- Starting pmart ---"

export MAP_VERSION=1
# Keep username empty by default for local runs so MinIO lookups use root object keys.
# Set SHINYPROXY_USERNAME explicitly if you need user-scoped keys (e.g., map/<username>/<uuid>).
export SHINYPROXY_USERNAME="${SHINYPROXY_USERNAME:-}"
export MAP_URL="${MAP_URL:-http://localhost:8300/}"
DEFAULT_PORT=2800
APP_PORT="${PMART_PORT:-${DEFAULT_PORT}}"

is_port_in_use() {
  local port="$1"
  lsof -nP -iTCP:"${port}" -sTCP:LISTEN >/dev/null 2>&1
}

if is_port_in_use "$APP_PORT"; then
  if [[ -n "${PMART_PORT:-}" ]]; then
    fail "Port ${APP_PORT} is already in use. Choose another PMART_PORT value."
  fi

  warn "Port ${DEFAULT_PORT} is in use; searching for the next free port..."
  FOUND_ALT_PORT=false
  for candidate_port in $(seq $((DEFAULT_PORT + 1)) $((DEFAULT_PORT + 50))); do
    if ! is_port_in_use "$candidate_port"; then
      APP_PORT="$candidate_port"
      FOUND_ALT_PORT=true
      break
    fi
  done

  if ! $FOUND_ALT_PORT; then
    fail "No free port found between $((DEFAULT_PORT + 1)) and $((DEFAULT_PORT + 50)). Set PMART_PORT manually."
  fi

  ok "Using available port: ${APP_PORT}"
fi

if [[ -n "$PRELOAD_UUIDS" ]]; then
  export SHINYTEST_LOAD_MAP_OBJECT="$PRELOAD_UUIDS"
  if [[ "$PRELOAD_MODE" == "dual-lipids" ]]; then
    info "Pre-loading dual lipid datasets: $PRELOAD_UUIDS"
  else
    info "Pre-loading single dataset: $PRELOAD_UUIDS"
  fi
else
  unset SHINYTEST_LOAD_MAP_OBJECT 2>/dev/null || true
fi

info "MAP_ACTIVE : TRUE (MAP_VERSION=${MAP_VERSION})"
info "MAP_CONFIG : ${MAP_CONFIG}"
info "MAP_URL    : ${MAP_URL}"
info "Username   : ${SHINYPROXY_USERNAME:-<none>}"
echo ""
ok "Starting pmart on http://localhost:${APP_PORT} ..."
echo ""

Rscript -e "shiny::runApp('${SCRIPT_DIR}', port = ${APP_PORT}, host = '0.0.0.0', launch.browser = FALSE)"
