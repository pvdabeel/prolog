#!/bin/sh
#
# Generate Certificates/passwordfile for HTTP digest authentication.
#
# The file is intentionally NOT committed: the previous well-known demo
# password ("portage-ng") was rotated out. Clients must use the same
# plaintext via Source/Config/Private/passwords.pl
# (config:digest_password/2).
#
# Usage (from project root or Certificates/):
#   DIGEST_PASSWORD='...' make passwordfile
#   DIGEST_PASSWORD='...' sh Certificates/Scripts/digestpassword.sh
#
# Optional overrides:
#   DIGEST_USER   (default: portage-ng)
#   DIGEST_REALM  (default: portage-ng)
#   DIGEST_FILE   (default: <Certificates>/passwordfile)
#
# Format written (SWI-Prolog http_digest):
#   User:MD5(User:Realm:Password)

set -eu

SCRIPTDIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
CERTDIR="$(CDPATH= cd -- "${SCRIPTDIR}/.." && pwd)"

USER="${DIGEST_USER:-portage-ng}"
REALM="${DIGEST_REALM:-portage-ng}"
OUT="${DIGEST_FILE:-${CERTDIR}/passwordfile}"

if [ -z "${DIGEST_PASSWORD:-}" ]; then
  if [ -t 0 ]; then
    printf 'Digest password for user %s (realm %s): ' "${USER}" "${REALM}" >&2
    # shellcheck disable=SC2039
    stty -echo 2>/dev/null || true
    # shellcheck disable=SC2162
    read DIGEST_PASSWORD
    stty echo 2>/dev/null || true
    printf '\n' >&2
  else
    echo "error: DIGEST_PASSWORD is unset (non-interactive)." >&2
    echo "  Set DIGEST_PASSWORD and re-run, e.g.:" >&2
    echo "    DIGEST_PASSWORD='...' make passwordfile" >&2
    exit 1
  fi
fi

if [ -z "${DIGEST_PASSWORD}" ]; then
  echo "error: empty DIGEST_PASSWORD rejected." >&2
  exit 1
fi

HASH="$(
  printf '%s' "${USER}:${REALM}:${DIGEST_PASSWORD}" \
    | openssl md5 2>/dev/null \
    | awk '{print $NF}'
)"

if [ -z "${HASH}" ] || [ "${#HASH}" -ne 32 ]; then
  echo "error: failed to compute MD5 digest hash (openssl md5)." >&2
  exit 1
fi

umask 077
printf '%s:%s\n' "${USER}" "${HASH}" > "${OUT}"
chmod 600 "${OUT}" || true

echo "Wrote ${OUT} (user=${USER}, realm=${REALM})."
echo "Mirror the plaintext in Source/Config/Private/passwords.pl as:"
echo "  config:digest_password('${USER}', '<same password>')."
echo "  config:digest_realm('${REALM}')."
