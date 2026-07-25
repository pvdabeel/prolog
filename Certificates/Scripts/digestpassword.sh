#!/bin/sh
#
# Derive Certificates/passwordfile from Source/Config/Private/passwords.pl.
#
# Single source of truth: set config:digest_password/2 (and optional
# config:digest_realm/1) in Private/passwords.pl, then run:
#
#   make passwordfile
#
# Format written (SWI-Prolog http_digest):
#   User:MD5(User:Realm:Password)

set -eu

SCRIPTDIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
CERTDIR="$(CDPATH= cd -- "${SCRIPTDIR}/.." && pwd)"
ROOTDIR="$(CDPATH= cd -- "${CERTDIR}/.." && pwd)"
PASSWORDS="${ROOTDIR}/Source/Config/Private/passwords.pl"
OUT="${DIGEST_FILE:-${CERTDIR}/passwordfile}"

if [ ! -f "${PASSWORDS}" ]; then
  echo "error: ${PASSWORDS} not found." >&2
  echo "  cp Source/Config/Private/template_passwords.pl \\" >&2
  echo "     Source/Config/Private/passwords.pl" >&2
  echo "  # edit config:digest_password/2, then: make passwordfile" >&2
  exit 1
fi

if ! command -v swipl >/dev/null 2>&1; then
  echo "error: swipl not found in PATH." >&2
  exit 127
fi

# Load Private/passwords.pl (facts are config:…/N), refuse an empty
# password, hash with the same helper the HTTP digest library uses.
TMP="$(mktemp "${TMPDIR:-/tmp}/portage-ng-digest.XXXXXX")"
trap 'rm -f "${TMP}"' EXIT

if ! swipl -q -g "
  use_module(library(http/http_digest)),
  consult('${PASSWORDS}'),
  ( current_predicate(config:digest_password/2),
    config:digest_password(User, Pass),
    atom(User), atom(Pass), Pass \\== ''
  -> true
  ;  format(user_error,
       'error: config:digest_password/2 missing or empty in ~w~n',
       ['${PASSWORDS}']),
     halt(1)
  ),
  ( current_predicate(config:digest_realm/1),
    config:digest_realm(Realm)
  -> true
  ;  Realm = 'portage-ng'
  ),
  http_digest_password_hash(User, Realm, Pass, Hash),
  setup_call_cleanup(
    open('${TMP}', write, S),
    format(S, '~w:~w~n', [User, Hash]),
    close(S)),
  format('user=~w realm=~w~n', [User, Realm]),
  halt.
" -t halt 2>"${TMP}.err"
then
  cat "${TMP}.err" >&2 || true
  rm -f "${TMP}.err"
  exit 1
fi
rm -f "${TMP}.err"

umask 077
mv "${TMP}" "${OUT}"
chmod 600 "${OUT}" || true
trap - EXIT

echo "Wrote ${OUT} from ${PASSWORDS}."
