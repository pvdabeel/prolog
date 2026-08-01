# Certificates

TLS certificates, keys, and the HTTP digest password file for
portage-ng client/server mode.

## Generating certificates

Use the Makefile target from the project root:

```bash
make certs HOST=mac-pro.local
```

This runs `Certificates/Scripts/generate.sh`, which creates:

- A local CA (`cacert.pem` + `cakey.pem`)
- Per-host client and server certificate/key pairs

## HTTP digest password file

`passwordfile` is **not** tracked in git (a previous well-known demo
password was rotated out). The plaintext lives in
`Source/Config/Private/passwords.pl` (copy from the template); the
hashed server file is derived from that:

```bash
cp Source/Config/Private/template_passwords.pl \
   Source/Config/Private/passwords.pl
# edit config:digest_password/2 to a strong secret
make passwordfile
```

Copy the same `passwords.pl` to every client/worker. The server reads
the hashed `Certificates/passwordfile`; clients send the plaintext via
`config:digest_password/2`.

## Checking and renewing certificates

```bash
make certs-check    # show expiry status for all hosts
make certs-renew    # renew any certs expiring within 30 days
```

Or use the script directly for more control:

```bash
sh Certificates/Scripts/generate.sh --check                # check all hosts
sh Certificates/Scripts/generate.sh --check mac-pro.local  # check one host
sh Certificates/Scripts/generate.sh --renew                # renew expired (all hosts)
sh Certificates/Scripts/generate.sh --renew mac-pro.local  # renew expired (one host)
sh Certificates/Scripts/generate.sh --renew-all            # renew all unconditionally
```

## File layout

| File | Tracked | Description |
|------|---------|-------------|
| `cacert.pem` | Yes | CA certificate (shared across hosts) |
| `cakey.pem` | No | CA private key |
| `cacert.srl` | No | CA serial number file |
| `passwordfile` | No | HTTP digest authentication password file |
| `<host>.server-cert.pem` | Yes | Server certificate for `<host>` |
| `<host>.server-key.pem` | No | Server private key for `<host>` |
| `<host>.client-cert.pem` | Yes | Client certificate for `<host>` |
| `<host>.client-key.pem` | No | Client private key for `<host>` |
| `Scripts/generate.sh` | Yes | Certificate generation script |
| `Scripts/digestpassword.sh` | Yes | Digest passwordfile generator |

Private keys (`*-key.pem`, `cakey.pem`), the serial file (`cacert.srl`),
and `passwordfile` are excluded via `.gitignore`.

## Usage

Certificate paths are resolved at runtime by `config:certificate/2` and
`config:digest_passwordfile/1` in `Source/config.pl`. See
[Documentation/Handbook/18-doc-distributed.md](../Documentation/Handbook/18-doc-distributed.md)
for the full TLS / digest setup guide.
