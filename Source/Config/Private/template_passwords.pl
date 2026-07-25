% =================================================================================
% Rename this file to passwords.pl and set your passwords for the following below.
% =================================================================================
%
% HTTP digest: set the same plaintext here that you used for
%   DIGEST_PASSWORD='...' make passwordfile
% The server reads the hashed Certificates/passwordfile; clients/workers
% send this plaintext via config:digest_password/2.


%! config:certificate_password(?Key,?Pass)
%
% Declares the password for the SSL client/server certificates

config:certificate_password(server,'').
config:certificate_password(client,'').


%! config:digest_password(?User,?Pass)
%
% Declares the password for digest user authentication

config:digest_password('portage-ng','').


%! config:digest_realm(?Realm)
%
% Declares the realm for digest user authentication

config:digest_realm('portage-ng').
