% =================================================================================
% Rename this file to passwords.pl and set your passwords for the following below.
% =================================================================================
%
% HTTP digest: set config:digest_password/2 below, then run
%   make passwordfile
% to derive Certificates/passwordfile for the server. Clients/workers
% use this plaintext file directly via config:digest_password/2.


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
