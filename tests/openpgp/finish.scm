#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(echo "Killing gpg-agent...")
(call-check `(,(tool 'gpg-connect-agent) --verbose killagent /bye))
